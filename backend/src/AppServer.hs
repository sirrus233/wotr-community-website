module AppServer where

import Amazonka qualified as AWS
import Amazonka.S3 qualified as S3
import Api (Api)
import AppConfig (AppM, gameLogBucket)
import Control.Monad.Logger (MonadLogger, logErrorN, logInfoN)
import Data.IntMap.Strict qualified as Map
import Data.Time (UTCTime (..), defaultTimeLocale, formatTime, getCurrentTime, toGregorian)
import Data.Validation (Validation (..))
import Database
  ( DBAction,
    deleteGameReport,
    deletePlayer,
    getAllGameReports,
    getAllStats,
    getGameReports,
    getPlayerByName,
    getPlayerStats,
    insertGameReport,
    insertPlayerIfNotExists,
    repsertPlayerStats,
    resetStats,
    runDb,
    updateActiveStatus,
    updatePlayerName,
    updateReports,
  )
import Database.Esqueleto.Experimental (Entity (..), PersistStoreRead (..), PersistStoreWrite (..))
import Logging ((<>:))
import Servant (NoContent (..), ServerError (errBody), ServerT, err404, err422, throwError, type (:<|>) (..))
import Servant.Multipart (FileData (..))
import Types.Api
  ( DeleteReportRequest (..),
    GetLeaderboardResponse (GetLeaderboardResponse),
    GetReportsResponse (GetReportsResponse),
    LeaderboardEntry (..),
    ModifyReportRequest (..),
    ProcessedGameReport,
    RawGameReport (..),
    RemapPlayerRequest (..),
    RemapPlayerResponse (..),
    RenamePlayerRequest (..),
    S3Url,
    SubmitGameReportResponse (..),
    SubmitReportRequest (..),
    fromGameReport,
    fromPlayerStats,
    toGameReport,
  )
import Types.DataField (Match (..), PlayerName, Rating, Side (..), Year)
import Types.Database
  ( GameReport (..),
    MaybePlayerStats,
    Player (..),
    PlayerId,
    PlayerStats,
    PlayerStatsTotal (..),
    ReportInsertion,
    defaultPlayerStatsTotal,
    defaultPlayerStatsYear,
    updatedPlayerStatsLose,
    updatedPlayerStatsWin,
  )
import Validation (validateReport)
import Prelude hiding (get, on)

defaultRating :: Rating
defaultRating = 500

ratingThresholds :: IntMap (Rating, Rating)
ratingThresholds =
  fromList
    [ (10, (16, 16)),
      (33, (15, 17)),
      (56, (14, 18)),
      (79, (13, 19)),
      (102, (12, 20)),
      (126, (11, 21)),
      (151, (10, 22)),
      (178, (9, 23)),
      (207, (8, 24)),
      (236, (7, 25)),
      (270, (6, 26)),
      (308, (5, 27)),
      (352, (4, 28)),
      (409, (3, 29)),
      (499, (2, 30))
    ]

maxThreshold :: (Rating, Rating)
maxThreshold = (1, 31)

ratingAdjustment :: Rating -> Rating -> Rating
ratingAdjustment winner loser
  | winner >= loser = smallAdjust
  | otherwise = bigAdjust
  where
    diff = abs (winner - loser)
    (smallAdjust, bigAdjust) = maybe maxThreshold snd (Map.lookupGE diff ratingThresholds)

yearOf :: UTCTime -> Year
yearOf = (\(y, _, _) -> fromIntegral y) . toGregorian . utctDay

readStats :: PlayerId -> Year -> MaybePlayerStats -> PlayerStats
readStats pid year (mStatsTotal, mStatsYear) = case (mStatsTotal, mStatsYear) of
  (Nothing, Nothing) -> (defaultPlayerStatsTotal_, defaultPlayerStatsYear_)
  (Nothing, Just statsYear) -> (defaultPlayerStatsTotal_, entityVal statsYear)
  (Just statsTotal, Nothing) -> (entityVal statsTotal, defaultPlayerStatsYear_)
  (Just statsTotal, Just statsYear) -> (entityVal statsTotal, entityVal statsYear)
  where
    defaultPlayerStatsTotal_ = defaultPlayerStatsTotal pid
    defaultPlayerStatsYear_ = defaultPlayerStatsYear pid year

readOrError :: (Monad m, MonadLogger m) => Text -> DBAction m (Maybe a) -> DBAction m a
readOrError errMsg action =
  action >>= \case
    Just a -> pure a
    Nothing -> do
      logErrorN errMsg
      throwError err404 {errBody = show errMsg}

toS3Key :: UTCTime -> PlayerName -> PlayerName -> S3.ObjectKey
toS3Key timestamp freePlayer shadowPlayer =
  S3.ObjectKey $ formattedPath <> formattedFilename
  where
    (y, m, d) = toGregorian . utctDay $ timestamp
    formattedPath = show y <> "/" <>: m <> "/" <>: d <> "/"
    formattedTimestamp = toText . formatTime defaultTimeLocale "%Y-%m-%d_%H%M%S" $ timestamp
    formattedFilename = formattedTimestamp <> "_FP_" <> freePlayer <> "_SP_" <> shadowPlayer <> ".log"

toS3Url :: AWS.Env -> S3.ObjectKey -> S3Url
toS3Url (AWS.Env {AWS.region}) (S3.ObjectKey key) =
  "https://" <> S3.fromBucketName gameLogBucket <> ".s3." <> AWS.fromRegion region <> ".amazonaws.com/" <> key

putS3Object :: (MonadIO m) => FilePath -> S3.ObjectKey -> m S3.PutObjectResponse
putS3Object path key = do
  awsLogger <- AWS.newLogger AWS.Debug stdout
  discoveredEnv <- liftIO $ AWS.newEnv AWS.discover
  let env = discoveredEnv {AWS.logger = awsLogger, AWS.region = AWS.Oregon}
  liftIO $ AWS.chunkedFile AWS.defaultChunkSize path >>= AWS.runResourceT . AWS.send env . S3.newPutObject gameLogBucket key

putS3Object_ :: (MonadIO m) => FilePath -> S3.ObjectKey -> m ()
putS3Object_ path key = putS3Object path key >> pass

insertReport :: (MonadIO m, MonadLogger m) => UTCTime -> RawGameReport -> Maybe S3Url -> DBAction m ReportInsertion
insertReport timestamp rawReport s3Url = do
  winner <- insertPlayerIfNotExists rawReport.winner
  loser <- insertPlayerIfNotExists rawReport.loser
  report <- insertGameReport $ toGameReport timestamp (entityKey winner) (entityKey loser) s3Url rawReport
  pure (report, winner, loser)

insertReport_ :: (MonadIO m, MonadLogger m) => UTCTime -> RawGameReport -> Maybe S3Url -> DBAction m ()
insertReport_ timestamp rawReport s3Url = insertReport timestamp rawReport s3Url >> pass

processReport :: (MonadIO m, MonadLogger m) => ReportInsertion -> DBAction m (ProcessedGameReport, Rating, Rating)
processReport (report@(Entity _ GameReport {..}), winnerPlayer@(Entity winnerId winner), loserPlayer@(Entity loserId loser)) = do
  let year = yearOf gameReportTimestamp
  let (winnerSide, loserSide) = (gameReportSide, other gameReportSide)

  (winnerStatsTotal, winnerStatsYear) <-
    readStats winnerId year <$> readOrError ("Missing stats for " <>: winner) (getPlayerStats winnerId year)
  (loserStatsTotal, loserStatsYear) <-
    readStats loserId year <$> readOrError ("Missing stats for " <>: loser) (getPlayerStats loserId year)

  let (winnerRatingOld, loserRatingOld) = (getRating winnerSide winnerStatsTotal, getRating loserSide loserStatsTotal)
  let adjustment = if gameReportMatch == Ranked then ratingAdjustment winnerRatingOld loserRatingOld else 0
  let (winnerRating, loserRating) = (winnerRatingOld + adjustment, loserRatingOld - adjustment)

  logInfoN $ "Rating diff: " <>: adjustment
  logInfoN $ "Adjustment for " <> winner.playerDisplayName <> " (" <>: gameReportSide <> "): " <>: winnerRatingOld <> " -> " <>: winnerRating
  logInfoN $ "Adjustment for " <> loser.playerDisplayName <> " (" <>: other gameReportSide <> "): " <>: loserRatingOld <> " -> " <>: loserRating

  repsertPlayerStats $ updatedPlayerStatsWin winnerSide winnerRating winnerStatsTotal winnerStatsYear
  repsertPlayerStats $ updatedPlayerStatsLose loserSide loserRating loserStatsTotal loserStatsYear

  pure (fromGameReport (report, winnerPlayer, loserPlayer), winnerRating, loserRating)
  where
    other side = case side of Free -> Shadow; Shadow -> Free

    getRating side (PlayerStatsTotal {..}) = case side of
      Free -> playerStatsTotalRatingFree
      Shadow -> playerStatsTotalRatingShadow

reprocessReports :: (MonadIO m, MonadLogger m) => DBAction m ()
reprocessReports = do
  resetStats
  reports <- getAllGameReports
  forM_ (reverse reports) processReport
  updateActiveStatus

submitReportHandler :: SubmitReportRequest -> AppM SubmitGameReportResponse
submitReportHandler (SubmitReportRequest rawReport logFileData) = case validateReport rawReport of
  Failure errors -> throwError $ err422 {errBody = show errors}
  Success (RawGameReport {..}) -> runDb $ do
    logInfoN $ "Processing game between " <> winner <> " and " <> loser <> "."
    timestamp <- liftIO getCurrentTime

    let key = toS3Key timestamp rawReport.winner rawReport.loser
    let s3Path = case logFileData of
          Nothing -> Nothing
          Just _ -> Just $ toS3Url key

    (processedReport, winnerRating, loserRating) <- processReport =<< insertReport timestamp rawReport s3Path

    case fdPayload <$> logFileData of
      Nothing -> pass
      Just path -> putS3Object_ path key

    pure SubmitGameReportResponse {report = processedReport, winnerRating, loserRating}

getReportsHandler :: AppM GetReportsResponse
getReportsHandler = runDb $ getGameReports 500 0 <&> GetReportsResponse . map fromGameReport

getLeaderboardHandler :: Int -> AppM GetLeaderboardResponse
getLeaderboardHandler year =
  runDb (getAllStats year)
    <&> ( GetLeaderboardResponse
            . sortOn (Down . averageRating)
            . map (fromPlayerStats . (\(player, stats) -> (player, readStats (entityKey player) year stats)))
        )

adminRenamePlayerHandler :: RenamePlayerRequest -> AppM NoContent
adminRenamePlayerHandler RenamePlayerRequest {pid, newName} = runDb $ do
  player <- getPlayerByName newName
  case player of
    Nothing -> updatePlayerName pid newName >> pure NoContent
    Just _ -> throwError err422 {errBody = "Name " <>: newName <> " already taken."}

adminRemapPlayerHandler :: RemapPlayerRequest -> AppM RemapPlayerResponse
adminRemapPlayerHandler RemapPlayerRequest {fromPid, toPid} = runDb $ do
  when (fromPid == toPid) (throwError err422 {errBody = "Cannot remap identical player IDs."})

  _ <- readOrError ("Cannot find player " <>: fromPid) $ lift . get $ fromPid
  player <- readOrError ("Cannot find player " <>: toPid) $ lift . get $ toPid

  updateReports fromPid toPid
  deletePlayer fromPid
  reprocessReports

  pure $ RemapPlayerResponse player.playerDisplayName

adminModifyReportHandler :: ModifyReportRequest -> AppM NoContent
adminModifyReportHandler ModifyReportRequest {rid, report} = case validateReport report of
  Failure errors -> throwError $ err422 {errBody = show errors}
  Success _ -> runDb $ do
    oldReport <- readOrError ("Cannot find report " <>: rid) $ lift . get $ rid
    Entity newWinnerId _ <- readOrError ("Cannot find player " <>: report.winner) $ getPlayerByName report.winner
    Entity newLoserId _ <- readOrError ("Cannot find player " <>: report.loser) $ getPlayerByName report.loser

    let newReport = toGameReport oldReport.gameReportTimestamp newWinnerId newLoserId oldReport.gameReportLogFile report
    lift $ replace rid newReport

    when (mustReprocess oldReport newReport) reprocessReports

    pure NoContent
  where
    mustReprocess :: GameReport -> GameReport -> Bool
    mustReprocess old new
      | old.gameReportWinnerId /= new.gameReportWinnerId = True
      | old.gameReportLoserId /= new.gameReportLoserId = True
      | old.gameReportSide /= new.gameReportSide = True
      | old.gameReportMatch /= new.gameReportMatch = True
      | otherwise = False

adminDeleteReportHandler :: DeleteReportRequest -> AppM NoContent
adminDeleteReportHandler DeleteReportRequest {rid} = runDb $ do
  _ <- readOrError ("Cannot find report " <>: rid) $ lift . get $ rid
  deleteGameReport rid
  reprocessReports
  pure NoContent

server :: ServerT Api AppM
server =
  submitReportHandler
    :<|> getReportsHandler
    :<|> getLeaderboardHandler
    :<|> adminRenamePlayerHandler
    :<|> adminRemapPlayerHandler
    :<|> adminModifyReportHandler
    :<|> adminDeleteReportHandler
