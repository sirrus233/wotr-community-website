module AppServer where

import Api (Api)
import AppConfig (AppM)
import Control.Monad.Logger (MonadLogger, logErrorN, logInfoN)
import Data.IntMap.Strict qualified as Map
import Data.Time (UTCTime (..), toGregorian)
import Data.Time.Clock (getCurrentTime)
import Data.Validation (Validation (..))
import Database
  ( DBAction,
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
    updatePlayerName,
  )
import Database.Esqueleto.Experimental (Entity (..), PersistStoreRead (..), PersistStoreWrite (..))
import Logging ((<>:))
import Servant (NoContent (..), ServerError (errBody), ServerT, err404, err422, throwError, type (:<|>) (..))
import Types.Api
  ( GetLeaderboardResponse (GetLeaderboardResponse),
    GetReportsResponse (GetReportsResponse),
    LeaderboardEntry (..),
    ModifyReportRequest (..),
    ProcessedGameReport,
    RawGameReport (..),
    RenamePlayerRequest (..),
    SubmitGameReportResponse (..),
    fromGameReport,
    fromPlayerStats,
    toGameReport,
  )
import Types.DataField (Match (..), Rating, Side (..), Year)
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

insertReport :: (MonadIO m, MonadLogger m) => UTCTime -> RawGameReport -> DBAction m ReportInsertion
insertReport timestamp rawReport = do
  winner <- insertPlayerIfNotExists rawReport.winner
  loser <- insertPlayerIfNotExists rawReport.loser
  report <- insertGameReport $ toGameReport timestamp (entityKey winner) (entityKey loser) rawReport
  pure (report, winner, loser)

insertReport_ :: (MonadIO m, MonadLogger m) => UTCTime -> RawGameReport -> DBAction m ()
insertReport_ timestamp rawReport = insertReport timestamp rawReport >> pass

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

submitReportHandler :: RawGameReport -> AppM SubmitGameReportResponse
submitReportHandler rawReport = case validateReport rawReport of
  Failure errors -> throwError $ err422 {errBody = show errors}
  Success (RawGameReport {..}) -> runDb $ do
    logInfoN $ "Processing game between " <> winner <> " and " <> loser <> "."
    timestamp <- liftIO getCurrentTime
    (processedReport, winnerRating, loserRating) <- processReport =<< insertReport timestamp rawReport
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
adminRenamePlayerHandler RenamePlayerRequest {pid, newName} = runDb $ updatePlayerName pid newName >> pure NoContent

adminModifyReportHandler :: ModifyReportRequest -> AppM NoContent
adminModifyReportHandler ModifyReportRequest {rid, report} = runDb $ do
  oldReport <- readOrError ("Cannot find report " <>: rid) $ lift . get $ rid
  Entity _ newWinner <- readOrError ("Cannot find player " <>: report.winner) $ getPlayerByName report.winner
  Entity _ newLoser <- readOrError ("Cannot find player " <>: report.loser) $ getPlayerByName report.loser

  pure NoContent

server :: ServerT Api AppM
server =
  submitReportHandler
    :<|> getReportsHandler
    :<|> getLeaderboardHandler
    :<|> adminRenamePlayerHandler
    :<|> adminModifyReportHandler
