module AppServer where

import Api (Api)
import AppConfig (AppM)
import Control.Monad.Logger (MonadLogger, logErrorN, logInfoN)
import Data.IntMap.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (..), toGregorian)
import Data.Time.Clock (getCurrentTime)
import Data.Validation (Validation (..))
import Database
  ( DBAction,
    MaybePlayerStats,
    getAllGameReports,
    getAllStats,
    getGameReports,
    getPlayerStats,
    insertGameReport,
    insertPlayerIfNotExists,
    repsertPlayerStats,
    runDb,
  )
import Database.Esqueleto.Experimental (Entity (..))
import Logging ((<>:))
import Servant (ServerError (errBody), ServerT, err422, err500, throwError, type (:<|>) (..))
import Types.Api
  ( GetLeaderboardResponse (GetLeaderboardResponse),
    GetReportsResponse (GetReportsResponse),
    LeaderboardEntry (..),
    RawGameReport (..),
    SubmitGameReportResponse (..),
    fromGameReport,
    fromPlayerStats,
    toGameReport,
  )
import Types.DataField (Match (..), Rating, Side (..), Year)
import Types.Database (GameReport (..), PlayerId, PlayerStats, PlayerStatsTotal (..), defaultPlayerStatsTotal, defaultPlayerStatsYear, updatedPlayerStatsLose, updatedPlayerStatsWin)
import Validation (validateReport)

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

readStats :: PlayerId -> Year -> MaybePlayerStats -> PlayerStats
readStats pid year (mStatsTotal, mStatsYear) = case (mStatsTotal, mStatsYear) of
  (Nothing, Nothing) -> (defaultPlayerStatsTotal_, defaultPlayerStatsYear_)
  (Nothing, Just statsYear) -> (defaultPlayerStatsTotal_, entityVal statsYear)
  (Just statsTotal, Nothing) -> (entityVal statsTotal, defaultPlayerStatsYear_)
  (Just statsTotal, Just statsYear) -> (entityVal statsTotal, entityVal statsYear)
  where
    defaultPlayerStatsTotal_ = defaultPlayerStatsTotal pid
    defaultPlayerStatsYear_ = defaultPlayerStatsYear pid year

reprocessReports :: AppM ()
reprocessReports = runDb $ do
  reports <- getAllGameReports
  forM_ (reverse reports) $ \(report, winner, loser) -> do
    let r = entityVal report
    let year = (\(y, _, _) -> fromIntegral y) . toGregorian . utctDay $ r.gameReportTimestamp
    let (winnerSide, loserSide) = (r.gameReportSide, other r.gameReportSide)
    let (winnerId, loserId) = (r.gameReportWinnerId, r.gameReportLoserId)

    (winnerStatsTotal, winnerStatsYear) <-
      readStats winnerId year <$> readOrError ("Could not find stats for " <>: winner) (getPlayerStats winnerId year)
    (loserStatsTotal, loserStatsYear) <-
      readStats loserId year <$> readOrError ("Could not find stats for " <>: loser) (getPlayerStats loserId year)

    let (winnerRatingOld, loserRatingOld) = (getRating winnerSide winnerStatsTotal, getRating loserSide loserStatsTotal)
    let adjustment = if r.gameReportMatch == Ranked then ratingAdjustment winnerRatingOld loserRatingOld else 0
    let (winnerRating, loserRating) = (winnerRatingOld + adjustment, loserRatingOld - adjustment)

    repsertPlayerStats $ updatedPlayerStatsWin winnerSide winnerRating winnerStatsTotal winnerStatsYear
    repsertPlayerStats $ updatedPlayerStatsLose loserSide loserRating loserStatsTotal loserStatsYear
  where
    other side = case side of Free -> Shadow; Shadow -> Free

    getRating side (PlayerStatsTotal {..}) = case side of
      Free -> playerStatsTotalRatingFree
      Shadow -> playerStatsTotalRatingShadow

normalizeName :: Text -> Text
normalizeName = T.toLower . T.strip

readOrError :: (Monad m, MonadLogger m) => Text -> DBAction m (Maybe a) -> DBAction m a
readOrError errMsg action =
  action >>= \case
    Just a -> pure a
    Nothing -> do
      logErrorN errMsg
      throwError err500 {errBody = show errMsg}

submitReportHandler :: RawGameReport -> AppM SubmitGameReportResponse
submitReportHandler report = case validateReport report of
  Failure errors -> throwError $ err422 {errBody = show errors}
  Success (RawGameReport {..}) -> runDb $ do
    logInfoN $ "Processing game between " <> winner <> " and " <> loser <> "."

    timestamp <- liftIO getCurrentTime
    let year = (\(y, _, _) -> fromIntegral y) . toGregorian . utctDay $ timestamp

    winnerId <- insertPlayerIfNotExists <$> normalizeName <*> id $ winner
    loserId <- insertPlayerIfNotExists <$> normalizeName <*> id $ loser
    _ <- insertGameReport $ toGameReport timestamp winnerId loserId report

    let (winnerSide, loserSide) = (side, other side)

    (winnerStatsTotal, winnerStatsYear) <-
      readStats winnerId year <$> readOrError ("Could not find stats for " <>: winner) (getPlayerStats winnerId year)
    (loserStatsTotal, loserStatsYear) <-
      readStats loserId year <$> readOrError ("Could not find stats for " <>: loser) (getPlayerStats loserId year)

    let (winnerRatingOld, loserRatingOld) = (getRating winnerSide winnerStatsTotal, getRating loserSide loserStatsTotal)
    let adjustment = if match == Ranked then ratingAdjustment winnerRatingOld loserRatingOld else 0
    let (winnerRating, loserRating) = (winnerRatingOld + adjustment, loserRatingOld - adjustment)

    logInfoN $ "Rating diff: " <>: adjustment
    logInfoN $ "Adjustment for " <> winner <> " (" <>: side <> "): " <>: winnerRatingOld <> " -> " <>: winnerRating
    logInfoN $ "Adjustment for " <> loser <> " (" <>: other side <> "): " <>: loserRatingOld <> " -> " <>: loserRating

    repsertPlayerStats $ updatedPlayerStatsWin winnerSide winnerRating winnerStatsTotal winnerStatsYear
    repsertPlayerStats $ updatedPlayerStatsLose loserSide loserRating loserStatsTotal loserStatsYear

    pure SubmitGameReportResponse {report, winnerRating, loserRating}
  where
    other side = case side of Free -> Shadow; Shadow -> Free

    getRating side (PlayerStatsTotal {..}) = case side of
      Free -> playerStatsTotalRatingFree
      Shadow -> playerStatsTotalRatingShadow

getReportsHandler :: AppM GetReportsResponse
getReportsHandler = runDb getGameReports <&> GetReportsResponse . map fromGameReport

getLeaderboardHandler :: Int -> AppM GetLeaderboardResponse
getLeaderboardHandler year =
  runDb (getAllStats year)
    <&> ( GetLeaderboardResponse
            . sortOn (Down . averageRating)
            . map (fromPlayerStats . (\(player, stats) -> (player, readStats (entityKey player) year stats)))
        )

server :: ServerT Api AppM
server = submitReportHandler :<|> getReportsHandler :<|> getLeaderboardHandler
