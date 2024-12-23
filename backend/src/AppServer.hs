module AppServer where

import Api (Api)
import Data.IntMap.Strict qualified as Map
import Data.Time.Clock (getCurrentTime)
import Data.Validation (Validation (..))
import Database (getMostRecentStats, insertGameReport, insertPlayerIfNotExists, insertRatingChange, replacePlayerStats)
import Servant (ServerError (errBody), ServerT, err422, throwError)
import Types.Api (RawGameReport (..), SubmitGameReportResponse (..), toGameReport)
import Types.App (AppM, runDb)
import Types.DataField (Match (..), Rating, Side (..))
import Types.Database (PlayerStats (..), RatingDiff (..), updatePlayerStatsLose, updatePlayerStatsWin)
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

submitReportHandler :: RawGameReport -> AppM SubmitGameReportResponse
submitReportHandler report = case validateReport report of
  Failure errors -> throwError $ err422 {errBody = show errors}
  Success (RawGameReport {..}) -> do
    response <- runDb $ do
      -- TODO Reduce code duplication
      timestamp <- liftIO getCurrentTime
      winnerId <- insertPlayerIfNotExists winner
      loserId <- insertPlayerIfNotExists loser
      reportId <- insertGameReport $ toGameReport timestamp winnerId loserId report
      winnerStats <- getMostRecentStats winnerId -- TODO Wrong, should be for current year specifically
      loserStats <- getMostRecentStats loserId

      let winnerSide = side
      let loserSide = case winnerSide of Free -> Shadow; Shadow -> Free
      let (winnerRatingOld, loserRatingOld) = (getRating winnerSide winnerStats, getRating loserSide loserStats)
      let adjustment = if match == Ranked then ratingAdjustment winnerRatingOld loserRatingOld else 0
      let (winnerRating, loserRating) = (winnerRatingOld + adjustment, loserRatingOld - adjustment)

      insertRatingChange $ RatingDiff timestamp winnerId reportId winnerSide winnerRatingOld winnerRating
      insertRatingChange $ RatingDiff timestamp loserId reportId loserSide loserRatingOld loserRating

      replacePlayerStats . updatePlayerStatsWin winnerSide winnerRating $ winnerStats
      replacePlayerStats . updatePlayerStatsLose loserSide loserRating $ loserStats

      pure $ Right SubmitGameReportResponse {report, winnerRating, loserRating}

    case response of
      Left e -> throwError e
      Right res -> pure res
  where
    getRating side (PlayerStats {..}) = case side of
      Free -> playerStatsCurrentRatingFree
      Shadow -> playerStatsCurrentRatingShadow

server :: ServerT Api AppM
server = submitReportHandler
