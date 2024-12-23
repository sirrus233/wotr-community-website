module AppServer where

import Api (Api)
import Data.IntMap.Strict qualified as Map
import Data.Time.Clock (getCurrentTime)
import Data.Validation (Validation (..))
import Database (getCurrentStats, insertGameReport, insertPlayerIfNotExists, insertRatingChange)
import Servant (ServerError (errBody), ServerT, err422, err500, throwError)
import Types.Api (RawGameReport (..), SubmitGameReportResponse (..), toGameReport)
import Types.App (AppM, runDb)
import Types.DataField (Match (..), Rating, Side (..))
import Types.Database (PlayerStats (..), RatingDiff (..))
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
      timestamp <- liftIO getCurrentTime
      winnerId <- insertPlayerIfNotExists winner
      loserId <- insertPlayerIfNotExists loser
      reportId <- insertGameReport $ toGameReport timestamp winnerId loserId report

      let winnerSide = side
      let loserSide = case winnerSide of
            Free -> Shadow
            Shadow -> Free

      (,) <$> getRating winnerId winnerSide <*> getRating loserId loserSide >>= \case
        (Nothing, _) -> pure $ Left err500 {errBody = "bad winner"}
        (_, Nothing) -> pure $ Left err500 {errBody = "bad loser"}
        (Just winnerRatingOld, Just loserRatingOld) -> do
          let adjustment = if match == Ranked then ratingAdjustment winnerRatingOld loserRatingOld else 0
          let winnerRating = winnerRatingOld + adjustment
          let loserRating = loserRatingOld - adjustment
          insertRatingChange $ RatingDiff timestamp winnerId reportId winnerSide winnerRatingOld winnerRating
          insertRatingChange $ RatingDiff timestamp loserId reportId loserSide loserRatingOld loserRating
          pure $ Right SubmitGameReportResponse {report, winnerRating, loserRating}

    case response of
      Left e -> throwError e
      Right res -> pure res
  where
    getRating pid side =
      getCurrentStats pid <&> fmap (if side == Free then playerStatsCurrentRatingFree else playerStatsCurrentRatingShadow)

server :: ServerT Api AppM
server = submitReportHandler
