module AppServer where

import Api (Api)
import Data.IntMap.Strict qualified as Map
import Data.Pool (withResource)
import Data.Time.Clock (getCurrentTime)
import Data.Validation (Validation (..))
import Database (getLatestRating, insertGameReport, insertPlayerIfNotExists, insertRatingChange)
import Database.SQLite.Simple (withTransaction)
import Servant (ServerError (errBody), ServerT, throwError)
import Servant.Server (err422)
import Types.Api (GameReport (..), SubmitGameReportResponse (..))
import Types.App (AppM, Env (..))
import Types.DataField (Rating, Side (..))
import Types.Database (ReadProcessedGameReport (..), WriteProcessedGameReport (..), WriteRatingChange (..))
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

submitReportHandler :: GameReport -> AppM SubmitGameReportResponse
submitReportHandler r = case validateReport r of
  Failure errors -> throwError $ err422 {errBody = show errors}
  Success (GameReport {..}) -> do
    env <- ask
    liftIO . withResource env.dbPool $ \conn -> withTransaction conn $ do
      timestamp <- liftIO getCurrentTime
      winnerId <- insertPlayerIfNotExists conn winner
      loserId <- insertPlayerIfNotExists conn loser
      report <- insertGameReport conn WriteProcessedGameReport {..}

      let winnerSide = report.side
      let loserSide = case winnerSide of
            Free -> Shadow
            Shadow -> Free

      winnerRatingBefore <- getLatestRating conn defaultRating winnerId winnerSide
      loserRatingBefore <- getLatestRating conn defaultRating loserId loserSide
      let adjustment = ratingAdjustment winnerRatingBefore loserRatingBefore

      winnerRating <-
        insertRatingChange
          conn
          WriteRatingChange
            { pid = winnerId,
              side = winnerSide,
              timestamp,
              rid = report.rid,
              ratingBefore = winnerRatingBefore,
              ratingAfter = winnerRatingBefore + adjustment
            }
      loserRating <-
        insertRatingChange
          conn
          WriteRatingChange
            { pid = loserId,
              side = loserSide,
              timestamp,
              rid = report.rid,
              ratingBefore = loserRatingBefore,
              ratingAfter = loserRatingBefore - adjustment
            }

      pure $ SubmitGameReportResponse {..}

server :: ServerT Api AppM
server = submitReportHandler
