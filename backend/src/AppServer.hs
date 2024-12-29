module AppServer where

import Api (Api)
import App (AppM, runDb)
import Control.Monad.Logger (logInfoN)
import Data.IntMap.Strict qualified as Map
import Data.Time.Clock (getCurrentTime)
import Data.Validation (Validation (..))
import Database (getGameReports, getStats, insertGameReport, insertPlayerIfNotExists, insertRatingChange, replacePlayerStats)
import Logging ((<>:))
import Servant (ServerError (errBody), ServerT, err422, throwError, type (:<|>) (..))
import Types.Api (GetReportsResponse (GetReportsResponse), RawGameReport (..), SubmitGameReportResponse (..), fromGameReport, toGameReport)
import Types.DataField (Match (..), Rating, Side (..))
import Types.Database (PlayerStats (..), RatingDiff (..), currentYear, updatePlayerStatsLose, updatePlayerStatsWin)
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
  Success (RawGameReport {..}) -> runDb $ do
    logInfoN $ "Processing game between " <> winner <> " and " <> loser <> "."

    timestamp <- liftIO getCurrentTime
    year <- currentYear

    -- TODO Normalize spelling/caps
    winnerId <- insertPlayerIfNotExists winner
    loserId <- insertPlayerIfNotExists loser

    winnerStats <- getStats winnerId year
    loserStats <- getStats loserId year

    reportId <- insertGameReport $ toGameReport timestamp winnerId loserId report

    let (winnerSide, loserSide) = (side, other side)
    let (winnerRatingOld, loserRatingOld) = (getRating winnerSide winnerStats, getRating loserSide loserStats)
    let adjustment = if match == Ranked then ratingAdjustment winnerRatingOld loserRatingOld else 0
    let (winnerRating, loserRating) = (winnerRatingOld + adjustment, loserRatingOld - adjustment)
    logInfoN $ "Rating diff: " <>: adjustment
    logInfoN $ "Adjustment for " <> winner <> " (" <>: side <> "): " <>: winnerRatingOld <> " -> " <>: winnerRating
    logInfoN $ "Adjustment for " <> loser <> " (" <>: other side <> "): " <>: loserRatingOld <> " -> " <>: loserRating

    insertRatingChange $ RatingDiff timestamp winnerId reportId winnerSide winnerRatingOld winnerRating
    insertRatingChange $ RatingDiff timestamp loserId reportId loserSide loserRatingOld loserRating

    replacePlayerStats . updatePlayerStatsWin winnerSide winnerRating $ winnerStats
    replacePlayerStats . updatePlayerStatsLose loserSide loserRating $ loserStats

    pure SubmitGameReportResponse {report, winnerRating, loserRating}
  where
    other side = case side of Free -> Shadow; Shadow -> Free

    getRating side (PlayerStats {..}) = case side of
      Free -> playerStatsCurrentRatingFree
      Shadow -> playerStatsCurrentRatingShadow

getReportsHandler :: AppM GetReportsResponse
getReportsHandler = runDb getGameReports <&> GetReportsResponse . map fromGameReport

server :: ServerT Api AppM
server = submitReportHandler :<|> getReportsHandler
