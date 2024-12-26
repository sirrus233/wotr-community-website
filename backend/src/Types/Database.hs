{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Database where

import Data.Time (UTCTime (utctDay), Year, toGregorian)
import Data.Time.Clock (getCurrentTime)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Types.DataField (Competition, Expansion, League, Match, PlayerName, Rating, Side (..), Stronghold, Victory)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
   Player
    name PlayerName
    country Text Maybe
    UniquePlayerName name
    deriving Show

   GameReport
    timestamp UTCTime
    winnerId PlayerId
    loserId PlayerId
    side Side
    victory Victory
    match Match
    competition [Competition]
    league League Maybe
    expansions [Expansion]
    treebeard Bool Maybe
    actionTokens Int
    dwarvenRings Int
    turns Int
    corruption Int
    mordor Int Maybe
    initialEyes Int
    aragornTurn Int Maybe
    strongholds [Stronghold]
    interestRating Int
    comments Text Maybe
    deriving Show

   RatingDiff
    timestamp UTCTime
    playerId PlayerId
    reportId GameReportId
    side Side
    ratingBefore Rating
    ratingAfter Rating
    deriving Show

   PlayerStats
    playerId PlayerId
    year Int
    currentRatingFree Rating
    currentRatingShadow Rating
    totalWinsFree Int
    totalWinsShadow Int
    totalLossesFree Int
    totalLossesShadow Int
    yearlyWinsFree Int
    yearlyWinsShadow Int
    yearlyLossesFree Int
    yearlyLossesShadow Int
    Primary playerId year
    deriving Show
|]

currentYear :: (MonadIO m) => m Year
currentYear = (\(year, _, _) -> pure year) . toGregorian . utctDay =<< liftIO getCurrentTime

defaultPlayerStats :: PlayerId -> Year -> PlayerStats
defaultPlayerStats pid year =
  PlayerStats
    { playerStatsPlayerId = pid,
      playerStatsYear = fromIntegral year,
      playerStatsCurrentRatingFree = 500,
      playerStatsCurrentRatingShadow = 500,
      playerStatsTotalWinsFree = 0,
      playerStatsTotalWinsShadow = 0,
      playerStatsTotalLossesFree = 0,
      playerStatsTotalLossesShadow = 0,
      playerStatsYearlyWinsFree = 0,
      playerStatsYearlyWinsShadow = 0,
      playerStatsYearlyLossesFree = 0,
      playerStatsYearlyLossesShadow = 0
    }

rolloverPlayerStats :: PlayerId -> Year -> PlayerStats -> PlayerStats
rolloverPlayerStats pid year (PlayerStats {..}) =
  PlayerStats
    { playerStatsPlayerId = pid,
      playerStatsYear = fromIntegral year,
      playerStatsYearlyWinsFree = 0,
      playerStatsYearlyWinsShadow = 0,
      playerStatsYearlyLossesFree = 0,
      playerStatsYearlyLossesShadow = 0,
      ..
    }

updatePlayerStatsWin :: Side -> Rating -> PlayerStats -> PlayerStats
updatePlayerStatsWin side rating (PlayerStats {..}) = case side of
  Free ->
    PlayerStats
      { playerStatsCurrentRatingFree = rating,
        playerStatsTotalWinsFree = playerStatsTotalWinsFree + 1,
        playerStatsYearlyWinsFree = playerStatsYearlyWinsFree + 1,
        ..
      }
  Shadow ->
    PlayerStats
      { playerStatsCurrentRatingShadow = rating,
        playerStatsTotalWinsShadow = playerStatsTotalWinsShadow + 1,
        playerStatsYearlyWinsShadow = playerStatsYearlyWinsShadow + 1,
        ..
      }

updatePlayerStatsLose :: Side -> Rating -> PlayerStats -> PlayerStats
updatePlayerStatsLose side rating (PlayerStats {..}) = case side of
  Free ->
    PlayerStats
      { playerStatsCurrentRatingFree = rating,
        playerStatsTotalLossesFree = playerStatsTotalLossesFree + 1,
        playerStatsYearlyLossesFree = playerStatsYearlyLossesFree + 1,
        ..
      }
  Shadow ->
    PlayerStats
      { playerStatsCurrentRatingShadow = rating,
        playerStatsTotalLossesShadow = playerStatsTotalLossesShadow + 1,
        playerStatsYearlyLossesShadow = playerStatsYearlyLossesShadow + 1,
        ..
      }
