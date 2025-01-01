{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Database where

import Data.Time (UTCTime (utctDay), toGregorian)
import Data.Time.Clock (getCurrentTime)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Types.DataField (Competition, Expansion, League, Match, PlayerName, Rating, Side (..), Stronghold, Victory, Year)

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

   PlayerStatsYear
    playerId PlayerId
    year Int
    winsFree Int
    winsShadow Int
    lossesFree Int
    lossesShadow Int
    Primary playerId year
    deriving Show

   PlayerStatsTotal
    playerId PlayerId
    ratingFree Rating
    ratingShadow Rating
    gameCount Int
    Primary playerId
    deriving Show

   PlayerStatsInitial
    playerId PlayerId
    ratingFree Rating
    ratingShadow Rating
    gameCount Int
    Primary playerId
    deriving Show
|]

currentYear :: (MonadIO m) => m Int
currentYear = (\(year, _, _) -> pure $ fromIntegral year) . toGregorian . utctDay =<< liftIO getCurrentTime

defaultPlayerStats :: PlayerId -> Year -> (PlayerStatsTotal, PlayerStatsYear)
defaultPlayerStats pid year =
  ( PlayerStatsTotal
      { playerStatsTotalPlayerId = pid,
        playerStatsTotalRatingFree = 500,
        playerStatsTotalRatingShadow = 500,
        playerStatsTotalGameCount = 0
      },
    PlayerStatsYear
      { playerStatsYearPlayerId = pid,
        playerStatsYearYear = year,
        playerStatsYearWinsFree = 0,
        playerStatsYearWinsShadow = 0,
        playerStatsYearLossesFree = 0,
        playerStatsYearLossesShadow = 0
      }
  )

updatedPlayerStatsWin :: Side -> Rating -> PlayerStatsTotal -> PlayerStatsYear -> (PlayerStatsTotal, PlayerStatsYear)
updatedPlayerStatsWin side rating (PlayerStatsTotal {..}) (PlayerStatsYear {..}) = case side of
  Free ->
    ( PlayerStatsTotal
        { playerStatsTotalRatingFree = rating,
          playerStatsTotalGameCount = playerStatsTotalGameCount + 1,
          ..
        },
      PlayerStatsYear
        { playerStatsYearWinsFree = playerStatsYearWinsFree + 1,
          ..
        }
    )
  Shadow ->
    ( PlayerStatsTotal
        { playerStatsTotalRatingShadow = rating,
          playerStatsTotalGameCount = playerStatsTotalGameCount + 1,
          ..
        },
      PlayerStatsYear
        { playerStatsYearWinsShadow = playerStatsYearWinsShadow + 1,
          ..
        }
    )

updatedPlayerStatsLose :: Side -> Rating -> PlayerStatsTotal -> PlayerStatsYear -> (PlayerStatsTotal, PlayerStatsYear)
updatedPlayerStatsLose side rating (PlayerStatsTotal {..}) (PlayerStatsYear {..}) = case side of
  Free ->
    ( PlayerStatsTotal
        { playerStatsTotalRatingFree = rating,
          playerStatsTotalGameCount = playerStatsTotalGameCount + 1,
          ..
        },
      PlayerStatsYear
        { playerStatsYearLossesFree = playerStatsYearLossesFree + 1,
          ..
        }
    )
  Shadow ->
    ( PlayerStatsTotal
        { playerStatsTotalRatingShadow = rating,
          playerStatsTotalGameCount = playerStatsTotalGameCount + 1,
          ..
        },
      PlayerStatsYear
        { playerStatsYearLossesShadow = playerStatsYearLossesShadow + 1,
          ..
        }
    )
