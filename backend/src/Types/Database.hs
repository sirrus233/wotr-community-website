{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Database where

import Data.Time (UTCTime)
import Database.Esqueleto.Experimental (Entity)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Types.DataField (Competition, Expansion, League, Match, PlayerName, Rating, Side (..), Stronghold, Victory, Year)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
   Player
    name PlayerName
    displayName PlayerName
    country Text Maybe
    isActive Bool
    UniquePlayerName name
    UniquePlayerDisplayName name
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

   PlayerStatsYear
    playerId PlayerId OnDeleteCascade
    year Int
    winsFree Int
    winsShadow Int
    lossesFree Int
    lossesShadow Int
    Primary playerId year
    deriving Show

   PlayerStatsTotal
    playerId PlayerId OnDeleteCascade
    ratingFree Rating
    ratingShadow Rating
    gameCount Int
    Primary playerId
    deriving Show

   PlayerStatsInitial
    playerId PlayerId OnDeleteCascade
    ratingFree Rating
    ratingShadow Rating
    gameCount Int
    Primary playerId
    deriving Show
|]

type PlayerStats = (PlayerStatsTotal, PlayerStatsYear)

type MaybePlayerStats = (Maybe (Entity PlayerStatsTotal), Maybe (Entity PlayerStatsYear))

type ReportInsertion = (Entity GameReport, Entity Player, Entity Player)

toPlayerStatsTotal :: PlayerStatsInitial -> PlayerStatsTotal
toPlayerStatsTotal (PlayerStatsInitial {..}) =
  PlayerStatsTotal
    { playerStatsTotalPlayerId = playerStatsInitialPlayerId,
      playerStatsTotalRatingFree = playerStatsInitialRatingFree,
      playerStatsTotalRatingShadow = playerStatsInitialRatingShadow,
      playerStatsTotalGameCount = playerStatsInitialGameCount
    }

defaultPlayerStatsTotal :: PlayerId -> PlayerStatsTotal
defaultPlayerStatsTotal pid =
  PlayerStatsTotal
    { playerStatsTotalPlayerId = pid,
      playerStatsTotalRatingFree = 500,
      playerStatsTotalRatingShadow = 500,
      playerStatsTotalGameCount = 0
    }

defaultPlayerStatsYear :: PlayerId -> Year -> PlayerStatsYear
defaultPlayerStatsYear pid year =
  PlayerStatsYear
    { playerStatsYearPlayerId = pid,
      playerStatsYearYear = year,
      playerStatsYearWinsFree = 0,
      playerStatsYearWinsShadow = 0,
      playerStatsYearLossesFree = 0,
      playerStatsYearLossesShadow = 0
    }

updatedPlayerStatsWin :: Side -> Rating -> PlayerStatsTotal -> PlayerStatsYear -> PlayerStats
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

updatedPlayerStatsLose :: Side -> Rating -> PlayerStatsTotal -> PlayerStatsYear -> PlayerStats
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
