{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Database where

import Control.Monad.Logger (LogLevel (..), ToLogStr (..))
import Data.Aeson (ToJSONKey (..), ToJSONKeyFunction)
import Data.Aeson.Types (toJSONKeyText)
import Data.Time (UTCTime)
import Database.Esqueleto.Experimental (Entity, Value, rawExecute, runMigrationQuiet, runSqlPool)
import Database.Esqueleto.Experimental qualified as SQL
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Logging (Logger, log)
import Types.DataField (Competition, Expansion, League, LeagueTier, Match, PlayerName, Rating, Side (..), Stronghold, Victory, Year)

share
  [mkPersist sqlSettings, mkMigrate "migrateAdmin"]
  [persistLowerCase|
   Admin
    userId Text
    sessionId Text Maybe
    UniqueAdminUserId userId
    UniqueAdminSessionId sessionId !force
    deriving Show
|]

share
  [mkPersist sqlSettings, mkMigrate "migrateDb"]
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
    comment Text Maybe
    logFile Text Maybe
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

  LeaguePlayer
    league League
    tier LeagueTier
    year Int
    playerId PlayerId
    Primary league tier year playerId
    deriving Show
|]

migrateSchema :: SQL.ConnectionPool -> Logger -> IO ()
migrateSchema dbPool logger = do
  migrations <- runSqlPool (runMigrationQuiet migrateDb) dbPool
  unless (null migrations) (log logger LevelWarn "Database schema changed. Running migrations.")
  mapM_ (log logger LevelDebug . toLogStr) migrations

  let index_statements =
        [ "CREATE INDEX IF NOT EXISTS idx_game_report_winner_id ON game_report (winner_id);",
          "CREATE INDEX IF NOT EXISTS idx_game_report_loser_id ON game_report (loser_id);",
          "CREATE INDEX IF NOT EXISTS idx_game_report_timestamp ON game_report (timestamp);",
          "CREATE INDEX IF NOT EXISTS idx_game_report_winner_loser_timestamp ON game_report (winner_id, loser_id, timestamp);"
        ]

  foldMapM ((`runSqlPool` dbPool) . (`rawExecute` [])) index_statements

instance ToJSONKey PlayerId where
  toJSONKey :: ToJSONKeyFunction PlayerId
  toJSONKey = toJSONKeyText (show . SQL.fromSqlKey)

type PlayerStats = (PlayerStatsTotal, PlayerStatsYear)

type MaybePlayerStats = (Maybe (Entity PlayerStatsTotal), Maybe (Entity PlayerStatsYear))

type ReportInsertion = (Entity GameReport, Entity Player, Entity Player)

type LeagueGameSummaryRecord = (Value PlayerId, (Value Text, Value Int, Value Int))

type LeagueGameSummaryMap = Map PlayerId (Text, Int, Int)

type LeagueGameStatsRecord = (Value PlayerId, (Value PlayerId, Value Text, Value Int, Value Int))

type LeagueGameStatsMap = Map PlayerId [(PlayerId, Text, Int, Int)]

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
