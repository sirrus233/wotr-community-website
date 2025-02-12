{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Database where

import Control.Monad.Logger (LogLevel (..), ToLogStr (..))
import Data.Aeson (ToJSONKey (..), ToJSONKeyFunction)
import Data.Aeson.Types (toJSONKeyText)
import Data.Csv (ToField (..), ToRecord (..))
import Data.Csv qualified as CSV
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Vector qualified as V
import Database.Esqueleto.Experimental (Entity, Value, rawExecute, runMigrationQuiet, runSqlPool)
import Database.Esqueleto.Experimental qualified as SQL
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Logging (Logger, log)
import Types.DataField
  ( Competition,
    Expansion,
    League,
    LeagueTier,
    Match,
    PlayerName,
    Rating,
    Side (..),
    Stronghold,
    Victory,
    Year,
  )

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
    deriving Generic
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
    deriving Generic
    deriving Show

   PlayerStatsYear
    playerId PlayerId OnDeleteCascade
    year Int
    winsFree Int
    winsShadow Int
    lossesFree Int
    lossesShadow Int
    Primary playerId year
    deriving Generic
    deriving Show

   PlayerStatsTotal
    playerId PlayerId OnDeleteCascade
    ratingFree Rating
    ratingShadow Rating
    gameCount Int
    Primary playerId
    deriving Generic
    deriving Show

   PlayerStatsInitial
    playerId PlayerId OnDeleteCascade
    ratingFree Rating
    ratingShadow Rating
    gameCount Int
    Primary playerId
    deriving Generic
    deriving Show

  LeaguePlayer
    league League
    tier LeagueTier
    year Int
    playerId PlayerId
    Primary league tier year playerId
    deriving Generic
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

instance ToField PlayerId where
  toField :: PlayerId -> CSV.Field
  toField = toField . SQL.fromSqlKey

instance ToRecord Player where
  toRecord :: Player -> CSV.Record
  toRecord (Player name displayName country isActive) =
    V.fromList
      [ toField name,
        toField displayName,
        toField (fromMaybe ("" :: Text) country),
        toField (if isActive then "true" :: Text else "false")
      ]

instance ToRecord GameReport where
  toRecord :: GameReport -> CSV.Record
  toRecord
    ( GameReport
        timestamp
        winnerId
        loserId
        side
        victory
        match
        competition
        league
        expansions
        treebeard
        actionTokens
        dwarvenRings
        turns
        corruption
        mordor
        initialEyes
        aragornTurn
        strongholds
        interestRating
        comment
        logFile
      ) =
      V.fromList
        [ toField (show timestamp :: Text),
          toField winnerId,
          toField loserId,
          toField (show side :: Text),
          toField (show victory :: Text),
          toField (show match :: Text),
          toField (T.intercalate "," . map show $ competition),
          toField (maybe ("" :: Text) show league),
          toField (T.intercalate "," . map show $ expansions),
          toField (maybe ("" :: Text) (\b -> if b then "true" else "false") treebeard),
          toField actionTokens,
          toField dwarvenRings,
          toField turns,
          toField corruption,
          toField (maybe ("" :: Text) show mordor),
          toField initialEyes,
          toField (maybe ("" :: Text) show aragornTurn),
          toField (T.intercalate "," . map show $ strongholds),
          toField interestRating,
          toField (fromMaybe ("" :: Text) comment),
          toField (fromMaybe ("" :: Text) logFile)
        ]

instance ToRecord PlayerStatsYear where
  toRecord :: PlayerStatsYear -> CSV.Record
  toRecord (PlayerStatsYear playerId year winsFree winsShadow lossesFree lossesShadow) =
    V.fromList
      [ toField playerId,
        toField year,
        toField winsFree,
        toField winsShadow,
        toField lossesFree,
        toField lossesShadow
      ]

instance ToRecord PlayerStatsTotal where
  toRecord :: PlayerStatsTotal -> CSV.Record
  toRecord (PlayerStatsTotal playerId ratingFree ratingShadow gameCount) =
    V.fromList
      [ toField playerId,
        toField ratingFree,
        toField ratingShadow,
        toField gameCount
      ]

instance ToRecord PlayerStatsInitial where
  toRecord :: PlayerStatsInitial -> CSV.Record
  toRecord (PlayerStatsInitial playerId ratingFree ratingShadow gameCount) =
    V.fromList
      [ toField playerId,
        toField ratingFree,
        toField ratingShadow,
        toField gameCount
      ]

instance ToRecord LeaguePlayer where
  toRecord :: LeaguePlayer -> CSV.Record
  toRecord (LeaguePlayer league tier year playerId) =
    V.fromList
      [ toField (show league :: Text),
        toField (show tier :: Text),
        toField year,
        toField playerId
      ]

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
