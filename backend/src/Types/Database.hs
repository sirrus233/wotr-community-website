{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Database where

import Control.Monad.Logger (LogLevel (..), ToLogStr (..))
import Data.Aeson (ToJSONKey (..), ToJSONKeyFunction)
import Data.Aeson.Types (toJSONKeyText)
import Data.Csv (Header, ToField (..), ToNamedRecord (..), (.=))
import Data.Csv qualified as CSV
import Data.Text qualified as T
import Data.Time (UTCTime)
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
    musterPoints Int default=0
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
    playerId PlayerId OnDeleteCascade
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

instance ToNamedRecord Player where
  toNamedRecord :: Player -> CSV.NamedRecord
  toNamedRecord Player {..} =
    CSV.namedRecord
      [ "name" .= playerName,
        "display_name" .= playerDisplayName,
        "country" .= fromMaybe ("" :: Text) playerCountry,
        "is_active" .= if playerIsActive then ("Active" :: Text) else "Inactive"
      ]

playerCsvHeader :: Header
playerCsvHeader = CSV.header ["name", "display_name", "country", "is_active"]

data ExportGameReport = ExportGameReport
  { exportGameReportRecord :: GameReport,
    exportGameReportWinnerName :: PlayerName,
    exportGameReportLoserName :: PlayerName
  }

instance ToNamedRecord ExportGameReport where
  toNamedRecord :: ExportGameReport -> CSV.NamedRecord
  toNamedRecord
    ExportGameReport
      { exportGameReportRecord =
          GameReport {..},
        ..
      } =
      CSV.namedRecord
        [ "timestamp" .= T.show gameReportTimestamp,
          "winner_name" .= exportGameReportWinnerName,
          "loser_name" .= exportGameReportLoserName,
          "side" .= T.show gameReportSide,
          "victory" .= T.show gameReportVictory,
          "match" .= T.show gameReportMatch,
          "competition" .= T.intercalate "," (map show gameReportCompetition),
          "league" .= maybe ("" :: Text) show gameReportLeague,
          "expansions" .= T.intercalate "," (map show gameReportExpansions),
          "treebeard" .= maybe ("" :: Text) (\b -> if b then "true" else "false") gameReportTreebeard,
          "action_tokens" .= gameReportActionTokens,
          "dwarven_rings" .= gameReportDwarvenRings,
          "turns" .= gameReportTurns,
          "corruption" .= gameReportCorruption,
          "mordor" .= maybe ("" :: Text) show gameReportMordor,
          "initial_eyes" .= gameReportInitialEyes,
          "aragorn_turn" .= maybe ("" :: Text) show gameReportAragornTurn,
          "strongholds" .= T.intercalate "," (map show gameReportStrongholds),
          "interest_rating" .= gameReportInterestRating,
          "comment" .= fromMaybe ("" :: Text) gameReportComment,
          "log_file" .= fromMaybe ("" :: Text) gameReportLogFile
        ]

gameReportCsvHeader :: Header
gameReportCsvHeader =
  CSV.header
    [ "timestamp",
      "winner_name",
      "loser_name",
      "side",
      "victory",
      "match",
      "competition",
      "league",
      "expansions",
      "treebeard",
      "action_tokens",
      "dwarven_rings",
      "turns",
      "corruption",
      "mordor",
      "initial_eyes",
      "aragorn_turn",
      "strongholds",
      "interest_rating",
      "comment",
      "log_file"
    ]

data ExportPlayerStatsYear = ExportPlayerStatsYear
  { exportPlayerStatsYearRecord :: PlayerStatsYear,
    exportPlayerStatsYearPlayerName :: PlayerName
  }

instance ToNamedRecord ExportPlayerStatsYear where
  toNamedRecord :: ExportPlayerStatsYear -> CSV.NamedRecord
  toNamedRecord
    ExportPlayerStatsYear
      { exportPlayerStatsYearRecord = PlayerStatsYear {..},
        ..
      } =
      CSV.namedRecord
        [ "player_name" .= exportPlayerStatsYearPlayerName,
          "year" .= playerStatsYearYear,
          "wins_free" .= playerStatsYearWinsFree,
          "wins_shadow" .= playerStatsYearWinsShadow,
          "losses_free" .= playerStatsYearLossesFree,
          "losses_shadow" .= playerStatsYearLossesShadow
        ]

playerStatsYearCsvHeader :: Header
playerStatsYearCsvHeader =
  CSV.header
    ["player_name", "year", "wins_free", "wins_shadow", "losses_free", "losses_shadow"]

data ExportPlayerStatsTotal = ExportPlayerStatsTotal
  { exportPlayerStatsTotalRecord :: PlayerStatsTotal,
    exportPlayerStatsTotalPlayerName :: PlayerName
  }

instance ToNamedRecord ExportPlayerStatsTotal where
  toNamedRecord :: ExportPlayerStatsTotal -> CSV.NamedRecord
  toNamedRecord
    ExportPlayerStatsTotal
      { exportPlayerStatsTotalRecord = PlayerStatsTotal {..},
        ..
      } =
      CSV.namedRecord
        [ "player_name" .= exportPlayerStatsTotalPlayerName,
          "rating_free" .= playerStatsTotalRatingFree,
          "rating_shadow" .= playerStatsTotalRatingShadow,
          "game_count" .= playerStatsTotalGameCount
        ]

playerStatsTotalCsvHeader :: Header
playerStatsTotalCsvHeader =
  CSV.header ["player_name", "rating_free", "rating_shadow", "game_count"]

data ExportPlayerStatsInitial = ExportPlayerStatsInitial
  { exportPlayerStatsInitialRecord :: PlayerStatsInitial,
    exportPlayerStatsInitialPlayerName :: PlayerName
  }

instance ToNamedRecord ExportPlayerStatsInitial where
  toNamedRecord :: ExportPlayerStatsInitial -> CSV.NamedRecord
  toNamedRecord
    ExportPlayerStatsInitial
      { exportPlayerStatsInitialRecord = PlayerStatsInitial {..},
        ..
      } =
      CSV.namedRecord
        [ "player_name" .= exportPlayerStatsInitialPlayerName,
          "rating_free" .= playerStatsInitialRatingFree,
          "rating_shadow" .= playerStatsInitialRatingShadow,
          "game_count" .= playerStatsInitialGameCount
        ]

playerStatsInitialCsvHeader :: Header
playerStatsInitialCsvHeader =
  CSV.header ["player_name", "rating_free", "rating_shadow", "game_count"]

data ExportLeaguePlayer = ExportLeaguePlayer
  { exportLeaguePlayerRecord :: LeaguePlayer,
    exportLeaguePlayerPlayerName :: PlayerName
  }

instance ToNamedRecord ExportLeaguePlayer where
  toNamedRecord :: ExportLeaguePlayer -> CSV.NamedRecord
  toNamedRecord
    ExportLeaguePlayer
      { exportLeaguePlayerRecord = LeaguePlayer {..},
        ..
      } =
      CSV.namedRecord
        [ "league" .= T.show leaguePlayerLeague,
          "tier" .= T.show leaguePlayerTier,
          "year" .= leaguePlayerYear,
          "player_name" .= exportLeaguePlayerPlayerName
        ]

leaguePlayerCsvHeader :: Header
leaguePlayerCsvHeader = CSV.header ["league", "tier", "year", "player_name"]
