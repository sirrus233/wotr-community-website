{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Database where

import Control.Monad.Logger (LogLevel (..), ToLogStr (..))
import Data.Aeson (ToJSONKey (..), ToJSONKeyFunction)
import Data.Aeson.Types (toJSONKeyText)
import Data.Csv (Header, ToField (..), ToNamedRecord (..), ToRecord (..), (.=))
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

data ExportPlayer = ExportPlayer
  { exportPlayerId :: PlayerId,
    exportPlayerRecord :: Player
  }

instance ToNamedRecord ExportPlayer where
  toNamedRecord :: ExportPlayer -> CSV.NamedRecord
  toNamedRecord
    ExportPlayer
      { exportPlayerId,
        exportPlayerRecord =
          Player
            { playerName,
              playerDisplayName,
              playerCountry,
              playerIsActive
            }
      } =
      CSV.namedRecord
        [ "id" .= exportPlayerId,
          "name" .= playerName,
          "display_name" .= playerDisplayName,
          "country" .= fromMaybe ("" :: Text) playerCountry,
          "is_active" .= if playerIsActive then ("true" :: Text) else "false"
        ]

playerCsvHeader :: Header
playerCsvHeader = CSV.header ["id", "name", "display_name", "country", "is_active"]

data ExportGameReport = ExportGameReport
  { exportGameReportRecord :: GameReport,
    exportGameReportWinnerName :: PlayerName,
    exportGameReportLoserName :: PlayerName
  }

instance ToRecord ExportGameReport where
  toRecord :: ExportGameReport -> CSV.Record
  toRecord
    ExportGameReport
      { exportGameReportRecord =
          GameReport
            { gameReportTimestamp,
              gameReportWinnerId,
              gameReportLoserId,
              gameReportSide,
              gameReportVictory,
              gameReportMatch,
              gameReportCompetition,
              gameReportLeague,
              gameReportExpansions,
              gameReportTreebeard,
              gameReportActionTokens,
              gameReportDwarvenRings,
              gameReportTurns,
              gameReportCorruption,
              gameReportMordor,
              gameReportInitialEyes,
              gameReportAragornTurn,
              gameReportStrongholds,
              gameReportInterestRating,
              gameReportComment,
              gameReportLogFile
            },
        ..
      } =
      V.fromList
        [ toField (show gameReportTimestamp :: Text),
          toField gameReportWinnerId,
          toField exportGameReportWinnerName,
          toField gameReportLoserId,
          toField exportGameReportLoserName,
          toField (show gameReportSide :: Text),
          toField (show gameReportVictory :: Text),
          toField (show gameReportMatch :: Text),
          toField (T.intercalate "," . map show $ gameReportCompetition),
          toField (maybe ("" :: Text) show gameReportLeague),
          toField (T.intercalate "," . map show $ gameReportExpansions),
          toField (maybe ("" :: Text) (\b -> if b then "true" else "false") gameReportTreebeard),
          toField gameReportActionTokens,
          toField gameReportDwarvenRings,
          toField gameReportTurns,
          toField gameReportCorruption,
          toField (maybe ("" :: Text) show gameReportMordor),
          toField gameReportInitialEyes,
          toField (maybe ("" :: Text) show gameReportAragornTurn),
          toField (T.intercalate "," . map show $ gameReportStrongholds),
          toField gameReportInterestRating,
          toField (fromMaybe ("" :: Text) gameReportComment),
          toField (fromMaybe ("" :: Text) gameReportLogFile)
        ]

instance ToNamedRecord ExportGameReport where
  toNamedRecord :: ExportGameReport -> CSV.NamedRecord
  toNamedRecord
    ExportGameReport
      { exportGameReportRecord =
          GameReport
            { gameReportTimestamp,
              gameReportWinnerId,
              gameReportLoserId,
              gameReportSide,
              gameReportVictory,
              gameReportMatch,
              gameReportCompetition,
              gameReportLeague,
              gameReportExpansions,
              gameReportTreebeard,
              gameReportActionTokens,
              gameReportDwarvenRings,
              gameReportTurns,
              gameReportCorruption,
              gameReportMordor,
              gameReportInitialEyes,
              gameReportAragornTurn,
              gameReportStrongholds,
              gameReportInterestRating,
              gameReportComment,
              gameReportLogFile
            },
        ..
      } =
      CSV.namedRecord
        [ "timestamp" .= T.show gameReportTimestamp,
          "winner_id" .= gameReportWinnerId,
          "winner_name" .= exportGameReportWinnerName,
          "loser_id" .= gameReportLoserId,
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
      "winner_id",
      "winner_name",
      "loser_id",
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

instance ToRecord ExportPlayerStatsYear where
  toRecord :: ExportPlayerStatsYear -> CSV.Record
  toRecord
    ExportPlayerStatsYear
      { exportPlayerStatsYearRecord =
          PlayerStatsYear
            { playerStatsYearPlayerId,
              playerStatsYearYear,
              playerStatsYearWinsFree,
              playerStatsYearWinsShadow,
              playerStatsYearLossesFree,
              playerStatsYearLossesShadow
            },
        ..
      } =
      V.fromList
        [ toField playerStatsYearPlayerId,
          toField exportPlayerStatsYearPlayerName,
          toField playerStatsYearYear,
          toField playerStatsYearWinsFree,
          toField playerStatsYearWinsShadow,
          toField playerStatsYearLossesFree,
          toField playerStatsYearLossesShadow
        ]

instance ToNamedRecord ExportPlayerStatsYear where
  toNamedRecord :: ExportPlayerStatsYear -> CSV.NamedRecord
  toNamedRecord
    ExportPlayerStatsYear
      { exportPlayerStatsYearRecord =
          PlayerStatsYear
            { playerStatsYearPlayerId,
              playerStatsYearYear,
              playerStatsYearWinsFree,
              playerStatsYearWinsShadow,
              playerStatsYearLossesFree,
              playerStatsYearLossesShadow
            },
        ..
      } =
      CSV.namedRecord
        [ "player_id" .= playerStatsYearPlayerId,
          "player_name" .= exportPlayerStatsYearPlayerName,
          "year" .= playerStatsYearYear,
          "wins_free" .= playerStatsYearWinsFree,
          "wins_shadow" .= playerStatsYearWinsShadow,
          "losses_free" .= playerStatsYearLossesFree,
          "losses_shadow" .= playerStatsYearLossesShadow
        ]

playerStatsYearCsvHeader :: Header
playerStatsYearCsvHeader =
  CSV.header
    ["player_id", "player_name", "year", "wins_free", "wins_shadow", "losses_free", "losses_shadow"]

data ExportPlayerStatsTotal = ExportPlayerStatsTotal
  { exportPlayerStatsTotalRecord :: PlayerStatsTotal,
    exportPlayerStatsTotalPlayerName :: PlayerName
  }

instance ToNamedRecord ExportPlayerStatsTotal where
  toNamedRecord :: ExportPlayerStatsTotal -> CSV.NamedRecord
  toNamedRecord
    ExportPlayerStatsTotal
      { exportPlayerStatsTotalRecord =
          PlayerStatsTotal
            { playerStatsTotalPlayerId,
              playerStatsTotalRatingFree,
              playerStatsTotalRatingShadow,
              playerStatsTotalGameCount
            },
        ..
      } =
      CSV.namedRecord
        [ "player_id" .= playerStatsTotalPlayerId,
          "player_name" .= exportPlayerStatsTotalPlayerName,
          "rating_free" .= playerStatsTotalRatingFree,
          "rating_shadow" .= playerStatsTotalRatingShadow,
          "game_count" .= playerStatsTotalGameCount
        ]

playerStatsTotalCsvHeader :: Header
playerStatsTotalCsvHeader =
  CSV.header ["player_id", "player_name", "rating_free", "rating_shadow", "game_count"]

data ExportPlayerStatsInitial = ExportPlayerStatsInitial
  { exportPlayerStatsInitialRecord :: PlayerStatsInitial,
    exportPlayerStatsInitialPlayerName :: PlayerName
  }

instance ToRecord ExportPlayerStatsInitial where
  toRecord :: ExportPlayerStatsInitial -> CSV.Record
  toRecord
    ExportPlayerStatsInitial
      { exportPlayerStatsInitialRecord =
          PlayerStatsInitial
            { playerStatsInitialPlayerId,
              playerStatsInitialRatingFree,
              playerStatsInitialRatingShadow,
              playerStatsInitialGameCount
            },
        ..
      } =
      V.fromList
        [ toField playerStatsInitialPlayerId,
          toField exportPlayerStatsInitialPlayerName,
          toField playerStatsInitialRatingFree,
          toField playerStatsInitialRatingShadow,
          toField playerStatsInitialGameCount
        ]

instance ToNamedRecord ExportPlayerStatsInitial where
  toNamedRecord :: ExportPlayerStatsInitial -> CSV.NamedRecord
  toNamedRecord
    ExportPlayerStatsInitial
      { exportPlayerStatsInitialRecord =
          PlayerStatsInitial
            { playerStatsInitialPlayerId,
              playerStatsInitialRatingFree,
              playerStatsInitialRatingShadow,
              playerStatsInitialGameCount
            },
        ..
      } =
      CSV.namedRecord
        [ "player_id" .= playerStatsInitialPlayerId,
          "player_name" .= exportPlayerStatsInitialPlayerName,
          "rating_free" .= playerStatsInitialRatingFree,
          "rating_shadow" .= playerStatsInitialRatingShadow,
          "game_count" .= playerStatsInitialGameCount
        ]

playerStatsInitialCsvHeader :: Header
playerStatsInitialCsvHeader =
  CSV.header ["player_id", "player_name", "rating_free", "rating_shadow", "game_count"]

data ExportLeaguePlayer = ExportLeaguePlayer
  { exportLeaguePlayerRecord :: LeaguePlayer,
    exportLeaguePlayerPlayerName :: PlayerName
  }

instance ToRecord ExportLeaguePlayer where
  toRecord :: ExportLeaguePlayer -> CSV.Record
  toRecord
    ExportLeaguePlayer
      { exportLeaguePlayerRecord =
          LeaguePlayer
            { leaguePlayerLeague,
              leaguePlayerTier,
              leaguePlayerYear,
              leaguePlayerPlayerId
            },
        ..
      } =
      V.fromList
        [ toField (show leaguePlayerLeague :: Text),
          toField (show leaguePlayerTier :: Text),
          toField leaguePlayerYear,
          toField leaguePlayerPlayerId,
          toField exportLeaguePlayerPlayerName
        ]

instance ToNamedRecord ExportLeaguePlayer where
  toNamedRecord :: ExportLeaguePlayer -> CSV.NamedRecord
  toNamedRecord
    ExportLeaguePlayer
      { exportLeaguePlayerRecord =
          LeaguePlayer
            { leaguePlayerLeague,
              leaguePlayerTier,
              leaguePlayerYear,
              leaguePlayerPlayerId
            },
        ..
      } =
      CSV.namedRecord
        [ "league" .= T.show leaguePlayerLeague,
          "tier" .= T.show leaguePlayerTier,
          "year" .= leaguePlayerYear,
          "player_id" .= leaguePlayerPlayerId,
          "player_name" .= exportLeaguePlayerPlayerName
        ]

leaguePlayerCsvHeader :: Header
leaguePlayerCsvHeader = CSV.header ["league", "tier", "year", "player_id", "player_name"]

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
