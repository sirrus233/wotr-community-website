module Database where

import Data.Pool (withResource)
import Database.SQLite.Simple (Only (..), execute, execute_, query, query_)
import Types.App (AppM, Env (..), log)
import Types.DataField (PlayerId, PlayerName)
import Types.Database (ReadPlayer (..), WritePlayer (..), WriteProcessedGameReport)

initializeDatabase :: ReaderT Env IO ()
initializeDatabase = do
  env <- ask
  liftIO . withResource env.dbPool $ \conn -> do
    log env.logger "Initializing database"
    tableCount <- query_ conn "SELECT COUNT(*) FROM sqlite_master WHERE type='table';" :: IO [Only Int]
    case tableCount of
      [Only 0] -> do
        log env.logger "Creating tables"
        execute_ conn "PRAGMA foreign_keys = ON;"
        execute_
          conn
          "CREATE TABLE GameReports (\
          \id INTEGER NOT NULL,\
          \timestamp INTEGER NOT NULL,\
          \winner INTEGER NOT NULL,\
          \loser INTEGER NOT NULL,\
          \side TEXT NOT NULL,\
          \victory TEXT NOT NULL,\
          \match TEXT NOT NULL,\
          \competition TEXT,\
          \league TEXT,\
          \expansions TEXT NOT NULL,\
          \treebeard INTEGER,\
          \actionTokens INTEGER NOT NULL,\
          \dwarvenRings INTEGER NOT NULL,\
          \turns INTEGER NOT NULL,\
          \corruption INTEGER NOT NULL,\
          \mordor INTEGER,\
          \initialEyes INTEGER NOT NULL,\
          \aragornTurn INTEGER,\
          \strongholds TEXT NOT NULL,\
          \interestRating INTEGER NOT NULL,\
          \comments TEXT,\
          \winnerRatingAfter INTEGER NOT NULL,\
          \loserRatingAfter INTEGER NOT NULL,\
          \PRIMARY KEY(id),\
          \FOREIGN KEY(winner) REFERENCES Players(id),\
          \FOREIGN KEY(loser) REFERENCES Players(id)\
          \);"
        execute_
          conn
          "CREATE TABLE Players (\
          \id INTEGER NOT NULL,\
          \name TEXT NOT NULL UNIQUE,\
          \country TEXT,\
          \PRIMARY KEY(id)\
          \);"
        execute_ conn "CREATE INDEX idx_reports_timestamp ON GameReports (timestamp DESC);"
        execute_ conn "CREATE INDEX idx_reports_winner_timestamp ON GameReports (winner, timestamp DESC);"
        execute_ conn "CREATE INDEX idx_reports_loser_timestamp ON GameReports (loser, timestamp DESC);"
        execute_ conn "CREATE INDEX idx_players_name ON Players (name);"
      _ -> log env.logger "Database already initialized"

getPlayerByName :: PlayerName -> AppM (Maybe ReadPlayer)
getPlayerByName name = do
  env <- ask
  liftIO . withResource env.dbPool $ \conn -> do
    player <- query conn "SELECT * FROM Players WHERE name=(?)" (Only name)
    case player of
      [p] -> pure (Just p)
      _ -> pure Nothing

insertPlayerIfNotExists :: PlayerName -> AppM PlayerId
insertPlayerIfNotExists name = do
  env <- ask
  player <- getPlayerByName name
  liftIO . withResource env.dbPool $ \conn -> do
    case player of
      Nothing -> do
        execute conn "INSERT INTO Players (name, country) VALUES (?, ?)" (WritePlayer {name, country = Nothing})
        rowId <- query_ conn "SELECT last_insert_rowid();"
        case rowId of
          [Only pid] -> pure pid
          _ -> pure 0
      Just p -> pure p.pid

insertGameReport :: WriteProcessedGameReport -> AppM ()
insertGameReport report = do
  env <- ask
  liftIO . withResource env.dbPool $ \conn -> do
    execute
      conn
      "INSERT INTO GameReports (\
      \timestamp,\
      \winner,\
      \loser,\
      \side,\
      \victory,\
      \match,\
      \competition,\
      \league,\
      \expansions,\
      \treebeard,\
      \actionTokens,\
      \dwarvenRings,\
      \turns,\
      \corruption,\
      \mordor,\
      \initialEyes,\
      \aragornTurn,\
      \strongholds,\
      \interestRating,\
      \comments,\
      \winnerRatingAfter,\
      \loserRatingAfter\
      \) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      report