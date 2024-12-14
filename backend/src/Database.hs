module Database where

import Data.Pool (withResource)
import Database.SQLite.Simple (Only (..), execute_, query_)
import Types.App (Env (..), log)

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
      _ -> log env.logger "Database already initialized"