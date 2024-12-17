module Database where

import Control.Exception (throwIO)
import Data.Pool (withResource)
import Database.SQLite.Simple (Only (..), execute_, query, query_)
import Database.SQLite.Simple qualified as SQL
import Types.App (Env (..), log)
import Types.DataField (PlayerId, PlayerName)
import Types.Database (ReadPlayer (..), ReadProcessedGameReport (..), WritePlayer (..), WriteProcessedGameReport (..))

data UnexpectedResultException = UnexpectedResultException deriving (Show)

instance Exception UnexpectedResultException

readSingle :: IO [Only r] -> IO r
readSingle rs = rs >>= \case [Only r] -> pure r; _ -> throwIO UnexpectedResultException

initializeDatabase :: ReaderT Env IO ()
initializeDatabase = do
  env <- ask
  liftIO . withResource env.dbPool $ \conn -> do
    log env.logger "Initializing database"
    tableCount <- readSingle $ query_ conn "SELECT COUNT(*) FROM sqlite_master WHERE type='table';" :: IO Int
    case tableCount of
      0 -> do
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

getPlayerByName :: SQL.Connection -> PlayerName -> IO (Maybe ReadPlayer)
getPlayerByName conn name = do
  result <- query conn "SELECT * FROM Players WHERE name = ?" (Only name)
  case result of
    [] -> pure Nothing
    [player] -> pure (Just player)
    _ -> throwIO UnexpectedResultException

insertPlayerIfNotExists :: SQL.Connection -> PlayerName -> IO PlayerId
insertPlayerIfNotExists conn name = do
  player <- getPlayerByName conn name
  case player of
    Nothing -> do
      let insertPlayerQuery = "INSERT INTO Players (name, country) VALUES (?, ?) RETURNING id"
      readSingle $ query conn insertPlayerQuery (WritePlayer {name, country = Nothing})
    Just (ReadPlayer {pid}) -> pure pid

insertGameReport :: SQL.Connection -> WriteProcessedGameReport -> IO ReadProcessedGameReport
insertGameReport conn report@(WriteProcessedGameReport {..}) = do
  let insertReportQuery =
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
        \) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)\
        \RETURNING id"
  rid <- readSingle $ query conn insertReportQuery report

  let nameQuery = "SELECT name FROM Players WHERE id = ?"
  winner <- readSingle $ query conn nameQuery (Only report.winnerId)
  loser <- readSingle $ query conn nameQuery (Only report.loserId)

  pure ReadProcessedGameReport {..}
