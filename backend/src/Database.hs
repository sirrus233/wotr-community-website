module Database where

import Control.Exception (throwIO)
import Data.Pool (withResource)
import Database.SQLite.Simple (Only (..), execute_, query, query_)
import Types.App (AppM, Env (..), log)
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

getPlayerByName :: PlayerName -> AppM (Maybe ReadPlayer)
getPlayerByName name = do
  env <- ask
  liftIO . withResource env.dbPool $ \conn -> do
    result <- query conn "SELECT * FROM Players WHERE name = ?" (Only name)
    case result of
      [] -> pure Nothing
      [player] -> pure (Just player)
      _ -> throwIO UnexpectedResultException

insertPlayerIfNotExists :: PlayerName -> AppM PlayerId
insertPlayerIfNotExists name = do
  env <- ask
  player <- getPlayerByName name
  liftIO . withResource env.dbPool $ \conn -> do
    case player of
      Nothing -> do
        let insertPlayerQuery = "INSERT INTO Players (name, country) VALUES (?, ?) RETURNING id"
        readSingle $ query conn insertPlayerQuery (WritePlayer {name, country = Nothing})
      Just (ReadPlayer {pid}) -> pure pid

insertGameReport :: WriteProcessedGameReport -> AppM ReadProcessedGameReport
insertGameReport report = do
  env <- ask
  liftIO . withResource env.dbPool $ \conn -> do
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
    winner <- readSingle $ query conn nameQuery (Only report.winner)
    loser <- readSingle $ query conn nameQuery (Only report.loser)

    pure
      ReadProcessedGameReport
        { rid,
          timestamp = report.timestamp,
          winner,
          loser,
          side = report.side,
          victory = report.victory,
          match = report.match,
          competition = report.competition,
          league = report.league,
          expansions = report.expansions,
          treebeard = report.treebeard,
          actionTokens = report.actionTokens,
          dwarvenRings = report.dwarvenRings,
          turns = report.turns,
          corruption = report.corruption,
          mordor = report.mordor,
          initialEyes = report.initialEyes,
          aragornTurn = report.aragornTurn,
          strongholds = report.strongholds,
          interestRating = report.interestRating,
          comments = report.comments,
          winnerRatingAfter = report.winnerRatingAfter,
          loserRatingAfter = report.loserRatingAfter
        }