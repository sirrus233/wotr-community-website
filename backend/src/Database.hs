module Database where

import Control.Exception (throwIO)
import Data.Pool (withResource)
import Database.SQLite.Simple (Only (..), execute_, query, query_)
import Database.SQLite.Simple qualified as SQL
import Types.App (Env (..), log)
import Types.DataField (PlayerId, PlayerName, Rating, Side)
import Types.Database
  ( ReadPlayer (..),
    ReadProcessedGameReport (..),
    WritePlayer (..),
    WriteProcessedGameReport (..),
    WriteRatingChange (..),
  )

data UnexpectedResultException = UnexpectedResultException deriving (Show)

instance Exception UnexpectedResultException

readZeroOrOne :: IO [r] -> IO (Maybe r)
readZeroOrOne rs = rs >>= \case [] -> pure Nothing; [r] -> pure $ Just r; _ -> throwIO UnexpectedResultException

readOne :: IO [r] -> IO r
readOne rs = rs >>= \case [r] -> pure r; _ -> throwIO UnexpectedResultException

initializeDatabase :: ReaderT Env IO ()
initializeDatabase = do
  env <- ask
  liftIO . withResource env.dbPool $ \conn -> do
    log env.logger "Initializing database"
    tableCount <- fromOnly <$> readOne (query_ conn "SELECT COUNT(*) FROM sqlite_master WHERE type='table';") :: IO Int
    case tableCount of
      0 -> do
        log env.logger "Creating tables"
        execute_ conn "PRAGMA foreign_keys = ON;"
        execute_
          conn
          "CREATE TABLE GameReports (\
          \id INTEGER NOT NULL,\
          \timestamp INTEGER NOT NULL,\
          \winnerId INTEGER NOT NULL,\
          \loserId INTEGER NOT NULL,\
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
          \PRIMARY KEY(id),\
          \FOREIGN KEY(winnerId) REFERENCES Players(id),\
          \FOREIGN KEY(loserId) REFERENCES Players(id)\
          \);"
        execute_
          conn
          "CREATE TABLE Players (\
          \id INTEGER NOT NULL,\
          \name TEXT NOT NULL UNIQUE,\
          \country TEXT,\
          \PRIMARY KEY(id)\
          \);"
        execute_
          conn
          "CREATE TABLE Ratings (\
          \id INTEGER NOT NULL,\
          \playerId INTEGER NOT NULL,\
          \side TEXT NOT NULL,\
          \timestamp INTEGER NOT NULL,\
          \reportId INTEGER NOT NULL,\
          \ratingBefore INTEGER NOT NULL,\
          \ratingAfter INTEGER NOT NULL,\
          \PRIMARY KEY(id),\
          \FOREIGN KEY(playerId) REFERENCES Players(id),\
          \FOREIGN KEY(reportId) REFERENCES GameReports(id));"
        execute_ conn "CREATE INDEX idx_reports_timestamp ON GameReports (timestamp DESC);"
        execute_ conn "CREATE INDEX idx_players_name ON Players (name);"
        execute_ conn "CREATE INDEX idx_ratings_player ON Ratings (playerId, side, timestamp DESC);"
      _ -> log env.logger "Database already initialized"

getPlayerByName :: SQL.Connection -> PlayerName -> IO (Maybe ReadPlayer)
getPlayerByName conn name = readZeroOrOne $ query conn "SELECT * FROM Players WHERE name = ?" (Only name)

insertPlayerIfNotExists :: SQL.Connection -> PlayerName -> IO PlayerId
insertPlayerIfNotExists conn name = do
  player <- getPlayerByName conn name
  case player of
    Nothing -> do
      let insertPlayerQuery = "INSERT INTO Players (name, country) VALUES (?, ?) RETURNING id"
      fromOnly <$> readOne (query conn insertPlayerQuery (WritePlayer {name, country = Nothing}))
    Just (ReadPlayer {pid}) -> pure pid

getLatestRating :: SQL.Connection -> Rating -> PlayerId -> Side -> IO Rating
getLatestRating conn defaultRating pid side = do
  let getRatingQuery = "SELECT ratingAfter FROM Ratings WHERE (playerId, side) = (?, ?) ORDER BY timestamp DESC LIMIT 1"
  rating <- fmap fromOnly <$> readZeroOrOne (query conn getRatingQuery (pid, side))
  case rating of
    Nothing -> pure defaultRating
    Just r -> pure r

insertRatingChange :: SQL.Connection -> WriteRatingChange -> IO Rating
insertRatingChange conn rating = do
  let insertRatingQuery =
        "INSERT INTO Ratings (\
        \playerId,\
        \side,\
        \timestamp,\
        \reportId,\
        \ratingBefore,\
        \ratingAfter\
        \) VALUES (?, ?, ?, ?, ?, ?)\
        \RETURNING ratingAfter"
  fromOnly <$> readOne (query conn insertRatingQuery rating)

insertGameReport :: SQL.Connection -> WriteProcessedGameReport -> IO ReadProcessedGameReport
insertGameReport conn report@(WriteProcessedGameReport {..}) = do
  let insertReportQuery =
        "INSERT INTO GameReports (\
        \timestamp,\
        \winnerId,\
        \loserId,\
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
        \comments\
        \) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)\
        \RETURNING id"
  rid <- fromOnly <$> readOne (query conn insertReportQuery report)

  let nameQuery = "SELECT name FROM Players WHERE id = ?"
  winner <- fromOnly <$> readOne (query conn nameQuery (Only winnerId))
  loser <- fromOnly <$> readOne (query conn nameQuery (Only loserId))

  pure ReadProcessedGameReport {..}
