module Main where

import AppConfig (databaseFile, runAppLogger)
import Data.Csv (FromRecord, HasHeader (..), decode)
import Data.Vector qualified as V
import Database.Esqueleto.Experimental
  ( PersistEntity (..),
    SqlPersistT,
    defaultConnectionPoolConfig,
    insert,
    insert_,
    runMigration,
    runSqlPool,
  )
import Database.Persist.Sqlite (createSqlitePoolWithConfig)
import Logging (stdoutLogger)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Types.Database (Player (..), PlayerStatsTotal (..), PlayerStatsYear (..), migrateAll)

data LadderEntryWithTrash = LadderEntryWithTrash
  { rank :: (),
    flag :: (),
    player :: Text,
    averageRating :: (),
    shadowRating :: Int,
    freeRating :: Int,
    gamesPlayedTotal :: Int,
    gamesPlayedYear :: Int,
    fpWins :: Int,
    fpLoss :: Int,
    fpPercent :: (),
    spWins :: Int,
    spLoss :: Int,
    spPercent :: (),
    lomeFpWins :: Int,
    lomeFpLoss :: Int,
    lome3 :: (),
    lomeSpWins :: Int,
    lomeSpLoss :: Int,
    lome6 :: (),
    lome7 :: (),
    fpmv :: (),
    trash1 :: (),
    trash2 :: (),
    trash3 :: (),
    trash4 :: (),
    trash5 :: (),
    trash6 :: (),
    trash7 :: (),
    trash8 :: (),
    trash9 :: (),
    trash10 :: (),
    trash11 :: (),
    trash12 :: (),
    trash13 :: (),
    trash14 :: (),
    trash15 :: (),
    trash16 :: (),
    trash17 :: (),
    trash18 :: (),
    trash19 :: ()
  }
  deriving (Generic, Show)

instance FromRecord LadderEntryWithTrash

data LadderEntry = LadderEntry
  { player :: Text,
    shadowRating :: Int,
    freeRating :: Int,
    gamesPlayedTotal :: Int,
    gamesPlayedYear :: Int,
    fpWins :: Int,
    fpLoss :: Int,
    spWins :: Int,
    spLoss :: Int,
    lomeFpWins :: Int,
    lomeFpLoss :: Int,
    lomeSpWins :: Int,
    lomeSpLoss :: Int
  }

toLadderEntry :: LadderEntryWithTrash -> LadderEntry
toLadderEntry (LadderEntryWithTrash {..}) = LadderEntry {..}

insertPlayer :: (MonadIO m) => Text -> SqlPersistT m (Key Player)
insertPlayer name = insert $ Player name Nothing

insertStats :: (MonadIO m) => Key Player -> LadderEntry -> SqlPersistT m ()
insertStats playerId entry = do
  insert_ $ PlayerStatsTotal playerId entry.freeRating entry.shadowRating entry.gamesPlayedTotal
  insert_ $ PlayerStatsYear playerId 2024 entry.fpWins entry.spWins entry.fpLoss entry.spLoss

insertEntry :: (MonadIO m) => LadderEntry -> SqlPersistT m ()
insertEntry entry = do
  playerId <- insertPlayer entry.player
  insertStats playerId entry

main :: IO ()
main = do
  createDirectoryIfMissing True . takeDirectory $ databaseFile
  csvData <- readFileLBS "/home/bradley/downloads/ladder.csv"
  case decode NoHeader csvData of
    Left err -> putStrLn err
    Right v -> do
      logger <- stdoutLogger
      dbPool <- runAppLogger logger $ createSqlitePoolWithConfig (toText databaseFile) defaultConnectionPoolConfig
      runSqlPool (runMigration migrateAll) dbPool
      let entries = V.toList $ fmap toLadderEntry v
      traverse_ (\entry -> runSqlPool (insertEntry entry) dbPool) entries
