module Main where

import AppConfig (AppM, Env (..), databaseFile, redisConfig, runAppLogger)
import AppServer (insertReport_, reprocessReports)
import Control.Monad.Logger (LogLevel (..))
import Data.Csv (FromRecord, HasHeader (..), decode)
import Data.Validation (Validation (..))
import Data.Vector qualified as V
import Database (insertLegacyEntry, runDb)
import Database.Esqueleto.Experimental (defaultConnectionPoolConfig, runMigration, runSqlPool)
import Database.Persist.Sqlite (createSqlitePoolWithConfig)
import Database.Redis (connect)
import Logging (Logger, log, stdoutLogger, (<>:))
import Servant (ServerError (errBody), err500, runHandler, throwError)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Types.Api (RawGameReport (victory))
import Types.DataField (Victory (..))
import Types.Database (migrateAll)
import Types.Migration
  ( ParsedGameReport (timestamp),
    ParsedLegacyLadderEntry (..),
    PlayerBanList,
    toParsedGameReport,
    toParsedLegacyLadderEntry,
    toRawGameReport,
  )
import Validation (ReportError (..), validateReport)

banList :: PlayerBanList
banList = ["mordak", "mellowsedge"]

migrate :: [ParsedLegacyLadderEntry] -> [ParsedGameReport] -> AppM ()
migrate legacyEntries reports = runDb $ do
  traverse_ insertLegacyEntry . filter (\entry -> entry.player `notElem` banList) $ legacyEntries

  forM_ (map (\r -> (r.timestamp, toRawGameReport r)) reports) $ \(timestamp, report) -> do
    case validateReport report of
      Failure errs -> case errs of
        [NoVictoryConditionMet] -> insertReport_ timestamp (report {victory = Concession})
        _ -> throwError $ err500 {errBody = "Unrecognized failure: " <>: errs <> " for report: " <>: report}
      Success _ -> insertReport_ timestamp report

  reprocessReports

tryParse :: (FromRecord a) => FilePath -> Logger -> (a -> b) -> IO (Maybe [b])
tryParse path logger mapper = do
  raw <- readFileLBS path

  let parsed = case decode NoHeader raw of
        Left err -> Left err
        Right a -> Right . map mapper . V.toList $ a

  case parsed of
    Left err -> do
      log logger LevelError $ "Error parsing " <>: path <> ": " <>: err
      pure Nothing
    Right a -> pure $ Just a

main :: IO ()
main = do
  createDirectoryIfMissing True . takeDirectory $ databaseFile

  logger <- stdoutLogger
  dbPool <- runAppLogger logger $ createSqlitePoolWithConfig (toText databaseFile) defaultConnectionPoolConfig
  redisPool <- connect redisConfig

  let env = Env {dbPool, redisPool, logger}

  legacyEntries <- tryParse "migration/legacy-ladder.csv" logger toParsedLegacyLadderEntry
  reports <- tryParse "migration/reports.csv" logger toParsedGameReport

  case (legacyEntries, reports) of
    (Just es, Just rs) -> do
      runSqlPool (runMigration migrateAll) dbPool
      migrationResult <- runHandler . runAppLogger logger . usingReaderT env $ migrate es (reverse rs)
      case migrationResult of
        Left err -> log logger LevelError $ "Migration failed: " <>: err
        Right _ -> pass
    _ -> pass
