module Main where

import AppConfig (AppM, Env (..), databaseFile, redisConfig, runAppLogger)
import Control.Monad.Logger (LogLevel (..))
import Data.Csv (HasHeader (..), decode)
import Data.Validation (Validation (..))
import Data.Vector qualified as V
import Database (insertGameReport, insertLegacyEntry, insertPlayerIfNotExists, runDb)
import Database.Esqueleto.Experimental (Entity (..), defaultConnectionPoolConfig, runMigration, runSqlPool)
import Database.Persist.Sqlite (createSqlitePoolWithConfig)
import Database.Redis (connect)
import Logging (Logger, log, stdoutLogger, (<>:))
import Servant (runHandler)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Types.Api (toGameReport)
import Types.DataField (Victory (..))
import Types.Database (GameReport (..), migrateAll)
import Types.Migration
  ( ParsedGameReport (..),
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
  forM_ reports $ \parsedReport -> do
    Entity winnerId _ <- insertPlayerIfNotExists parsedReport.winner
    Entity loserId _ <- insertPlayerIfNotExists parsedReport.loser
    let rawReport = toRawGameReport parsedReport
    let report = toGameReport parsedReport.timestamp winnerId loserId rawReport
    let validation = validateReport rawReport
    case validation of
      Failure errs -> case errs of
        [NoVictoryConditionMet] -> insertGameReport (report {gameReportVictory = Concession})
        _ -> error $ "Unrecognized failure: " <> show errs <> " for report: " <> show report
      Success _ -> insertGameReport report
  reprocessReports

tryParse :: FilePath -> Logger -> (a -> b) -> IO (Maybe [b])
tryParse path logger mapper = do
  raw <- readFileLBS path

  let parsed = case decode NoHeader raw of
        Left err -> Left err
        Right a -> Right . map mapper . V.toList $ a

  case parsed of
    Left err -> do
      log logger LevelError "Error parsing " <>: path <> ": " <>: err
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
