module Main where

import AppConfig (AppM, Env (..), databaseFile, redisConfig, runAppLogger)
import AppServer (submitReportHandler)
import Data.Csv (HasHeader (..), decode)
import Data.Validation (Validation (..))
import Data.Vector qualified as V
import Database.Esqueleto.Experimental (defaultConnectionPoolConfig, runMigration, runSqlPool)
import Database.Persist.Sqlite (createSqlitePoolWithConfig)
import Database.Redis (connect)
import Logging (stdoutLogger)
import Migration.Actions (insertLegacyEntry)
import Migration.Types
  ( ParsedGameReport,
    ParsedLegacyLadderEntry (..),
    PlayerBanList,
    toParsedGameReport,
    toParsedLegacyLadderEntry,
    toRawGameReport,
  )
import Servant (runHandler)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Types.Api (RawGameReport (..))
import Types.DataField (Victory (..))
import Types.Database (migrateAll)
import Validation (ReportError (..), validateReport)

banList :: PlayerBanList
banList = ["mordak", "mellowsedge"]

migrate :: [ParsedLegacyLadderEntry] -> [ParsedGameReport] -> AppM ()
migrate legacyEntries reports = do
  traverse_ insertLegacyEntry . filter (\entry -> entry.player `notElem` banList) $ legacyEntries
  let rawReports = map toRawGameReport reports
  forM_
    (map (liftA2 (,) validateReport id) rawReports)
    ( \case
        (Failure errs, r) -> case errs of
          [NoVictoryConditionMet] -> submitReportHandler (r {victory = Concession}) >> pass
          _ -> error $ "Unrecognized failure: " <> show errs <> " for report: " <> show r
        (Success report, _) -> submitReportHandler report >> pass
    )

main :: IO ()
main = do
  createDirectoryIfMissing True . takeDirectory $ databaseFile

  logger <- stdoutLogger
  dbPool <- runAppLogger logger $ createSqlitePoolWithConfig (toText databaseFile) defaultConnectionPoolConfig
  redisPool <- connect redisConfig

  let env = Env {dbPool, redisPool, logger}

  legacyData <- readFileLBS "migration/legacy-ladder.csv"
  reportData <- readFileLBS "migration/reports.csv"

  let legacyEntries = case decode NoHeader legacyData of
        Left err -> error $ show err
        Right a -> map toParsedLegacyLadderEntry . V.toList $ a

  let reports = case decode NoHeader reportData of
        Left err -> error $ show err
        Right a -> map toParsedGameReport . V.toList $ a

  runSqlPool (runMigration migrateAll) dbPool
  migrationResult <- runHandler . runAppLogger logger . usingReaderT env $ migrate legacyEntries (reverse reports)
  case migrationResult of
    Left err -> error $ show err
    Right _ -> pass
