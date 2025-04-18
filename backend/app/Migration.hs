module Main where

import Amazonka qualified as AWS
import AppConfig (AppM, Env (..), authDatabaseFile, databaseFile, logFilter, redisConfig, runAppLogger)
import AppServer (insertReport_, reprocessReports)
import Control.Monad.Logger (LogLevel (..), MonadLogger)
import Data.Csv (FromRecord, HasHeader (..), decode)
import Data.Validation (Validation (..))
import Data.Vector qualified as V
import Database (DBAction, insertInitialStats, insertPlayerIfNotExists, repsertPlayerStats, runDb)
import Database.Esqueleto.Experimental (Entity (..), defaultConnectionPoolConfig)
import Database.Persist.Sqlite (createSqlitePoolWithConfig)
import Database.Redis (connect)
import Logging (Logger, stdoutLogger, toAwsLogger, (<>:))
import Logging qualified as L
import Servant (ServerError (errBody), err500, runHandler, throwError)
import System.Directory (createDirectoryIfMissing)
import System.Environment (setEnv)
import System.FilePath (takeDirectory)
import Types.Api (RawGameReport (victory))
import Types.DataField (Victory (..))
import Types.Database (PlayerStatsInitial (..), PlayerStatsTotal (..), defaultPlayerStatsYear, migrateSchema)
import Types.Migration
  ( ParsedGameReport (log, timestamp),
    ParsedLegacyLadderEntry (..),
    PlayerBanList,
    toParsedGameReport,
    toParsedLegacyLadderEntry,
    toRawGameReport,
  )
import Validation (ReportError (..), validateReport)

banList :: PlayerBanList
banList = ["mordak", "mellowsedge"]

insertLegacyEntry :: (MonadIO m, MonadLogger m) => ParsedLegacyLadderEntry -> DBAction m ()
insertLegacyEntry entry = do
  (Entity playerId _) <- insertPlayerIfNotExists entry.player entry.country

  let initialStats = PlayerStatsInitial playerId entry.freeRating entry.shadowRating entry.gamesPlayedTotal
  let totalStats = PlayerStatsTotal playerId entry.freeRating entry.shadowRating entry.gamesPlayedTotal
  let yearStats = defaultPlayerStatsYear playerId 2022

  insertInitialStats initialStats
  repsertPlayerStats (totalStats, yearStats)

migrate :: [ParsedLegacyLadderEntry] -> [ParsedGameReport] -> AppM ()
migrate legacyEntries reports = runDb $ do
  traverse_ insertLegacyEntry . filter (\entry -> entry.player `notElem` banList) $ legacyEntries

  forM_ (map (\r -> (r.timestamp, toRawGameReport r, r.log)) reports) $ \(timestamp, report, s3Url) -> do
    case validateReport report of
      Failure errs -> case errs of
        [NoVictoryConditionMet] -> insertReport_ timestamp (report {victory = Concession}) s3Url
        _ -> throwError $ err500 {errBody = "Unrecognized failure: " <>: errs <> " for report: " <>: report}
      Success _ -> insertReport_ timestamp report s3Url

  reprocessReports

tryParse :: (FromRecord a) => FilePath -> Logger -> (a -> b) -> IO (Maybe [b])
tryParse path logger mapper = do
  raw <- readFileLBS path

  let parsed = case decode NoHeader raw of
        Left err -> Left err
        Right a -> Right . map mapper . V.toList $ a

  case parsed of
    Left err -> do
      L.log logger LevelError $ "Error parsing " <>: path <> ": " <>: err
      pure Nothing
    Right a -> pure $ Just a

main :: IO ()
main = do
  setEnv "AWS_PROFILE" "wotrcommunity"
  createDirectoryIfMissing True . takeDirectory $ databaseFile

  logger <- stdoutLogger
  dbPool <- runAppLogger logger $ createSqlitePoolWithConfig (toText databaseFile) defaultConnectionPoolConfig
  authDbPool <- runAppLogger logger $ createSqlitePoolWithConfig (toText authDatabaseFile) defaultConnectionPoolConfig
  redisPool <- connect redisConfig
  aws <- AWS.newEnv AWS.discover >>= \awsEnv -> pure $ awsEnv {AWS.logger = toAwsLogger logFilter logger, AWS.region = AWS.Oregon}

  let env = Env {dbPool, authDbPool, redisPool, logger, aws}

  legacyEntries <- tryParse "migration/legacy-ladder.csv" logger toParsedLegacyLadderEntry
  reports <- tryParse "migration/reports.csv" logger (toParsedGameReport aws)

  case (legacyEntries, reports) of
    (Just es, Just rs) -> do
      migrateSchema dbPool logger
      migrationResult <- runHandler . runAppLogger logger . usingReaderT env $ migrate es (reverse rs)
      case migrationResult of
        Left err -> L.log logger LevelError $ "Migration failed: " <>: err
        Right _ -> pass
    _ -> pass
