module Main where

import AppConfig (AppM, Env (..), databaseFile, redisConfig, runAppLogger)
import AppServer (submitReportHandler)
import Data.Csv (HasHeader (..), decode)
import Data.Vector qualified as V
import Database.Esqueleto.Experimental (defaultConnectionPoolConfig, runMigration, runSqlPool)
import Database.Persist.Sqlite (createSqlitePoolWithConfig)
import Database.Redis (connect)
import Logging (stdoutLogger)
import Migration.Database (insertLegacyEntry)
import Migration.Types
  ( ParsedGameReport,
    ParsedLegacyLadderEntry,
    PlayerBanList,
    toParsedGameReport,
    toParsedLegacyLadderEntry,
    toRawGameReport,
  )
import Servant (runHandler)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Types.DataField (PlayerName)
import Types.Database (migrateAll)

tragedies :: [PlayerName]
tragedies =
  [ "Shade",
    "Mikhael",
    "exegesis1978",
    "Mikhael Kates",
    "Fluffy1",
    "rosenbud",
    "Arathaert",
    "Interrogans",
    "Fil",
    "LUPO1972",
    "Hallow",
    "OurSaltation",
    "CaHek",
    "Jakalor",
    "Kraktus",
    "alfx23",
    "herth",
    "TheLastRoman",
    "dinosaur-chan",
    "Corey",
    "Eric Garrison",
    "Corey Chaves",
    "bd",
    "Woody23",
    "Mogus",
    "Iceman",
    "Danisimos",
    "JohnnyVictory",
    "Starlock",
    "Kakashi",
    "Guthix",
    "Tom Bombadil"
  ]

banList :: PlayerBanList
banList = ["mordak", "mellowsedge"]

rename :: PlayerName -> PlayerName
rename = \case
  "DR Sigma" -> "DrSigma"
  "Igforce" -> "igforce77"
  name -> name

migrate :: [ParsedLegacyLadderEntry] -> [ParsedGameReport] -> AppM ()
migrate legacyEntries reports = do
  traverse_ insertLegacyEntry legacyEntries
  forM_ (map toRawGameReport reports) submitReportHandler

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
  migrationResult <- runHandler . runAppLogger logger . usingReaderT env $ migrate legacyEntries reports
  case migrationResult of
    Left err -> error $ show err
    Right _ -> pass
