module Main where

import AppConfig (databaseFile, runAppLogger)
import AppServer (normalizeName)
import Data.Csv (HasHeader (..), decode)
import Data.Vector qualified as V
import Database.Esqueleto.Experimental (SqlPersistT, defaultConnectionPoolConfig, runMigration, runSqlPool)
import Database.Persist.Sqlite (createSqlitePoolWithConfig)
import Logging (stdoutLogger)
import Migration.Database (insertGameReport, insertLegacyEntry)
import Migration.Types (ParsedGameReport, ParsedLadderEntry, ParsedLegacyLadderEntry, PlayerBanList, toParsedGameReport, toParsedLadderEntry, toParsedLegacyLadderEntry)
import Relude.Extra (traverseToSnd)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Types.DataField (PlayerName)
import Types.Database (PlayerId, migrateAll)

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

migrate :: [ParsedLegacyLadderEntry] -> [HashMap PlayerName PlayerId -> ParsedGameReport] -> SqlPersistT IO ()
migrate legacyEntries reports = do
  playerMap <- fromList <$> traverse insertLegacyEntry legacyEntries
  pure playerMap

-- sadPlayers <- traverse (traverseToSnd insertPlayer . normalizeName) tragedies
-- traverse_ (insertGameReport . ($ fromList $ sadPlayers <> players)) reports

main :: IO ()
main = do
  createDirectoryIfMissing True . takeDirectory $ databaseFile

  legacyData <- readFileLBS "migration/legacy-ladder.csv"
  ladderData <- readFileLBS "migration/ladder.csv"
  reportData <- readFileLBS "migration/reports.csv"

  let legacyEntries = case decode NoHeader legacyData of
        Left err -> error $ show err
        Right raw -> map toParsedLegacyLadderEntry . V.toList $ raw

  -- let ladderEntries = case decode NoHeader ladderData of
  --       Left err -> error $ show err
  --       Right raw -> mapMaybe (toParsedLadderEntry banList) . V.toList $ raw

  let reports = case decode NoHeader reportData of
        Left err -> error $ show err
        Right raw -> V.toList $ fmap toParsedGameReport raw

  logger <- stdoutLogger
  dbPool <- runAppLogger logger $ createSqlitePoolWithConfig (toText databaseFile) defaultConnectionPoolConfig

  runSqlPool (runMigration migrateAll) dbPool
  runSqlPool (migrate ladderEntries reports) dbPool
