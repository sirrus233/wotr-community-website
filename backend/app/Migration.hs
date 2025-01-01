module Main where

import AppConfig (databaseFile, runAppLogger)
import Data.Csv (HasHeader (..), decode)
import Data.Vector qualified as V
import Database.Esqueleto.Experimental (SqlPersistT, defaultConnectionPoolConfig, runMigration, runSqlPool)
import Database.Persist.Sqlite (createSqlitePoolWithConfig)
import Logging (stdoutLogger)
import Migration.Database (insertEntry, insertGameReport)
import Migration.Types (ParsedGameReport, ParsedLadderEntry, PlayerBanList, toParsedGameReport, toParsedLadderEntry)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Types.DataField (PlayerName)
import Types.Database (PlayerId, migrateAll)

tragedies :: [PlayerName]
tragedies =
  [ "Shade",
    "Igforce",
    "Mikhael",
    "exegesis1978",
    "Mikhael Kates",
    "Fluffy1",
    "rosenbud",
    "Arathaert",
    "Dimmadome ",
    "Mol ",
    " Jaratam",
    " Komap4uk",
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
    "The board is set ",
    "Vanali ",
    "Mistakentuna ",
    "Michel ",
    "JohnnyVictory",
    "Sharpz ",
    "Starlock",
    " Gileforn",
    "Kakashi",
    " Jyoung1234",
    "Guthix",
    "TheLegoQuill ",
    "TurtlePenguin9 ",
    "Barbarisco ",
    "Wems ",
    "Tom Bombadil",
    "Jorrick ",
    "DR Sigma"
  ]

banList :: PlayerBanList
banList = ["mordak", "mellowsedge"]

migrate :: [ParsedLadderEntry] -> [HashMap PlayerName PlayerId -> ParsedGameReport] -> SqlPersistT IO ()
migrate ladderEntries reports = do
  players <- traverse insertEntry ladderEntries
  traverse_ (insertGameReport . ($ fromList players)) reports

-- sadPlayers <- traverse (\player -> (T.toLower player,) <$> runSqlPool (insertPlayer (T.toLower player)) dbPool) tragedies

main :: IO ()
main = do
  createDirectoryIfMissing True . takeDirectory $ databaseFile

  ladderData <- readFileLBS "migration/ladder.csv"
  reportData <- readFileLBS "migration/reports.csv"

  let ladderEntries = case decode NoHeader ladderData of
        Left err -> error $ show err
        Right raw -> mapMaybe (toParsedLadderEntry banList) . V.toList $ raw

  let reports = case decode NoHeader reportData of
        Left err -> error $ show err
        Right raw -> V.toList $ fmap toParsedGameReport raw

  logger <- stdoutLogger
  dbPool <- runAppLogger logger $ createSqlitePoolWithConfig (toText databaseFile) defaultConnectionPoolConfig

  runSqlPool (runMigration migrateAll) dbPool
  runSqlPool (migrate ladderEntries reports) dbPool
