module Main where

import AppConfig (databaseFile, runAppLogger)
import Data.Csv (HasHeader (..), decode)
import Data.Text qualified as T
import Data.Vector qualified as V
import Database.Esqueleto.Experimental (defaultConnectionPoolConfig, runMigration, runSqlPool)
import Database.Persist.Sqlite (createSqlitePoolWithConfig)
import Logging (stdoutLogger)
import Migration.Database (insertEntry, insertGameReport, insertPlayer)
import Migration.Types (GameReportWithTrash, PlayerBanList, toParsedGameReport, toParsedLadderEntry)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Types.DataField (PlayerName)
import Types.Database (migrateAll)

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

main :: IO ()
main = do
  createDirectoryIfMissing True . takeDirectory $ databaseFile

  ladderData <- readFileLBS "migration/ladder.csv"
  case decode NoHeader ladderData of
    Left err -> putStrLn err
    Right rawLadderEntries -> do
      logger <- stdoutLogger
      dbPool <- runAppLogger logger $ createSqlitePoolWithConfig (toText databaseFile) defaultConnectionPoolConfig
      runSqlPool (runMigration migrateAll) dbPool

      sadPlayers <- traverse (\player -> (T.toLower player,) <$> runSqlPool (insertPlayer (T.toLower player)) dbPool) tragedies

      let entries = mapMaybe (toParsedLadderEntry banList) . V.toList $ rawLadderEntries
      players <- traverse (\entry -> runSqlPool (insertEntry entry) dbPool) entries

      reportData <- readFileLBS "migration/reports.csv"
      case decode NoHeader reportData :: Either String (V.Vector GameReportWithTrash) of
        Left err -> putStrLn err
        Right rawGameReports -> do
          let reports = V.toList $ fmap (toParsedGameReport (fromList $ sadPlayers <> players)) rawGameReports
          traverse_ (\report -> runSqlPool (insertGameReport report) dbPool) reports
