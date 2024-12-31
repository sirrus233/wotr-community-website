module Main where

import AppConfig (databaseFile, runAppLogger)
import Data.Csv (HasHeader (..), decode)
import Data.Text qualified as T
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
import Migration.Types
  ( GameReportWithTrash,
    LadderEntry (..),
    ParsedGameReport (..),
    toGameReport,
    toLadderEntry,
  )
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Types.DataField
  ( PlayerName,
    Side (..),
  )
import Types.Database
  ( GameReport (..),
    Player (..),
    PlayerStatsTotal (..),
    PlayerStatsYear (..),
    RatingDiff (..),
    migrateAll,
  )

insertPlayer :: (MonadIO m) => Text -> SqlPersistT m (Key Player)
insertPlayer name = insert $ Player name Nothing

insertStats :: (MonadIO m) => Key Player -> LadderEntry -> SqlPersistT m ()
insertStats playerId entry = do
  insert_ $ PlayerStatsTotal playerId entry.freeRating entry.shadowRating entry.gamesPlayedTotal
  insert_ $ PlayerStatsYear playerId 2024 entry.fpWins entry.spWins entry.fpLoss entry.spLoss

insertEntry :: (MonadIO m) => LadderEntry -> SqlPersistT m (PlayerName, Key Player)
insertEntry entry = do
  playerId <- insertPlayer entry.player
  insertStats playerId entry
  pure (entry.player, playerId)

insertGameReport :: (MonadIO m) => ParsedGameReport -> SqlPersistT m ()
insertGameReport (ParsedGameReport {..}) = do
  rid <- insert $ GameReport {..}
  let losingSide = case gameReportSide of
        Free -> Shadow
        Shadow -> Free
  insert_ $ RatingDiff gameReportTimestamp gameReportWinnerId rid gameReportSide winnerRatingBefore winnerRatingAfter
  insert_ $ RatingDiff gameReportTimestamp gameReportLoserId rid losingSide loserRatingBefore loserRatingAfter

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

main :: IO ()
main = do
  createDirectoryIfMissing True . takeDirectory $ databaseFile

  ladderData <- readFileLBS "/home/bradley/downloads/ladder.csv"
  case decode NoHeader ladderData of
    Left err -> putStrLn err
    Right rawLadderEntries -> do
      logger <- stdoutLogger
      dbPool <- runAppLogger logger $ createSqlitePoolWithConfig (toText databaseFile) defaultConnectionPoolConfig
      runSqlPool (runMigration migrateAll) dbPool

      sadPlayers <- traverse (\player -> (T.toLower player,) <$> runSqlPool (insertPlayer (T.toLower player)) dbPool) tragedies

      let entries = V.toList $ fmap toLadderEntry rawLadderEntries
      players <- traverse (\entry -> runSqlPool (insertEntry entry) dbPool) entries

      reportData <- readFileLBS "/home/bradley/downloads/reports.csv"
      case decode NoHeader reportData :: Either String (V.Vector GameReportWithTrash) of
        Left err -> putStrLn err
        Right rawGameReports -> do
          let reports = V.toList $ fmap (toGameReport (fromList $ sadPlayers <> players)) rawGameReports
          traverse_ (\report -> runSqlPool (insertGameReport report) dbPool) reports
