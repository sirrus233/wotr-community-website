module Main where

import AppConfig (databaseFile, runAppLogger)
import Data.Csv (FromRecord, HasHeader (..), decode)
import Data.HashMap.Strict (lookup)
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian, midnight, timeOfDayToTime)
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
import Types.DataField (Competition (..), Expansion (..), League (..), Match (..), PlayerName, Side (..), Stronghold (..), Victory (..))
import Types.Database (GameReport (..), Player (..), PlayerId, PlayerStatsTotal (..), PlayerStatsYear (..), RatingDiff (..), migrateAll)

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

data GameReportWithTrash = GameReportWithTrash
  { gameId :: (),
    pairing :: (),
    timestamp :: Text,
    turns :: Int,
    winner :: Text,
    loser :: Text,
    expansions :: Text,
    typeOfVictory :: Text,
    competitive :: Text,
    cities :: Maybe Text,
    treebeard :: Maybe Text,
    fateOfErebor :: Maybe Text,
    tokens :: Text,
    dwarvenRings :: Text,
    corruption :: Int,
    mordor :: Maybe Int,
    step :: Int,
    aragorn :: Maybe Int,
    tree :: Maybe Int,
    eyes :: Text,
    rivendell :: Maybe Int,
    gH :: Maybe Int,
    shire :: Maybe Int,
    hD :: Maybe Int,
    edoras :: Maybe Int,
    lorien :: Maybe Int,
    wR :: Maybe Int,
    dale :: Maybe Int,
    erebor1 :: Maybe Int,
    mT :: Maybe Int,
    pelargir :: Maybe Int,
    dA :: Maybe Int,
    eredLuin :: Maybe Int,
    gameRating :: Text,
    gameComments :: Maybe Text,
    gameLog :: Maybe Text,
    dolGuldur :: Maybe Int,
    morannon :: Maybe Int,
    orthanc :: Maybe Int,
    mtGundabad :: Maybe Int,
    angmar :: Maybe Int,
    moria :: Maybe Int,
    minasMorgul :: Maybe Int,
    baradDur :: Maybe Int,
    umbar :: Maybe Int,
    farHarad :: Maybe Int,
    southRhun :: Maybe Int,
    erebor :: Maybe Int,
    ironHills :: Maybe Int,
    winningSide :: Text,
    miniExpansions :: (),
    ringsTokens :: (),
    sPVP :: (),
    ladderFriendly :: (),
    tourLeague :: (),
    log :: (),
    victoryCheck :: (),
    dEWcaptured :: (),
    gondorCaptured :: (),
    rohanCaptured :: (),
    constants :: (),
    expPro :: (),
    expTop :: (),
    vetPro :: (),
    vetTop :: (),
    expVet :: (),
    expRookie :: (),
    rookieVet :: (),
    rookieTop :: (),
    vetGames :: (),
    vetWinner :: (),
    vetLoser :: (),
    expGames :: (),
    expWinner :: (),
    expLoser :: (),
    rookieGames :: (),
    rookieWinner :: (),
    rookieLoser :: (),
    top50Game :: (),
    top30Game :: (),
    top10Game :: (),
    winnerTop30 :: (),
    loserTop30 :: (),
    winnerTop10 :: (),
    loserTop10 :: (),
    winnerNumGames :: (),
    winnerRank :: (),
    winnerRatingBefore :: Double,
    winnerRatingAfter :: Double,
    loserNumGames :: (),
    loserRankBefore :: (),
    loserRatingBefore :: Double,
    loserRatingAfter :: Double
  }
  deriving (Generic, Show)

instance FromRecord GameReportWithTrash

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
toLadderEntry (LadderEntryWithTrash {..}) =
  LadderEntry
    { player = T.toLower player,
      ..
    }

data ParsedGameReport = ParsedGameReport
  { gameReportTimestamp :: UTCTime,
    gameReportWinnerId :: PlayerId,
    gameReportLoserId :: PlayerId,
    gameReportSide :: Side,
    gameReportVictory :: Victory,
    gameReportMatch :: Match,
    gameReportCompetition :: [Competition],
    gameReportLeague :: Maybe League,
    gameReportExpansions :: [Expansion],
    gameReportTreebeard :: Maybe Bool,
    gameReportActionTokens :: Int,
    gameReportDwarvenRings :: Int,
    gameReportTurns :: Int,
    gameReportCorruption :: Int,
    gameReportMordor :: Maybe Int,
    gameReportInitialEyes :: Int,
    gameReportAragornTurn :: Maybe Int,
    gameReportStrongholds :: [Stronghold],
    gameReportInterestRating :: Int,
    gameReportComments :: Maybe Text,
    winnerRatingBefore :: Int,
    winnerRatingAfter :: Int,
    loserRatingBefore :: Int,
    loserRatingAfter :: Int
  }

toGameReport :: HashMap PlayerName PlayerId -> GameReportWithTrash -> ParsedGameReport
toGameReport playersByName report =
  ParsedGameReport
    { gameReportTimestamp,
      gameReportWinnerId,
      gameReportLoserId,
      gameReportSide,
      gameReportVictory,
      gameReportMatch,
      gameReportCompetition,
      gameReportLeague,
      gameReportExpansions,
      gameReportTreebeard,
      gameReportActionTokens,
      gameReportDwarvenRings,
      gameReportTurns,
      gameReportCorruption,
      gameReportMordor,
      gameReportInitialEyes,
      gameReportAragornTurn,
      gameReportStrongholds,
      gameReportInterestRating,
      gameReportComments,
      winnerRatingBefore,
      winnerRatingAfter,
      loserRatingBefore,
      loserRatingAfter
    }
  where
    gameReportTimestamp = case T.splitOn "/" (report.timestamp) of
      [d, m, y] ->
        UTCTime
          (fromGregorian (readOrError y) (readOrError m) (readOrError d))
          (timeOfDayToTime midnight)
      _ -> error $ "Invalid timestamp: " <> report.timestamp
      where
        readOrError :: (Read a, Num a) => Text -> a
        readOrError t = case readMaybe . toString $ t of
          Just a -> a
          Nothing -> error $ "Invalid timestamp number: " <> t
    gameReportWinnerId = case lookup (T.toLower report.winner) playersByName of
      Just a -> a
      Nothing -> error $ "Invalid winner: " <> report.winner
    gameReportLoserId = case lookup (T.toLower report.loser) playersByName of
      Just a -> a
      Nothing -> error $ "Invalid loser: " <> report.loser
    gameReportSide = case report.winningSide of
      "FP" -> Free
      "SP" -> Shadow
      _ -> error $ "Invalid side: " <> report.winningSide
    gameReportVictory = case report.typeOfVictory of
      "Free People Ring" -> Ring
      "Free People Military" -> Military
      "Shadow Forces Corruption" -> Ring
      "Shadow Forces Military" -> Military
      "Conceded FP won" -> Concession
      "Conceded SP won" -> Concession
      _ -> error $ "Invalid victory: " <> report.typeOfVictory
    gameReportMatch = case report.competitive of
      "Ladder" -> Ranked
      "ladder" -> Ranked
      "Friendly" -> Unranked
      "friendly" -> Unranked
      "Ladder and tournament" -> Ranked
      "Ladder and league (wome)" -> Ranked
      "Ladder and league (lome)" -> Ranked
      "Ladder and league (general)" -> Ranked
      "Ladder and league (super)" -> Ranked
      "Ladder and league (TTS)" -> Ranked
      "ladder and league (wome) 2023" -> Ranked
      "ladder and league (general) 2023" -> Ranked
      "ladder and league (lome) 2023 2023" -> Ranked
      "ladder and league (TTS) 2023 2023" -> Ranked
      _ -> error $ "Invalid match: " <> report.competitive
    gameReportCompetition = case report.competitive of
      "Ladder" -> []
      "ladder" -> []
      "Friendly" -> []
      "friendly" -> []
      "Ladder and tournament" -> [Tournament]
      "Ladder and league (wome)" -> [League]
      "Ladder and league (lome)" -> [League]
      "Ladder and league (general)" -> [League]
      "Ladder and league (super)" -> [League]
      "Ladder and league (TTS)" -> [League]
      "ladder and league (wome) 2023" -> [League]
      "ladder and league (general) 2023" -> [League]
      "ladder and league (lome) 2023 2023" -> [League]
      "ladder and league (TTS) 2023 2023" -> [League]
      _ -> error $ "Invalid competition: " <> report.competitive
    gameReportLeague = case report.competitive of
      "Ladder" -> Nothing
      "ladder" -> Nothing
      "Friendly" -> Nothing
      "friendly" -> Nothing
      "Ladder and tournament" -> Nothing
      "Ladder and league (wome)" -> Just WoMELeague
      "Ladder and league (lome)" -> Just LoMELeague
      "Ladder and league (general)" -> Just GeneralLeague
      "Ladder and league (super)" -> Just SuperLeague
      "Ladder and league (TTS)" -> Just TTSLeague
      "ladder and league (wome) 2023" -> Just WoMELeague
      "ladder and league (general) 2023" -> Just GeneralLeague
      "ladder and league (lome) 2023 2023" -> Just LoMELeague
      "ladder and league (TTS) 2023 2023" -> Just TTSLeague
      _ -> error $ "Invalid league: " <> report.competitive
    gameReportExpansions = expansions <> cities <> treebeard <> fateOfErebor
      where
        expansions = case report.expansions of
          "LoME" -> [LoME]
          "Lome" -> [LoME]
          "Base" -> []
          "LoME+WoME" -> [LoME, WoME]
          "Lome+Wome" -> [LoME, WoME]
          "WoME" -> [WoME]
          "KoME" -> [KoME]
          "KoME+LoME+WoME" -> [LoME, WoME, KoME]
          "KoME+LoME" -> [LoME, KoME]
          _ -> error $ "Invalid expansions: " <> report.expansions
        cities = usedMiniExpansion report.cities Cities
        treebeard = usedMiniExpansion report.treebeard Treebeard
        fateOfErebor = usedMiniExpansion report.fateOfErebor FateOfErebor
        usedMiniExpansion expansionStr expansion = case expansionStr of
          Just "Yes" -> [expansion]
          Just "No" -> []
          Nothing -> []
          _ -> error $ "Invalid expansion for (" <> show expansionStr <> "): " <> show expansion
    gameReportTreebeard = case (report.treebeard, report.tree) of
      (Just "Yes", Nothing) -> Just False
      (Just "Yes", Just 1) -> Just True
      (Just "No", _) -> Nothing
      (Nothing, _) -> Nothing
      _ -> error $ "Invalid treebeard: " <> show (report.treebeard, report.tree)
    gameReportActionTokens = case report.tokens of
      "Nope!" -> 0
      "One Action Token" -> 1
      "Two Action Tokens" -> 2
      "Three Action Tokens" -> 3
      "Four Action Tokens" -> 4
      "5+" -> 5
      _ -> error $ "Invalid action tokens: " <> show report.tokens
    gameReportDwarvenRings = case report.dwarvenRings of
      "Nope!" -> 0
      "One Dwarven Ring" -> 1
      "Two Dwarven Rings" -> 2
      "Two Dwarven Ring" -> 2
      "Three Dwarven Rings" -> 3
      "Four Dwarven Rings" -> 4
      "5+" -> 5
      _ -> error $ "Invalid dwarven rings: " <> show report.dwarvenRings
    gameReportTurns = report.turns
    gameReportCorruption = report.corruption
    gameReportMordor = case report.step of
      0 -> Nothing
      a -> Just a
    gameReportInitialEyes = fromMaybe 4 (readMaybe . toString $ report.gameRating)
    gameReportAragornTurn = case report.aragorn of
      Nothing -> Nothing
      Just 0 -> Nothing
      Just a -> Just a
    gameReportStrongholds =
      concat
        [ toStronghold report.rivendell Rivendell,
          toStronghold report.gH GreyHavens,
          toStronghold report.shire Shire,
          toStronghold report.hD HelmsDeep,
          toStronghold report.edoras Edoras,
          toStronghold report.lorien Lorien,
          toStronghold report.wR WoodlandRealm,
          toStronghold report.dale Dale,
          toStronghold report.erebor1 Erebor,
          toStronghold report.mT MinasTirith,
          toStronghold report.pelargir Pelargir,
          toStronghold report.dA DolAmroth,
          toStronghold report.eredLuin EredLuin,
          toStronghold report.dolGuldur DolGuldur,
          toStronghold report.morannon Morannon,
          toStronghold report.orthanc Orthanc,
          toStronghold report.mtGundabad MountGundabad,
          toStronghold report.angmar Angmar,
          toStronghold report.moria Moria,
          toStronghold report.minasMorgul MinasMorgul,
          toStronghold report.baradDur BaradDur,
          toStronghold report.umbar Umbar,
          toStronghold report.farHarad FarHarad,
          toStronghold report.southRhun SouthRhun,
          toStronghold report.erebor Erebor,
          toStronghold report.ironHills IronHills
        ]
      where
        toStronghold strongholdField stronghold = [stronghold | strongholdField == Just 1]
    gameReportInterestRating = fromMaybe (-1) (readMaybe . toString $ report.gameRating)
    gameReportComments = report.gameComments
    winnerRatingBefore = round report.winnerRatingBefore
    winnerRatingAfter = round report.winnerRatingAfter
    loserRatingBefore = round report.loserRatingBefore
    loserRatingAfter = round report.loserRatingAfter

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