module Migration.Types where

import AppServer (normalizeName)
import Data.Csv (FromRecord)
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian, midnight, timeOfDayToTime)
import Relude.Extra (lookup)
import Types.DataField (Competition (..), Expansion (..), League (..), Match (..), PlayerName, Side (..), Stronghold (..), Victory (..))
import Types.Database (PlayerId)

type PlayerBanList = [PlayerName]

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

data ParsedLadderEntry = ParsedLadderEntry
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

toParsedLadderEntry :: PlayerBanList -> LadderEntryWithTrash -> Maybe ParsedLadderEntry
toParsedLadderEntry banList (LadderEntryWithTrash {..}) =
  if player `elem` banList
    then Nothing
    else Just ParsedLadderEntry {player = normalizeName player, ..}

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

toParsedGameReport :: GameReportWithTrash -> HashMap PlayerName PlayerId -> ParsedGameReport
toParsedGameReport report playersByName =
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
    gameReportWinnerId = case lookup (normalizeName report.winner) playersByName of
      Just a -> a
      Nothing -> error $ "Invalid winner: " <> report.winner
    gameReportLoserId = case lookup (normalizeName report.loser) playersByName of
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
      "Friendly" -> Unranked
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
      "Friendly" -> []
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
      "Friendly" -> Nothing
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
          "Base" -> []
          "LoME+WoME" -> [LoME, WoME]
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
