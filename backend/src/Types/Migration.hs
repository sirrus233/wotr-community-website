module Types.Migration where

import Amazonka qualified as AWS
import AppServer (toS3Key, toS3Url)
import Data.Csv (FromRecord)
import Data.Text qualified as T
import Data.Time (TimeOfDay (..), UTCTime (..), fromGregorian, timeOfDayToTime)
import Types.Api (RawGameReport (..))
import Types.DataField (Competition (..), Expansion (..), League (..), Match (..), PlayerName, Side (..), Stronghold (..), Victory (..))

type PlayerBanList = [PlayerName]

data LegacyLadderEntryWithTrash = LegacyLadderEntryWithTrash
  { rank :: (),
    country :: Maybe Text,
    player :: Text,
    averageRating :: (),
    shadowRating :: Int,
    freeRating :: Int,
    gamesPlayedTotal :: Double
  }
  deriving (Generic, Show)

instance FromRecord LegacyLadderEntryWithTrash

data ParsedLegacyLadderEntry = ParsedLegacyLadderEntry
  { player :: Text,
    country :: Maybe Text,
    shadowRating :: Int,
    freeRating :: Int,
    gamesPlayedTotal :: Int
  }

toParsedLegacyLadderEntry :: LegacyLadderEntryWithTrash -> ParsedLegacyLadderEntry
toParsedLegacyLadderEntry entry@(LegacyLadderEntryWithTrash {..}) =
  ParsedLegacyLadderEntry {gamesPlayedTotal = totalGames, ..}
  where
    totalGames = round entry.gamesPlayedTotal

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
    gameComment :: Maybe Text,
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
  { timestamp :: UTCTime,
    winner :: Text,
    loser :: Text,
    side :: Side,
    victory :: Victory,
    match :: Match,
    competition :: [Competition],
    league :: Maybe League,
    expansions :: [Expansion],
    treebeard :: Maybe Bool,
    actionTokens :: Int,
    dwarvenRings :: Int,
    turns :: Int,
    corruption :: Int,
    mordor :: Maybe Int,
    initialEyes :: Int,
    aragornTurn :: Maybe Int,
    strongholds :: [Stronghold],
    interestRating :: Int,
    comment :: Maybe Text,
    log :: Maybe Text
  }

toParsedGameReport :: AWS.Env -> GameReportWithTrash -> ParsedGameReport
toParsedGameReport awsEnv report =
  ParsedGameReport
    { timestamp,
      winner,
      loser,
      side,
      victory,
      match,
      competition,
      league,
      expansions,
      treebeard,
      actionTokens,
      dwarvenRings,
      turns,
      corruption,
      mordor,
      initialEyes,
      aragornTurn,
      strongholds,
      interestRating,
      comment,
      log
    }
  where
    timestamp = case T.splitOn " " (report.timestamp) of
      [date, time] -> case (T.splitOn "/" date, T.splitOn ":" time) of
        ([d, m, y], [h, minute, s]) ->
          UTCTime
            (fromGregorian (readOrError y) (readOrError m) (readOrError d))
            (timeOfDayToTime $ TimeOfDay (readOrError h) (readOrError minute) (readOrError s))
        _ -> error $ "Invalid timestamp: " <> report.timestamp
      _ -> error $ "Invalid timestamp: " <> report.timestamp
      where
        readOrError :: (Read a, Num a) => Text -> a
        readOrError t = case readMaybe . toString $ t of
          Just a -> a
          Nothing -> error $ "Invalid timestamp number: " <> t
    winner = report.winner
    loser = report.loser
    side = case report.winningSide of
      "FP" -> Free
      "SP" -> Shadow
      _ -> error $ "Invalid side: " <> report.winningSide
    victory = case report.typeOfVictory of
      "Free People Ring" -> Ring
      "Free People Military" -> Military
      "Shadow Forces Corruption" -> Ring
      "Shadow Forces Military" -> Military
      "Conceded FP won" -> Concession
      "Conceded SP won" -> Concession
      _ -> error $ "Invalid victory: " <> report.typeOfVictory
    match = case report.competitive of
      "Ladder" -> Rated
      "Friendly" -> Unrated
      "Ladder and tournament" -> Rated
      "Ladder and league (wome)" -> Rated
      "Ladder and league (lome)" -> Rated
      "Ladder and league (general)" -> Rated
      "Ladder and league (super)" -> Rated
      "Ladder and league (TTS)" -> Rated
      "ladder and league (wome) 2023" -> Rated
      "ladder and league (general) 2023" -> Rated
      "ladder and league (lome) 2023 2023" -> Rated
      "ladder and league (TTS) 2023 2023" -> Rated
      _ -> error $ "Invalid match: " <> report.competitive
    competition = case report.competitive of
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
    league = case report.competitive of
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
    expansions = mainExpansions <> cities <> treebeardExp <> fateOfErebor
      where
        mainExpansions = case report.expansions of
          "LoME" -> [LoME]
          "Base" -> []
          "LoME+WoME" -> [LoME, WoME]
          "WoME" -> [WoME]
          "KoME" -> [KoME]
          "KoME+LoME+WoME" -> [LoME, WoME, KoME]
          "KoME+LoME" -> [LoME, KoME]
          _ -> error $ "Invalid expansions: " <> report.expansions
        cities = usedMiniExpansion report.cities Cities
        treebeardExp = usedMiniExpansion report.treebeard Treebeard
        fateOfErebor = usedMiniExpansion report.fateOfErebor FateOfErebor
        usedMiniExpansion expansionStr expansion = case expansionStr of
          Just "Yes" -> [expansion]
          Just "No" -> []
          Nothing -> []
          _ -> error $ "Invalid expansion for (" <> show expansionStr <> "): " <> show expansion
    treebeard = case (report.treebeard, report.tree) of
      (Just "Yes", Nothing) -> Just False
      (Just "Yes", Just 1) -> Just True
      (Just "No", _) -> Nothing
      (Nothing, _) -> Nothing
      _ -> error $ "Invalid treebeard: " <> show (report.treebeard, report.tree)
    actionTokens = case report.tokens of
      "Nope!" -> 0
      "One Action Token" -> 1
      "Two Action Tokens" -> 2
      "Three Action Tokens" -> 3
      "Four Action Tokens" -> 4
      "5+" -> 5
      _ -> error $ "Invalid action tokens: " <> show report.tokens
    dwarvenRings = case report.dwarvenRings of
      "Nope!" -> 0
      "One Dwarven Ring" -> 1
      "Two Dwarven Rings" -> 2
      "Three Dwarven Rings" -> 3
      "Four Dwarven Rings" -> 4
      "5+" -> 5
      _ -> error $ "Invalid dwarven rings: " <> show report.dwarvenRings
    turns = if report.turns == 0 then 1 else report.turns
    corruption = report.corruption
    mordor = case report.step of
      0 -> Nothing
      a -> Just a
    initialEyes = fromMaybe 4 (readMaybe . toString $ report.eyes)
    aragornTurn = case report.aragorn of
      Nothing -> Nothing
      Just 0 -> Nothing
      Just a -> Just a
    strongholds =
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
    interestRating = if interest == 0 then 1 else interest
      where
        interest = fromMaybe 1 (readMaybe . toString $ report.gameRating)
    comment = report.gameComment
    log = toS3Url awsEnv.region (toS3Key timestamp freePlayer shadowPlayer) <$ report.gameLog
      where
        freePlayer = if side == Free then winner else loser
        shadowPlayer = if side == Shadow then winner else loser

toRawGameReport :: ParsedGameReport -> RawGameReport
toRawGameReport (ParsedGameReport {..}) = RawGameReport {..}
