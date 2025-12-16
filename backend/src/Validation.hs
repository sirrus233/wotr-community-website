module Validation where

import AppConfig (AppM)
import Control.Monad.Logger (logErrorN)
import Data.Text qualified as T
import Data.Text.Encoding (decodeLatin1)
import Data.Validation (Validation (..))
import Servant (ServerError (..), err422, throwError)
import Types.Api (RawGameReport (..))
import Types.DataField (Competition (..), Expansion (..), League (..), Side (..), Stronghold (..), Victory (..))

data ReportError
  = VictoryConditionConflictSPRV
  | VictoryConditionConflictFPRV
  | VictoryConditionConflictSPMV
  | VictoryConditionConflictFPMV
  | VictoryConditionConflictConcession
  | NoVictoryConditionMet
  | InvalidSPMV
  | InvalidFPMV
  | InvalidSPRV
  | InvalidFPRV
  | CompetitionMismatch
  | LeagueExpansionMismatch
  | LeagueOutOfSeason
  | TreebeardExpansionMismatch
  | TurnsOutOfRange
  | CorruptionOutOfRange
  | MordorOutOfRange
  | InitialEyesOutOfRange
  | InterestRatingOutOfRange
  | InvalidStronghold
  deriving (Show)

vpValue :: Stronghold -> Int
vpValue Rivendell = 2
vpValue GreyHavens = 2
vpValue WoodlandRealm = 2
vpValue Lorien = 2
vpValue HelmsDeep = 2
vpValue Edoras = 1
vpValue MinasTirith = 2
vpValue DolAmroth = 2
vpValue Pelargir = 1
vpValue Shire = 1
vpValue Dale = 1
vpValue Erebor = 2
vpValue EredLuin = 1
vpValue IronHills = 1
vpValue MountGundabad = 2
vpValue Angmar = 1
vpValue Moria = 2
vpValue DolGuldur = 2
vpValue Orthanc = 2
vpValue Morannon = 2
vpValue BaradDur = 2
vpValue MinasMorgul = 2
vpValue Umbar = 2
vpValue FarHarad = 1
vpValue SouthRhun = 1

strongholdSide :: [Expansion] -> Stronghold -> Side
strongholdSide expansions stronghold
  | FateOfErebor `elem` expansions && stronghold == Erebor = Shadow
  | otherwise = case stronghold of
      Rivendell -> Free
      GreyHavens -> Free
      WoodlandRealm -> Free
      Lorien -> Free
      HelmsDeep -> Free
      Edoras -> Free
      MinasTirith -> Free
      DolAmroth -> Free
      Pelargir -> Free
      Shire -> Free
      Dale -> Free
      Erebor -> Free
      EredLuin -> Free
      IronHills -> Free
      MountGundabad -> Shadow
      Angmar -> Shadow
      Moria -> Shadow
      DolGuldur -> Shadow
      Orthanc -> Shadow
      Morannon -> Shadow
      BaradDur -> Shadow
      MinasMorgul -> Shadow
      Umbar -> Shadow
      FarHarad -> Shadow
      SouthRhun -> Shadow

victoryPoints :: RawGameReport -> Side -> Int
victoryPoints (RawGameReport {strongholds, expansions}) side =
  sum . map vpValue . filter ((== opponent) . strongholdSide expansions) $ strongholds
  where
    opponent = case side of Free -> Shadow; Shadow -> Free

validateVictory :: RawGameReport -> Validation [ReportError] RawGameReport
validateVictory report
  | Just victory /= correctVictory = case correctVictory of
      Just (Shadow, Ring) -> Failure [VictoryConditionConflictSPRV]
      Just (Free, Ring) -> Failure [VictoryConditionConflictFPRV]
      Just (Shadow, Military) -> Failure [VictoryConditionConflictSPMV]
      Just (Free, Military) -> Failure [VictoryConditionConflictFPMV]
      Just (_, Concession) -> Failure [VictoryConditionConflictConcession]
      Nothing -> Failure [NoVictoryConditionMet]
  | spmv && victoryPoints report Shadow < 10 = Failure [InvalidSPMV]
  | fpmv && victoryPoints report Free < 4 = Failure [InvalidFPMV]
  | sprv && report.corruption < 12 = Failure [InvalidSPRV]
  | fprv && report.mordor /= Just 5 = Failure [InvalidFPRV]
  | otherwise = Success report
  where
    sprv = report.side == Shadow && report.victory == Ring
    fprv = report.side == Free && report.victory == Ring
    spmv = report.side == Shadow && report.victory == Military
    fpmv = report.side == Free && report.victory == Military
    victory = (report.side, report.victory)
    correctVictory
      | report.side == Free && report.victory == Concession = Just (Free, Concession)
      | report.side == Shadow && report.victory == Concession = Just (Shadow, Concession)
      | report.corruption >= 12 = Just (Shadow, Ring)
      | report.mordor == Just 5 = Just (Free, Ring)
      | victoryPoints report Shadow >= 10 = Just (Shadow, Military)
      | victoryPoints report Free >= 4 = Just (Free, Military)
      | otherwise = Nothing

validateCompetition :: RawGameReport -> Validation [ReportError] RawGameReport
validateCompetition report
  | isJust report.league == League `elem` report.competition = Success report
  | otherwise = Failure [CompetitionMismatch]

validateLeague :: RawGameReport -> Validation [ReportError] RawGameReport
validateLeague report = case report.league of
  Nothing -> Success report
  Just TTSLeague -> Success report
  Just GeneralLeague -> Success report
  Just LoMELeague | LoME `elem` report.expansions -> Success report
  Just WoMELeague | WoME `elem` report.expansions -> Success report
  Just SuperLeague | LoME `elem` report.expansions && WoME `elem` report.expansions -> Success report
  _ -> Failure [LeagueExpansionMismatch]

validateLeague' :: RawGameReport -> Validation [ReportError] RawGameReport
validateLeague' report = case report.league of
  Nothing -> Success report
  _ -> Failure [LeagueOutOfSeason]

validateTreebeard :: RawGameReport -> Validation [ReportError] RawGameReport
validateTreebeard report
  | isJust report.treebeard == Treebeard `elem` report.expansions = Success report
  | otherwise = Failure [TreebeardExpansionMismatch]

validateTurns :: RawGameReport -> Validation [ReportError] RawGameReport
validateTurns report
  | report.turns >= 1 = Success report
  | otherwise = Failure [TurnsOutOfRange]

validateCorruption :: RawGameReport -> Validation [ReportError] RawGameReport
validateCorruption report
  | report.corruption >= 0 = Success report
  | otherwise = Failure [CorruptionOutOfRange]

validateMordor :: RawGameReport -> Validation [ReportError] RawGameReport
validateMordor report = case report.mordor of
  Nothing -> Success report
  Just step | step >= 0 && step <= 5 -> Success report
  _ -> Failure [MordorOutOfRange]

validateInitialEyes :: RawGameReport -> Validation [ReportError] RawGameReport
validateInitialEyes report
  | report.initialEyes >= 0 && report.initialEyes <= 7 = Success report
  | otherwise = Failure [InitialEyesOutOfRange]

validateInterestRating :: RawGameReport -> Validation [ReportError] RawGameReport
validateInterestRating report
  | report.interestRating >= 1 && report.interestRating <= 10 = Success report
  | otherwise = Failure [InterestRatingOutOfRange]

validateStrongholds :: RawGameReport -> Validation [ReportError] RawGameReport
validateStrongholds report
  | EredLuin `elem` report.strongholds && Cities `notElem` report.expansions = Failure [InvalidStronghold]
  | IronHills `elem` report.strongholds && FateOfErebor `notElem` report.expansions = Failure [InvalidStronghold]
  | otherwise = Success report

validateReport :: RawGameReport -> Validation [ReportError] RawGameReport
validateReport report =
  validateVictory report
    <* validateCompetition report
    <* validateLeague' report
    <* validateTreebeard report
    <* validateTurns report
    <* validateCorruption report
    <* validateMordor report
    <* validateInitialEyes report
    <* validateInterestRating report
    <* validateStrongholds report

validateLogFile :: FilePath -> AppM ()
validateLogFile fp = do
  logFileLines <- take headerSearchSpan . lines . decodeLatin1 <$> readFileBS fp
  unless (any (logFileHeader `T.isPrefixOf`) logFileLines) $ do
    logErrorN "Log file missing header."
    throwError $ err422 {errBody = "Invalid log file."}
  where
    headerSearchSpan = 5
    logFileHeader = "<auto> silent null"
