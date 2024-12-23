module Validation where

import Data.Validation (Validation (..))
import Types.Api (RawGameReport (..))
import Types.DataField (Competition (..), Expansion (..), League (..), Side (..), Stronghold (..), Victory (..))

data ReportError
  = VictoryConditionConflictSPRV
  | VictoryConditionConflictFPRV
  | VictoryConditionConflictSPMV
  | InvalidSPMV
  | InvalidFPMV
  | InvalidSPRV
  | InvalidFPRV
  | CompetitionMismatch
  | LeagueExpansionMismatch
  | TreebeardExpansionMismatch
  | TurnsOutOfRange
  | CorruptionOutOfRange
  | MordorOutOfRange
  | InitialEyesOutOfRange
  | InterestRatingOutOfRange
  | InvalidStronghold
  deriving (Show)

vpValue :: Side -> Stronghold -> Int
vpValue Shadow Rivendell = 2
vpValue Shadow GreyHavens = 2
vpValue Shadow WoodlandRealm = 2
vpValue Shadow Lorien = 2
vpValue Shadow HelmsDeep = 2
vpValue Shadow Edoras = 1
vpValue Shadow MinasTirith = 2
vpValue Shadow DolAmroth = 2
vpValue Shadow Pelargir = 1
vpValue Shadow Shire = 1
vpValue Shadow Dale = 1
vpValue Shadow Erebor = 2
vpValue Shadow EredLuin = 1
vpValue Shadow IronHills = 1
vpValue _ _ = 0

victoryPoints :: RawGameReport -> Int
victoryPoints report = sum . map (vpValue report.side) $ report.strongholds

validateVictory :: RawGameReport -> Validation [ReportError] RawGameReport
validateVictory report
  | report.corruption >= 12 && not sprv = Failure [VictoryConditionConflictSPRV]
  | report.mordor == Just 5 && not fprv = Failure [VictoryConditionConflictFPRV]
  | victoryPoints report >= 10 && not spmv = Failure [VictoryConditionConflictSPMV]
  | spmv && victoryPoints report < 10 = Failure [InvalidSPMV]
  | fpmv && victoryPoints report < 4 = Failure [InvalidFPMV]
  | sprv && report.corruption < 12 = Failure [InvalidSPRV]
  | fprv && report.mordor /= Just 5 = Failure [InvalidFPRV]
  | otherwise = Success report
  where
    sprv = report.side == Shadow && report.victory == Ring
    fprv = report.side == Free && report.victory == Ring
    spmv = report.side == Shadow && report.victory == Military
    fpmv = report.side == Free && report.victory == Military

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
    <* validateLeague report
    <* validateTreebeard report
    <* validateTurns report
    <* validateCorruption report
    <* validateMordor report
    <* validateInitialEyes report
    <* validateInterestRating report
    <* validateStrongholds report
