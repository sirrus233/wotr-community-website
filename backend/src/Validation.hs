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
vpValue DolGoldur = 2
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
      DolGoldur -> Shadow
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
  | report.corruption >= 12 && not sprv = Failure [VictoryConditionConflictSPRV]
  | report.mordor == Just 5 && not fprv = Failure [VictoryConditionConflictFPRV]
  | victoryPoints report Shadow >= 10 && not spmv = Failure [VictoryConditionConflictSPMV]
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
