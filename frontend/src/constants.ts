export const sides = ["Free", "Shadow"] as const;

export const victoryTypes = ["Ring", "Military", "Concession"] as const;

export const matchTypes = ["Ranked", "Unranked"] as const;

export const competitionTypes = ["League", "Tournament"] as const;

export const leagues = [
    "GeneralLeague",
    "LoMELeague",
    "WoMELeague",
    "SuperLeague",
    "TTSLeague",
] as const;

export const expansions = [
    "LoME",
    "WoME",
    "KoME",
    "Cities",
    "FateOfErebor",
    "Treebeard",
] as const;

export const strongholds = [
    "Rivendell",
    "GreyHavens",
    "HelmsDeep",
    "Lorien",
    "WoodlandRealm",
    "MinasTirith",
    "DolAmroth",
    "Shire",
    "Edoras",
    "Dale",
    "Pelargir",
    "EredLuin",
    "IronHills",
    "MountGundabad",
    "Moria",
    "DolGuldur",
    "Orthanc",
    "Morannon",
    "BaradDur",
    "MinasMorgul",
    "Umbar",
    "Angmar",
    "FarHarad",
    "SouthRhun",
    "Erebor",
] as const;

export const optionalFields = [
    "competition",
    "league",
    "expansions",
    "treebeard",
    "actionTokens",
    "dwarvenRings",
    "mordor",
    "aragornTurn",
    "strongholds",
    "comment",
] as const;

export const payloadFields = [
    ...optionalFields,
    "winner",
    "loser",
    "side",
    "victory",
    "match",
    "turns",
    "corruption",
    "initialEyes",
    "interestRating",
] as const;

export const serverValidationErrors = [
    "VictoryConditionConflictSPRV",
    "VictoryConditionConflictFPRV",
    "VictoryConditionConflictSPMV",
    "VictoryConditionConflictFPMV",
    "VictoryConditionConflictConcession",
    "NoVictoryConditionMet",
    "InvalidSPMV",
    "InvalidFPMV",
    "InvalidSPRV",
    "InvalidFPRV",
    "CompetitionMismatch",
    "LeagueExpansionMismatch",
    "TreebeardExpansionMismatch",
    "TurnsOutOfRange",
    "CorruptionOutOfRange",
    "MordorOutOfRange",
    "InitialEyesOutOfRange",
    "InterestRatingOutOfRange",
    "InvalidStronghold",
] as const;

export enum ErrorMessage {
    Required = "Required",
    OnSubmit = "Could not submit, please resolve errors",
    MissingPlayerName = "This player does not exist in the database. Unless it's a new player, please check the spelling.",
}

export const INFINITE = 100;
export const DEFAULT_YEAR = 2024;
export const availableYears = [DEFAULT_YEAR, 2023, 2022] as const;
