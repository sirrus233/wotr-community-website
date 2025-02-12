export const sides = ["Free", "Shadow"] as const;

export const victoryTypes = ["Ring", "Military", "Concession"] as const;

export const matchTypes = ["Rated", "Unrated"] as const;

export const competitionTypes = ["League", "Tournament"] as const;

export const expansionLeagues = [
    "LoMELeague",
    "WoMELeague",
    "SuperLeague",
    "TTSLeague",
] as const;

export const leagues = ["GeneralLeague", ...expansionLeagues] as const;

export const leagueTiers = ["Tier1", "Tier2", "Tier3"] as const;

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

export const optionalFormFields = ["rid", "timestamp", "logFile"] as const;

export const optionalPayloadFields = [
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

export const optionalFields = [
    ...optionalFormFields,
    ...optionalPayloadFields,
] as const;

export const payloadFields = [
    ...optionalPayloadFields,
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

export const playerStates = ["Active", "Inactive"] as const;

export enum ErrorMessage {
    Default = "Something went wrong. Please contact an admin for assistance.",
    NotAuthorized = "You cannot pass! If you're an administrator, your session may have expired. Please log in and try again.",
    NotAuthorizedStatus = "You cannot pass!",
    UnknownAuthStatus = "Sign-in unavailable",
    Required = "Required",
    OnSubmit = "Could not submit, please resolve errors",
    MissingPlayerName = "This player does not exist in the database. Unless it's a new player, please check the spelling.",
    ExistingPlayerRequired = "Must choose an existing player",
}

export const INFINITE = 100;
export const LEADERBOARD_START_YEAR = 2023;
export const LEAGUE_START_YEAR = 2025;
export const MAX_GAME_LOG_SIZE_MB = 1;
export const MAX_GAME_LOG_SIZE_BYTES = MAX_GAME_LOG_SIZE_MB * 1024 * 1024;

export const COUNTRY_FLAGS = {
    Argentina: "🇦🇷",
    Australia: "🇦🇺",
    Austria: "🇦🇹",
    Belgium: "🇧🇪",
    Bhutan: "🇧🇹",
    Bosnia: "🇧🇦",
    Brazil: "🇧🇷",
    Canada: "🇨🇦",
    "Czech Republic": "🇨🇿",
    Estonia: "🇪🇪",
    Finland: "🇫🇮",
    France: "🇫🇷",
    Germany: "🇩🇪",
    Greece: "🇬🇷",
    India: "🇮🇳",
    Italy: "🇮🇹",
    Japan: "🇯🇵",
    Mexico: "🇲🇽",
    Netherlands: "🇳🇱",
    Norway: "🇳🇴",
    Peru: "🇵🇪",
    Poland: "🇵🇱",
    Russia: "🇷🇺",
    Scotland: "🏴󠁧󠁢󠁳󠁣󠁴󠁿",
    Serbia: "🇷🇸",
    Slovakia: "🇸🇰",
    "South Africa": "🇿🇦",
    Spain: "🇪🇸",
    Sweden: "🇸🇪",
    Switzerland: "🇨🇭",
    Ukraine: "🇺🇦",
    "United Kingdom": "🇬🇧",
    "United States of America": "🇺🇸",
    Uruguay: "🇺🇾",
    Wales: "🏴󠁧󠁢󠁷󠁬󠁳󠁿",
};
