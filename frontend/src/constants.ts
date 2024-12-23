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
    "Erebor",
    "MinasTirith",
    "DolAmroth",
    "Shire",
    "Edoras",
    "Dale",
    "Pelargir",
    "EredLuin",
    "IronHills",
] as const;

export const cities: (typeof strongholds)[number][] = [
    "Shire",
    "Edoras",
    "Dale",
    "Pelargir",
    "EredLuin",
    "IronHills",
];

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

export enum ErrorMessage {
    Required = "Required",
    OnSubmit = "Could not submit, please resolve errors",
}

export const INFINITE = 100;
