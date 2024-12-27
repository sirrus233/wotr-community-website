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

export const shadowCities = ["Angmar", "FarHarad", "SouthRhun"] as const;

export const staticFreeCities = [
    "Shire",
    "Edoras",
    "Dale",
    "Pelargir",
] as const;

export const conditionalFreeCities = ["EredLuin", "IronHills"] as const;

export const staticFreeStrongholds = [
    ...staticFreeCities,
    "Rivendell",
    "GreyHavens",
    "HelmsDeep",
    "Lorien",
    "WoodlandRealm",
    "MinasTirith",
    "DolAmroth",
] as const;

export const conditionalFreeStrongholds = [
    ...conditionalFreeCities,
    "Erebor",
] as const;

export const shadowStrongholds = [
    ...shadowCities,
    "MountGundabad",
    "Moria",
    "DolGuldur",
    "Orthanc",
    "Morannon",
    "BaradDur",
    "MinasMorgul",
    "Umbar",
] as const;

export const strongholds = [
    ...shadowStrongholds,
    ...staticFreeStrongholds,
    ...conditionalFreeStrongholds,
] as const;

export const cities: (typeof strongholds)[number][] = [
    ...shadowCities,
    ...staticFreeCities,
    ...conditionalFreeCities,
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

export enum ErrorMessage {
    Required = "Required",
    OnSubmit = "Could not submit, please resolve errors",
}

export const INFINITE = 100;
