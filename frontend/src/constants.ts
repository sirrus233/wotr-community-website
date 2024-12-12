export const sides = ["Free", "Shadow"] as const;

export const victoryTypes = ["Ring", "Military", "Concession"] as const;

export const matchType = ["Ranked", "Unranked"] as const;

export const competitiveType = ["League", "Tournament"] as const;

export const leagues = ["General", "LoME", "WoME", "Super", "TTS"] as const;

export const expansions = [
    "LoME",
    "WoME",
    "KoME",
    "Cities",
    "Fate of Erebor",
    "Treebeard",
] as const;

export const strongholds = [
    "Rivendell",
    "Grey Havens",
    "Helm's Deep",
    "Lorien",
    "Woodland Realm",
    "Erebor",
    "Minas Tirith",
    "Dol Amroth",
    "Shire",
    "Edoras",
    "Dale",
    "Pelargir",
    "Ered Luin (Cities expansion only)",
    "Iron Hills (Fate of Erebor expansion only)",
] as const;

export const cities: (typeof strongholds)[number][] = [
    "Shire",
    "Edoras",
    "Dale",
    "Pelargir",
    "Ered Luin (Cities expansion only)",
    "Iron Hills (Fate of Erebor expansion only)",
];

export enum ErrorMessage {
    Required = "Required",
    OnSubmit = "Could not submit, please resolve errors",
}

export const INFINITE = 100;
