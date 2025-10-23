import { Expansion, League, LeagueTier, Side, Stronghold } from "./types";

export function strongholdSide(
    expansions: Expansion[],
    stronghold: Stronghold
): Side {
    switch (stronghold) {
        case "Erebor":
            return expansions.includes("FateOfErebor") ? "Shadow" : "Free";
        case "Shire":
        case "Edoras":
        case "Dale":
        case "Pelargir":
        case "EredLuin":
        case "IronHills":
        case "Rivendell":
        case "GreyHavens":
        case "HelmsDeep":
        case "Lorien":
        case "WoodlandRealm":
        case "MinasTirith":
        case "DolAmroth":
            return "Free";
        case "Angmar":
        case "FarHarad":
        case "SouthRhun":
        case "MountGundabad":
        case "Moria":
        case "DolGuldur":
        case "Orthanc":
        case "Morannon":
        case "BaradDur":
        case "MinasMorgul":
        case "Umbar":
            return "Shadow";
    }
}

export function getStrongholdLabel(stronghold: Stronghold): string {
    switch (stronghold) {
        case "Rivendell":
        case "Erebor":
        case "Shire":
        case "Edoras":
        case "Dale":
        case "Pelargir":
        case "Angmar":
        case "Moria":
        case "Orthanc":
        case "Morannon":
        case "Umbar":
            return stronghold;
        case "Lorien":
            return "Lórien";
        case "GreyHavens":
            return "Grey Havens";
        case "HelmsDeep":
            return "Helm's Deep";
        case "WoodlandRealm":
            return "Woodland Realm";
        case "MinasTirith":
            return "Minas Tirith";
        case "DolAmroth":
            return "Dol Amroth";
        case "EredLuin":
            return "Ered Luin";
        case "IronHills":
            return "Iron Hills";
        case "MountGundabad":
            return "Mount Gundabad";
        case "DolGuldur":
            return "Dol Guldur";
        case "BaradDur":
            return "Barad-dûr";
        case "MinasMorgul":
            return "Minas Morgul";
        case "FarHarad":
            return "Far Harad";
        case "SouthRhun":
            return "South Rhûn";
    }
}

export function getStrongholdAbbreviation(stronghold: Stronghold): string {
    switch (stronghold) {
        case "Angmar":
            return "A";
        case "BaradDur":
            return "BD";
        case "Edoras":
            return "Ed";
        case "Erebor":
            return "E";
        case "EredLuin":
            return "EL";
        case "Dale":
            return "D";
        case "DolAmroth":
            return "DA";
        case "DolGuldur":
            return "DG";
        case "FarHarad":
            return "FH";
        case "GreyHavens":
            return "GH";
        case "HelmsDeep":
            return "HD";
        case "IronHills":
            return "IH";
        case "Lorien":
            return "L";
        case "MinasMorgul":
            return "MM";
        case "MinasTirith":
            return "MT";
        case "Morannon":
            return "Morannon";
        case "Moria":
            return "Moria";
        case "MountGundabad":
            return "MG";
        case "Orthanc":
            return "O";
        case "Pelargir":
            return "P";
        case "Rivendell":
            return "R";
        case "Shire":
            return "Sh";
        case "SouthRhun":
            return "SR";
        case "Umbar":
            return "Um";
        case "WoodlandRealm":
            return "W";
    }
}

export function getExpansionLabel(expansion: Expansion): string {
    switch (expansion) {
        case "LoME":
        case "WoME":
        case "KoME":
        case "Cities":
        case "Treebeard":
            return expansion;
        case "FateOfErebor":
            return "Fate of Erebor";
    }
}

export function getLeagueLabel(league: League): string {
    switch (league) {
        case "GeneralLeague":
            return "General";
        case "LoMELeague":
            return "LoME";
        case "WoMELeague":
            return "WoME";
        case "SuperLeague":
            return "Super";
        case "TTSLeague":
            return "TTS";
    }
}

export function getLeagueTierLabel(tier: LeagueTier): string {
    switch (tier) {
        case "Tier1":
            return "Elven";
        case "Tier2":
            return "Dwarf";
        case "Tier3":
            return "Hobbit";
    }
}

export function strongholdPoints(stronghold: Stronghold): 1 | 2 {
    switch (stronghold) {
        case "Shire":
        case "Edoras":
        case "Dale":
        case "Pelargir":
        case "EredLuin":
        case "IronHills":
        case "Angmar":
        case "FarHarad":
        case "SouthRhun":
            return 1;
        case "Rivendell":
        case "GreyHavens":
        case "HelmsDeep":
        case "Lorien":
        case "WoodlandRealm":
        case "MinasTirith":
        case "DolAmroth":
        case "Erebor":
        case "MountGundabad":
        case "Moria":
        case "DolGuldur":
        case "Orthanc":
        case "Morannon":
        case "BaradDur":
        case "MinasMorgul":
        case "Umbar":
            return 2;
    }
}

export function isStrongholdInPlay(
    expansions: Expansion[],
    stronghold: Stronghold
): boolean {
    switch (stronghold) {
        case "EredLuin":
            return expansions.includes("Cities");
        case "SouthRhun":
            return expansions.includes("Cities");
        case "IronHills":
            return expansions.includes("FateOfErebor");
        case "Shire":
        case "Edoras":
        case "Dale":
        case "Pelargir":
        case "Rivendell":
        case "GreyHavens":
        case "HelmsDeep":
        case "Lorien":
        case "WoodlandRealm":
        case "MinasTirith":
        case "DolAmroth":
        case "Erebor":
        case "Angmar":
        case "FarHarad":
        case "MountGundabad":
        case "Moria":
        case "DolGuldur":
        case "Orthanc":
        case "Morannon":
        case "BaradDur":
        case "MinasMorgul":
        case "Umbar":
            return true;
    }
}

export function displayTime(timestamp: string) {
    return Intl.DateTimeFormat("en-GB").format(new Date(Date.parse(timestamp)));
}

export function objectKeys<T extends object>(obj: T): Array<keyof T> {
    return Object.keys(obj) as Array<keyof T>;
}

export function range(start: number = 0, end: number): number[] {
    return end > start
        ? [...Array(end - start).keys()].map((i) => i + start)
        : [];
}

export function toPercent(num: number) {
    return Math.round(num * 100) + "%";
}

export function noNansense(num: number) {
    return isNaN(num) ? 0 : num;
}

export function isDefined<T>(value: T): value is Exclude<T, null | undefined> {
    return value !== null && value !== undefined;
}

export function sum(numbers: number[]) {
    return numbers.reduce((sum, num) => sum + num, 0);
}

export function fallback<T>(value: T, fallback: T) {
    return isDefined(value) ? value : fallback;
}

export function hasKey<K extends PropertyKey>(
    obj: object,
    key: K
): obj is Record<K, unknown> {
    return key in obj;
}
