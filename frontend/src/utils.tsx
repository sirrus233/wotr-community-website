import { ErrorMessage } from "./constants";
import {
    Expansion,
    League,
    Match,
    ServerErrorBody,
    Side,
    Stronghold,
} from "./types";

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

export function getMatchLabel(match: Match): string {
    switch (match) {
        case "Ranked":
            return "Rated";
        case "Unranked":
            return "Unrated";
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

export function isServerError(error: unknown): error is ServerErrorBody {
    return (
        typeof error === "object" &&
        error !== null &&
        "status" in error &&
        typeof error.status === "number" &&
        "response" in error &&
        typeof error.response === "object" &&
        error.response !== null &&
        "data" in error.response &&
        typeof error.response.data === "string"
    );
}

export function toErrorMessage(error: ServerErrorBody): string {
    if (error.status === 422) {
        return error.response.data;
    }
    return ErrorMessage.Default;
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
