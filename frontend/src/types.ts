import {
    competitionTypes,
    expansions,
    leagues,
    matchTypes,
    optionalFields,
    payloadFields,
    playerStates,
    serverValidationErrors,
    sides,
    strongholds,
    victoryTypes,
} from "./constants";

export type Side = (typeof sides)[number];

export type Victory = (typeof victoryTypes)[number];

export type Expansion = (typeof expansions)[number];

export type Match = (typeof matchTypes)[number];

export type Competition = (typeof competitionTypes)[number];

export type League = (typeof leagues)[number];

export type Stronghold = (typeof strongholds)[number];

export type OptionalField = (typeof optionalFields)[number];

export type PayloadField = (typeof payloadFields)[number];

export type PlayerState = (typeof playerStates)[number];

export type SuccessMessage = string | null;

export type ServerErrorBody = {
    message: string;
    status: number;
    response: {
        data: string;
    };
};

export type ServerValidationError = (typeof serverValidationErrors)[number];

export type FieldError = string | null;

export interface FieldData<T> {
    value: T;
    error: FieldError;
    validate: () => FieldError;
}

export type ConstrainedFormData<F> = {
    [K in keyof F]: F[K] extends FieldData<unknown> ? F[K] : never;
};

export interface GameFormData {
    rid: FieldData<number | null>;
    timestamp: FieldData<string | null>;
    winner: FieldData<string | null>;
    loser: FieldData<string | null>;
    side: FieldData<Side | null>;
    victory: FieldData<Victory | null>;
    match: FieldData<Match | null>;
    competition: FieldData<Competition[]>;
    league: FieldData<League | null>;
    usedExpansions: FieldData<boolean | null>;
    expansions: FieldData<Expansion[]>;
    treebeard: FieldData<boolean | null>;
    usedHandicap: FieldData<boolean | null>;
    actionTokens: FieldData<number | null>;
    dwarvenRings: FieldData<number | null>;
    turns: FieldData<number | null>;
    corruption: FieldData<number | null>;
    didFellowshipReachMordor: FieldData<boolean | null>;
    mordor: FieldData<number | null>;
    initialEyes: FieldData<number | null>;
    wasAragornCrowned: FieldData<boolean | null>;
    aragornTurn: FieldData<number | null>;
    strongholds: FieldData<Stronghold[]>;
    interestRating: FieldData<number | null>;
    comment: FieldData<string | null>;
    logFile: FieldData<File | null>;
}

export type ValidGameFormData = {
    [K in keyof GameFormData]: K extends OptionalField
        ? GameFormData[K]
        : { [J in keyof GameFormData[K]]: Exclude<GameFormData[K][J], null> };
};

export type GameReportPayload = {
    rid: ValidGameFormData["rid"]["value"];
    timestamp: ValidGameFormData["timestamp"]["value"];
    logFile: ValidGameFormData["logFile"]["value"];
    report: {
        [K in keyof Pick<ValidGameFormData, PayloadField>]: Pick<
            ValidGameFormData,
            PayloadField
        >[K]["value"];
    };
};

export type ProcessedGameReport = GameReportPayload["report"] & {
    rid: number;
    timestamp: string;
    winnerId: number;
    loserId: number;
    logFile: string | null;
};

export interface LeaderboardEntry {
    pid: number;
    name: string;
    country: string | null;
    isActive: boolean;
    currentRatingFree: number;
    currentRatingShadow: number;
    averageRating: number;
    totalGames: number;
    totalWinsFree: number;
    totalWinsShadow: number;
    totalLossesFree: number;
    totalLossesShadow: number;
    totalWinRateFree: number;
    totalWinRateShadow: number;
    year: number;
    yearlyGames: number;
    yearlyWinsFree: number;
    yearlyWinsShadow: number;
    yearlyLossesFree: number;
    yearlyLossesShadow: number;
    yearlyWinRateFree: number;
    yearlyWinRateShadow: number;
}

export type PlayerEditMode = "edit" | "remap";

export type PlayerOption = {
    label: string;
    pid: number;
};

export interface PlayerEditFormData {
    pid: FieldData<number>;
    newName: FieldData<string | null>;
}

export type ValidPlayerEditFormData = {
    [K in keyof PlayerEditFormData]: {
        [J in keyof PlayerEditFormData[K]]: Exclude<
            PlayerEditFormData[K][J],
            null
        >;
    };
};

export interface PlayerRemapFormData {
    fromPlayer: FieldData<PlayerOption>;
    toPlayer: FieldData<PlayerOption | null>;
}

export type ValidPlayerRemapFormData = {
    [K in keyof PlayerRemapFormData]: {
        [J in keyof PlayerRemapFormData[K]]: Exclude<
            PlayerRemapFormData[K][J],
            null
        >;
    };
};

export type ReportEditMode = "edit" | "delete";

export interface ReportDeleteFormData {
    rid: FieldData<number>;
}

export type ValidReportDeleteFormData = {
    [K in keyof ReportDeleteFormData]: {
        [J in keyof ReportDeleteFormData[K]]: Exclude<
            ReportDeleteFormData[K][J],
            null
        >;
    };
};

export type ValueOf<T> = T[keyof T];
