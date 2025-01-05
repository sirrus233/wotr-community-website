import {
    competitionTypes,
    expansions,
    leagues,
    matchTypes,
    optionalFields,
    payloadFields,
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

export interface FormData {
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
}

export type ValidFormData = {
    [K in keyof FormData]: K extends OptionalField
        ? FormData[K]
        : { [J in keyof FormData[K]]: Exclude<FormData[K][J], null> };
};

export type GameReportPayload = {
    [K in keyof Pick<ValidFormData, PayloadField>]: Pick<
        ValidFormData,
        PayloadField
    >[K]["value"];
};

export interface LeaderboardEntry {
    pid: number;
    name: string;
    country: string | null;
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

export interface ProcessedGameReport {
    rid: number;
    timestamp: string;
    winnerId: number;
    winner: string;
    loserId: number;
    loser: string;
    side: Side;
    victory: Victory;
    match: Match;
    competition: Competition[];
    league: League | null;
    expansions: Expansion[];
    treebeard: boolean | null;
    actionTokens: number;
    dwarvenRings: number;
    turns: number;
    corruption: number;
    mordor: number | null;
    initialEyes: number;
    aragornTurn: number | null;
    strongholds: Stronghold[];
    interestRating: number;
    comments: string | null;
}

export type ValueOf<T> = T[keyof T];
