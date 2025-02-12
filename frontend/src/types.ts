import {
    competitionTypes,
    COUNTRY_FLAGS,
    expansions,
    leagues,
    leagueTiers,
    matchTypes,
    optionalFields,
    optionalPlayerEditFields,
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

export type LeagueTier = (typeof leagueTiers)[number];

export type Stronghold = (typeof strongholds)[number];

export type OptionalField = (typeof optionalFields)[number];

export type PayloadField = (typeof payloadFields)[number];

export type OptionalPlayerEditField = (typeof optionalPlayerEditFields)[number];

export type PlayerState = (typeof playerStates)[number];

export type Country = keyof typeof COUNTRY_FLAGS;

export type SuccessMessage = string | null;

export type UserInfo = {
    isAdmin: boolean;
};

export type ServerErrorBody = {
    message: string;
    status: number;
    config: unknown;
    response: {
        data: string;
        headers: unknown;
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
    country: Country | null;
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
    name: FieldData<string | null>;
    country: FieldData<Country | null>;
}

export type ValidPlayerEditFormData = {
    [K in keyof PlayerEditFormData]: {
        [J in keyof PlayerEditFormData[K]]: K extends OptionalPlayerEditField
            ? PlayerEditFormData[K][J]
            : Exclude<PlayerEditFormData[K][J], null>;
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

export type LeagueParams = {
    league: League;
    tier: LeagueTier;
    year: number;
};

export type LeagueStats = Record<string, LeaguePlayerStats>;

export type LeaguePlayerStats = {
    name: string;
    summary: {
        totalWins: number;
        totalGames: number;
        points: number;
    };
    gameStatsByOpponent: Record<string, LeagueGameStats>;
};

export type LeagueGameStats = {
    opponent: string;
    wins: number;
    losses: number;
};

export type LeaguePlayerFormData = {
    league: FieldData<League>;
    tier: FieldData<LeagueTier>;
    year: FieldData<number>;
    playerName: FieldData<string | null>;
};

export type ValidLeaguePlayerFormData = {
    [K in keyof LeaguePlayerFormData]: {
        [J in keyof LeaguePlayerFormData[K]]: Exclude<
            LeaguePlayerFormData[K][J],
            null
        >;
    };
};

export type ValueOf<T> = T[keyof T];
