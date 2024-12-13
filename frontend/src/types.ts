import {
    competitiveType,
    expansions,
    leagues,
    matchType,
    sides,
    strongholds,
    victoryTypes,
} from "./constants";

export type Side = (typeof sides)[number];

export type Victory = (typeof victoryTypes)[number];

export type Expansion = (typeof expansions)[number];

export type Match = (typeof matchType)[number];

export type Competitive = (typeof competitiveType)[number];

export type League = (typeof leagues)[number];

export type Stronghold = (typeof strongholds)[number];

export type FieldError = string | null;

export interface FieldData<T> {
    value: T;
    error: FieldError;
    validate: () => FieldError;
}

export interface FormData {
    winner: FieldData<string | null>;
    loser: FieldData<string | null>;
    side: FieldData<Side | null>;
    victoryType: FieldData<Victory | null>;
    matchType: FieldData<Match | null>;
    competitionTypes: FieldData<Competitive[]>;
    league: FieldData<League | null>;
    usedExpansions: FieldData<boolean | null>;
    expansions: FieldData<Expansion[]>;
    wasTreebeardMustered: FieldData<boolean | null>;
    usedHandicap: FieldData<boolean | null>;
    actionTokens: FieldData<number | null>;
    dwarvenRings: FieldData<number | null>;
    gameTurns: FieldData<number | null>;
    corruption: FieldData<number | null>;
    didFellowshipReachMordor: FieldData<boolean | null>;
    mordorTrack: FieldData<number | null>;
    initialEyes: FieldData<number | null>;
    wasAragornCrowned: FieldData<boolean | null>;
    aragornCrownedTurn: FieldData<number | null>;
    capturedStrongholds: FieldData<Stronghold[]>;
    interestRating: FieldData<number | null>;
    comment: FieldData<string | null>;
}

export interface GameReportPayload {
    winner: string;
    loser: string;
    side: Side;
    victoryType: Victory;
    matchType: Match;
    competitionTypes: Competitive[];
    league: League | null;
    expansions: Expansion[];
    wasTreebeardMustered: boolean | null;
    actionTokens: number | null;
    dwarvenRings: number | null;
    gameTurns: number;
    corruption: number;
    mordorTrack: number | null;
    initialEyes: number;
    aragornCrownedTurn: number | null;
    capturedStrongholds: Stronghold[];
    interestRating: number;
    comment: string | null;
}

export type ValueOf<T> = T[keyof T];
