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
    actionTokens: FieldData<number>;
    dwarvenRings: FieldData<number>;
    gameTurns: FieldData<number>;
    corruption: FieldData<number>;
    didFellowshipReachMordor: FieldData<boolean | null>;
    mordorTrack: FieldData<number>;
    initialEyes: FieldData<number>;
    wasAragornCrowned: FieldData<boolean | null>;
    aragornCrownedTurn: FieldData<number>;
    capturedStrongholds: FieldData<Stronghold[]>;
    interestRating: FieldData<number>;
    comment: FieldData<string | null>;
}

export type ValueOf<T> = T[keyof T];
