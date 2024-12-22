import {
    competitionTypes,
    expansions,
    leagues,
    matchTypes,
    optionalFields,
    payloadFields,
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

export type OptionalFields = (typeof optionalFields)[number];

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
    [K in keyof FormData]: K extends OptionalFields
        ? FormData[K]
        : { [J in keyof FormData[K]]: Exclude<FormData[K][J], null> };
};

export type GameReportPayload = {
    [K in keyof Pick<ValidFormData, (typeof payloadFields)[number]>]: Pick<
        ValidFormData,
        (typeof payloadFields)[number]
    >[K]["value"];
};

export type ValueOf<T> = T[keyof T];
