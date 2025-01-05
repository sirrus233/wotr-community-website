import { FormData } from "../types";

const initialFormData: FormData = {
    winner: {
        value: null,
        error: null,
        validate: alwaysValid,
    },
    loser: {
        value: null,
        error: null,
        validate: alwaysValid,
    },
    side: {
        value: null,
        error: null,
        validate: alwaysValid,
    },
    victory: {
        value: null,
        error: null,
        validate: alwaysValid,
    },
    match: {
        value: null,
        error: null,
        validate: alwaysValid,
    },
    competition: {
        value: [],
        error: null,
        validate: alwaysValid,
    },
    league: {
        value: null,
        error: null,
        validate: alwaysValid,
    },
    usedExpansions: {
        value: null,
        error: null,
        validate: alwaysValid,
    },
    expansions: {
        value: [],
        error: null,
        validate: alwaysValid,
    },
    treebeard: {
        value: null,
        error: null,
        validate: alwaysValid,
    },
    usedHandicap: {
        value: null,
        error: null,
        validate: alwaysValid,
    },
    actionTokens: { value: 0, error: null, validate: alwaysValid },
    dwarvenRings: { value: 0, error: null, validate: alwaysValid },
    turns: {
        value: null,
        error: null,
        validate: alwaysValid,
    },
    corruption: {
        value: null,
        error: null,
        validate: alwaysValid,
    },
    didFellowshipReachMordor: {
        value: null,
        error: null,
        validate: alwaysValid,
    },
    mordor: {
        value: null,
        error: null,
        validate: alwaysValid,
    },
    initialEyes: {
        value: null,
        error: null,
        validate: alwaysValid,
    },
    wasAragornCrowned: {
        value: null,
        error: null,
        validate: alwaysValid,
    },
    aragornTurn: {
        value: null,
        error: null,
        validate: alwaysValid,
    },
    strongholds: {
        value: [],
        error: null,
        validate: alwaysValid,
    },
    interestRating: {
        value: null,
        error: null,
        validate: alwaysValid,
    },
    comment: { value: null, error: null, validate: alwaysValid },
};

export default initialFormData;

function alwaysValid() {
    return null;
}
