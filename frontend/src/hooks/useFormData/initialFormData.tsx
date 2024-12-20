import { ErrorMessage } from "../../constants";
import { FieldError, FormData } from "../../types";

const initialFormData: FormData = {
    winner: {
        value: null,
        error: null,
        validate: function _() {
            return detectMissingInput(this.value);
        },
    },
    loser: {
        value: null,
        error: null,
        validate: function _() {
            return detectMissingInput(this.value);
        },
    },
    side: {
        value: null,
        error: null,
        validate: function _() {
            return detectMissingInput(this.value);
        },
    },
    victory: {
        value: null,
        error: null,
        validate: function _() {
            return detectMissingInput(this.value);
        },
    },
    match: {
        value: null,
        error: null,
        validate: function _() {
            return detectMissingInput(this.value);
        },
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
        validate: function _() {
            return detectMissingInput(this.value);
        },
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
        validate: function _() {
            return detectMissingInput(this.value);
        },
    },
    actionTokens: { value: 0, error: null, validate: alwaysValid },
    dwarvenRings: { value: 0, error: null, validate: alwaysValid },
    turns: {
        value: null,
        error: null,
        validate: function _() {
            return detectMissingInput(this.value);
        },
    },
    corruption: {
        value: null,
        error: null,
        validate: function _() {
            return detectMissingInput(this.value);
        },
    },
    didFellowshipReachMordor: {
        value: null,
        error: null,
        validate: function _() {
            return detectMissingInput(this.value);
        },
    },
    mordor: {
        value: null,
        error: null,
        validate: alwaysValid,
    },
    initialEyes: {
        value: null,
        error: null,
        validate: function _() {
            return detectMissingInput(this.value);
        },
    },
    wasAragornCrowned: {
        value: null,
        error: null,
        validate: function _() {
            return detectMissingInput(this.value);
        },
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
        validate: function _() {
            return detectMissingInput(this.value);
        },
    },
    comment: { value: null, error: null, validate: alwaysValid },
};

export default initialFormData;

function detectMissingInput(value: unknown): FieldError {
    return value !== null && value !== undefined && value !== ""
        ? null
        : ErrorMessage.Required;
}

function alwaysValid() {
    return null;
}
