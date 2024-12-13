import axios from "axios";
import { useEffect, useState } from "react";
import { FieldError, FormData, ValueOf } from "../types";
import { ErrorMessage } from "../constants";

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
    victoryType: {
        value: null,
        error: null,
        validate: function _() {
            return detectMissingInput(this.value);
        },
    },
    matchType: {
        value: null,
        error: null,
        validate: function _() {
            return detectMissingInput(this.value);
        },
    },
    competitionTypes: {
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
    wasTreebeardMustered: {
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
    gameTurns: {
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
    mordorTrack: {
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
    aragornCrownedTurn: {
        value: null,
        error: null,
        validate: alwaysValid,
    },
    capturedStrongholds: {
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

type Helpers = {
    handleInputChange: <K extends keyof FormData>(
        field: K
    ) => (value: FormData[K]["value"]) => void;
    validateField: <K extends keyof FormData>(field: K) => () => void;
    validateForm: () => FieldError;
    handleSubmit: () => Promise<void>;
};

type Meta = {
    errorOnSubmit: FieldError;
};

const useFormData = (): [FormData, Meta, Helpers] => {
    const [formData, setFormData] = useState(initialFormData);
    const [errorOnSubmit, setErrorOnSubmit] = useState<FieldError>(null);

    const handleInputChange = <K extends keyof FormData>(field: K) => {
        return (value: FormData[K]["value"]) =>
            setFormData((prevData) => ({
                ...prevData,
                [field]: { ...prevData[field], value },
            }));
    };

    const validateField = <K extends keyof FormData>(field: K) => {
        return () => {
            setFormData((prevData) => {
                const error = prevData[field].validate();
                return error || prevData[field].error
                    ? {
                          ...prevData,
                          [field]: { ...prevData[field], error },
                      }
                    : prevData;
            });
        };
    };

    const validateForm = () => {
        const stateUpdates = objectKeys(formData).reduce<(() => void)[]>(
            (updates, field) => {
                const fieldError = formData[field].validate();
                if (fieldError) {
                    updates.push(() =>
                        setFormData((prevData) => ({
                            ...prevData,
                            [field]: { ...prevData[field], error: fieldError },
                        }))
                    );
                }
                return updates;
            },
            []
        );

        if (stateUpdates.length) {
            stateUpdates.forEach((update) => update());
            return ErrorMessage.OnSubmit;
        } else {
            return null;
        }
    };

    const handleSubmit = async () => {
        try {
            const formError = validateForm();

            if (formError) {
                setErrorOnSubmit(formError);
            } else {
                setErrorOnSubmit(null);

                const response = await axios.post(
                    "http://localhost:3001/submit-report",
                    toPayload(formData),
                    {
                        headers: {
                            "Content-Type": "application/json",
                        },
                    }
                );

                console.log("Form submitted successfully:", response);
                // Handle the response data as needed
            }
        } catch (error) {
            console.error("Error submitting form:", error);
        }
    };

    const useControlledClearEffect = <
        K extends keyof FormData,
        V extends ValueOf<FormData>["value"]
    >(
        controlField: V,
        clearField: K,
        controlCondition?: (value: V) => boolean
    ) => {
        const condition = controlCondition
            ? controlCondition(controlField)
            : controlField;
        useEffect(() => {
            if (!condition) {
                setFormData((formData) => ({
                    ...formData,
                    [clearField]: initialFormData[clearField],
                }));
            }
        }, [controlField]);
    };

    useControlledClearEffect(
        formData.matchType.value,
        "competitionTypes",
        (matchType) => matchType === "Ranked"
    );
    useControlledClearEffect(
        formData.matchType.value,
        "league",
        (matchType) => matchType === "Ranked"
    );
    useControlledClearEffect(
        formData.competitionTypes.value,
        "league",
        (competitionTypes) => competitionTypes.includes("League")
    );
    useControlledClearEffect(
        formData.expansions.value,
        "wasTreebeardMustered",
        (expansions) => expansions.includes("Treebeard")
    );
    useControlledClearEffect(formData.usedExpansions.value, "expansions");
    useControlledClearEffect(
        formData.usedExpansions.value,
        "wasTreebeardMustered"
    );
    useControlledClearEffect(
        formData.expansions.value,
        "wasTreebeardMustered",
        (expansions) => expansions.includes("Treebeard")
    );
    useControlledClearEffect(formData.usedHandicap.value, "actionTokens");
    useControlledClearEffect(formData.usedHandicap.value, "dwarvenRings");
    useControlledClearEffect(
        formData.didFellowshipReachMordor.value,
        "mordorTrack"
    );
    useControlledClearEffect(
        formData.wasAragornCrowned.value,
        "aragornCrownedTurn"
    );

    return [
        formData,
        { errorOnSubmit },
        { handleInputChange, validateField, validateForm, handleSubmit },
    ];
};

export default useFormData;

function detectMissingInput(value: unknown): FieldError {
    return value !== null && value !== undefined && value !== ""
        ? null
        : ErrorMessage.Required;
}

function alwaysValid() {
    return null;
}

function toPayload(formData: FormData) {
    return Object.fromEntries(
        Object.entries(formData).map(([field, fieldData]) => [
            field,
            fieldData.value,
        ])
    );
}

function objectKeys<T extends object>(obj: T): Array<keyof T> {
    return Object.keys(obj) as Array<keyof T>;
}
