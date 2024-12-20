import axios from "axios";
import { useEffect, useState } from "react";
import { FieldError, FormData, ValueOf } from "../../types";
import { ErrorMessage } from "../../constants";
import initialFormData from "./initialFormData";

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
                    "http://localhost:8081/submitReport",
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
        formData.match.value,
        "competition",
        (match) => match === "Ranked"
    );
    useControlledClearEffect(
        formData.match.value,
        "league",
        (match) => match === "Ranked"
    );
    useControlledClearEffect(
        formData.competition.value,
        "league",
        (competition) => competition.includes("League")
    );
    useControlledClearEffect(
        formData.expansions.value,
        "treebeard",
        (expansions) => expansions.includes("Treebeard")
    );
    useControlledClearEffect(formData.usedExpansions.value, "expansions");
    useControlledClearEffect(formData.usedExpansions.value, "treebeard");
    useControlledClearEffect(
        formData.expansions.value,
        "treebeard",
        (expansions) => expansions.includes("Treebeard")
    );
    useControlledClearEffect(formData.usedHandicap.value, "actionTokens");
    useControlledClearEffect(formData.usedHandicap.value, "dwarvenRings");
    useControlledClearEffect(formData.didFellowshipReachMordor.value, "mordor");
    useControlledClearEffect(formData.wasAragornCrowned.value, "aragornTurn");

    return [
        formData,
        { errorOnSubmit },
        { handleInputChange, validateField, validateForm, handleSubmit },
    ];
};

export default useFormData;

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
