import axios from "axios";
import { useEffect, useState } from "react";
import {
    Expansion,
    FieldError,
    FormData,
    GameReportPayload,
    Stronghold,
    SuccessMessage,
    ValidFormData,
    ValueOf,
} from "../../types";
import { ErrorMessage, optionalFields, strongholds } from "../../constants";
import initialFormData from "./initialFormData";

type Helpers = {
    handleInputChange: <K extends keyof FormData>(
        field: K
    ) => (value: FormData[K]["value"]) => void;
    validateField: <K extends keyof FormData>(field: K) => () => void;
    handleSubmit: () => Promise<void>;
    setSuccessMessage: (message: SuccessMessage) => void;
    isStrongholdInPlay: (
        expansions: Expansion[],
        stronghold: Stronghold
    ) => boolean;
};

type Meta = {
    errorOnSubmit: FieldError;
    successMessage: SuccessMessage;
};

const useFormData = (): [FormData, Meta, Helpers] => {
    const [formData, setFormData] = useState(initialFormData);
    const [errorOnSubmit, setErrorOnSubmit] = useState<FieldError>(null);
    const [successMessage, setSuccessMessage] = useState<SuccessMessage>(null);

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
                const error: FieldError =
                    prevData[field].validate() ||
                    (isFieldMissing(field, prevData) &&
                        ErrorMessage.Required) ||
                    null;

                return error || prevData[field].error
                    ? {
                          ...prevData,
                          [field]: { ...prevData[field], error },
                      }
                    : prevData;
            });
        };
    };

    const validateForm = (): ValidFormData | ErrorMessage.OnSubmit => {
        const stateUpdates = objectKeys(formData).reduce<(() => void)[]>(
            (updates, field) => {
                const error: FieldError =
                    formData[field].validate() ||
                    (isFieldMissing(field, formData) &&
                        ErrorMessage.Required) ||
                    null;

                if (error) {
                    updates.push(() =>
                        setFormData((prevData) => ({
                            ...prevData,
                            [field]: { ...prevData[field], error },
                        }))
                    );
                }

                return updates;
            },
            []
        );

        if (!stateUpdates.length) {
            return formData as ValidFormData;
        } else {
            stateUpdates.forEach((update) => update());
            return ErrorMessage.OnSubmit;
        }
    };

    const handleSubmit = async () => {
        try {
            const validatedResult = validateForm();

            if (validatedResult === ErrorMessage.OnSubmit) {
                setErrorOnSubmit(validatedResult);
            } else {
                setErrorOnSubmit(null);

                const response = await axios.post(
                    "http://localhost:8081/submitReport",
                    toPayload(validatedResult),
                    {
                        headers: {
                            "Content-Type": "application/json",
                        },
                    }
                );

                console.log("Form submitted successfully:", response);
                // Handle the response data as needed

                setSuccessMessage("Report submitted. Thank you!");
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

    useEffect(
        function resetForm() {
            if (successMessage) setFormData(initialFormData);
        },
        [successMessage]
    );
    const useStrongholdDeselectEffect = (
        deselectedStronghold: Stronghold,
        controlCondition: boolean
    ) => {
        useEffect(() => {
            if (!controlCondition) {
                setFormData((formData) => ({
                    ...formData,
                    strongholds: {
                        ...formData.strongholds,
                        value: formData.strongholds.value.filter(
                            (stronghold) => stronghold !== deselectedStronghold
                        ),
                    },
                }));
            }
        }, [controlCondition]);
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
    useControlledClearEffect(formData.usedHandicap.value, "actionTokens");
    useControlledClearEffect(formData.usedHandicap.value, "dwarvenRings");
    useControlledClearEffect(formData.didFellowshipReachMordor.value, "mordor");
    useControlledClearEffect(formData.wasAragornCrowned.value, "aragornTurn");

    strongholds.filter(isStrongholdConditional).map((stronghold) => {
        useStrongholdDeselectEffect(
            stronghold,
            isStrongholdInPlay(formData.expansions.value, stronghold)
        );
    });

    return [
        formData,
        { errorOnSubmit, successMessage },
        {
            handleInputChange,
            validateField,
            handleSubmit,
            isStrongholdInPlay,
            setSuccessMessage,
        },
    ];
};

export default useFormData;

function isFieldMissing(field: keyof FormData, formData: FormData) {
    const { value } = formData[field];
    const isRequired = optionalFields.every((opF) => opF !== field);
    const isEmpty =
        value === null ||
        value === undefined ||
        value === "" ||
        (Array.isArray(value) && !value.length);
    return isEmpty && isRequired;
}

function toPayload(formData: ValidFormData): GameReportPayload {
    return {
        winner: formData.winner.value,
        loser: formData.loser.value,
        side: formData.side.value,
        victory: formData.victory.value,
        match: formData.match.value,
        competition: formData.competition.value,
        league: formData.league.value,
        expansions: formData.expansions.value,
        treebeard: formData.treebeard.value,
        actionTokens: formData.actionTokens.value,
        dwarvenRings: formData.dwarvenRings.value,
        turns: formData.turns.value,
        corruption: formData.corruption.value,
        mordor: formData.mordor.value,
        initialEyes: formData.initialEyes.value,
        aragornTurn: formData.aragornTurn.value,
        strongholds: formData.strongholds.value,
        interestRating: formData.interestRating.value,
        comment: formData.comment.value,
    };
}

function isStrongholdInPlay(
    expansions: Expansion[],
    stronghold: Stronghold
): boolean {
    switch (stronghold) {
        case "EredLuin":
            return expansions.includes("Cities");
        case "SouthRhun":
            return expansions.includes("Cities");
        case "IronHills":
            return expansions.includes("FateOfErebor");
        case "Shire":
        case "Edoras":
        case "Dale":
        case "Pelargir":
        case "Rivendell":
        case "GreyHavens":
        case "HelmsDeep":
        case "Lorien":
        case "WoodlandRealm":
        case "MinasTirith":
        case "DolAmroth":
        case "Erebor":
        case "Angmar":
        case "FarHarad":
        case "MountGundabad":
        case "Moria":
        case "DolGuldur":
        case "Orthanc":
        case "Morannon":
        case "BaradDur":
        case "MinasMorgul":
        case "Umbar":
            return true;
    }
}

function isStrongholdConditional(stronghold: Stronghold): boolean {
    return !isStrongholdInPlay([], stronghold);
}

function objectKeys<T extends object>(obj: T): Array<keyof T> {
    return Object.keys(obj) as Array<keyof T>;
}
