import axios from "axios";
import { useEffect, useState } from "react";
import {
    Expansion,
    FieldError,
    FormData,
    GameReportPayload,
    LeaderboardEntry,
    ServerErrorBody,
    ServerValidationError,
    Stronghold,
    SuccessMessage,
    ValidFormData,
    ValueOf,
} from "../../types";
import {
    ErrorMessage,
    optionalFields,
    serverValidationErrors,
    strongholds,
} from "../../constants";
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
    loading: boolean;
    playerNames: string[];
    loadingPlayers: boolean;
};

const useFormData = (): [FormData, Meta, Helpers] => {
    const [formData, setFormData] = useState(initialFormData);
    const [playerNames, setPlayerNames] = useState<string[]>([]);
    const [errorOnSubmit, setErrorOnSubmit] = useState<FieldError>(null);
    const [successMessage, setSuccessMessage] = useState<SuccessMessage>(null);
    const [loading, setLoading] = useState(false);
    const [loadingPlayers, setLoadingPlayers] = useState(false);

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
                setLoading(true);

                const response = await axios.post(
                    //"http://localhost:8081/submitReport",
                    "https://api.waroftheringcommunity.net:8080/submitReport",
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
            if (isServerError(error)) {
                setErrorOnSubmit(toErrorMessage(error));
            } else {
                setErrorOnSubmit("Something went wrong.");
            }
        }
        setLoading(false);
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

    useEffect(function loadPlayerNames() {
        setLoadingPlayers(true);

        axios
            .get(
                "https://api.waroftheringcommunity.net:8080/leaderboard",
                { params: { year: 2024 } } // :'D
            )
            .then((response) => {
                setPlayerNames(
                    (response.data.entries as LeaderboardEntry[]).map(
                        (entry) => entry.name
                    )
                );
            })
            .catch((error) => console.error(error))
            .finally(() => {
                setLoadingPlayers(false);
            });
    }, []);

    useEffect(
        function resetForm() {
            if (successMessage) setFormData(initialFormData);
        },
        [successMessage]
    );

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
        { errorOnSubmit, successMessage, loading, loadingPlayers, playerNames },
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

function isServerError(error: unknown): error is ServerErrorBody {
    return (
        typeof error === "object" &&
        error !== null &&
        "status" in error &&
        typeof error.status === "number" &&
        "response" in error &&
        typeof error.response === "object" &&
        error.response !== null &&
        "data" in error.response &&
        typeof error.response.data === "string"
    );
}

function toErrorMessage(error: ServerErrorBody): string {
    if (error.status === 422) {
        const parsedResult = parseValidationResult(error.response.data);
        if (parsedResult.length) {
            const errorMessage = parsedResult
                .map(validationErrorToMessage)
                .join(", ");
            return (
                errorMessage.slice(0, 1).toUpperCase() + errorMessage.slice(1)
            );
        }
    }
    return "Something went wrong.";
}

function parseValidationResult(
    serverValidationResult: string
): ServerValidationError[] {
    const parsedResult = serverValidationResult
        .slice(1, serverValidationResult.length - 1)
        .split(",");

    return parsedResult.every((error) =>
        serverValidationErrors.includes(error as ServerValidationError)
    )
        ? (parsedResult as ServerValidationError[])
        : [];
}

function validationErrorToMessage(
    validationError: ServerValidationError
): string {
    switch (validationError) {
        case "VictoryConditionConflictSPRV":
            return "conditions met for Shadow ring victory instead of selected victory type";
        case "VictoryConditionConflictFPRV":
            return "conditions met for Free Peoples ring victory instead of selected victory type";
        case "VictoryConditionConflictSPMV":
            return "conditions met for Shadow military victory instead of selected victory type";
        case "VictoryConditionConflictFPMV":
            return "conditions met for Free Peoples military victory instead of selected victory type";
        case "VictoryConditionConflictConcession":
            return "conditions met for Concession victory type instead of selected victory type";
        case "NoVictoryConditionMet":
            return "no victory conditions met";
        case "InvalidSPMV":
            return "invalid Shadow military victory";
        case "InvalidFPMV":
            return "invalid Free Peoples military victory";
        case "InvalidSPRV":
            return "invalid Shadow ring victory";
        case "InvalidFPRV":
            return "invalid Free Peoples ring victory";
        case "CompetitionMismatch":
            return "reported competition type does not match reported league";
        case "LeagueExpansionMismatch":
            return "reported league does not match reported expansions";
        case "TreebeardExpansionMismatch":
            return "reported Treebeard muster does not match reported expansions";
        case "TurnsOutOfRange":
            return "invalid ending game turn selection";
        case "CorruptionOutOfRange":
            return "invalid fellowship corruption selection";
        case "MordorOutOfRange":
            return "invalid Mordor track selection";
        case "InitialEyesOutOfRange":
            return "invalid number of eyes allocated by Shadow on turn 1";
        case "InterestRatingOutOfRange":
            return "invalid interest rating selection";
        case "InvalidStronghold":
            return "invalid stronghold selections for the indicated expansions";
    }
}
