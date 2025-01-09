import { useEffect } from "react";
import { GameFormData, Stronghold, ValueOf } from "../types";
import { strongholds } from "../constants";
import { isStrongholdInPlay } from "../utils";

interface Args {
    formData: GameFormData;
    initialFormData: GameFormData;
    setFormData: React.Dispatch<React.SetStateAction<GameFormData>>;
    successMessage: string | null;
}

export default function useGameReportClearEffects({
    formData,
    initialFormData,
    setFormData,
    successMessage,
}: Args) {
    const useControlledClearEffect = <
        K extends keyof GameFormData,
        V extends ValueOf<GameFormData>["value"]
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

    useEffect(
        function resetForm() {
            if (successMessage) setFormData(initialFormData);
        },
        [successMessage]
    );

    useControlledClearEffect(
        formData.match.value,
        "competition",
        (match) => match === "Rated"
    );
    useControlledClearEffect(
        formData.match.value,
        "league",
        (match) => match === "Rated"
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
}

function isStrongholdConditional(stronghold: Stronghold): boolean {
    return !isStrongholdInPlay([], stronghold);
}
