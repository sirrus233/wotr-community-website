import axios from "axios";
import React, { useState } from "react";
import Alert from "@mui/joy/Alert";
import {
    PlayerOption,
    PlayerRemapFormData,
    ValidPlayerRemapFormData,
} from "./types";
import AdminFormLayout from "./AdminFormLayout";
import Autocomplete from "./Autocomplete";
import useConditionalActionEffect from "./hooks/useConditionalActionEffect";
import useFormData, { initializeToDefaults } from "./hooks/useFormData";
import { ErrorMessage } from "./constants";
import { toErrorMessage } from "./networkErrorHandlers";

interface Props {
    pid: number;
    name: string;
    playerOptions: PlayerOption[];
    refresh: () => void;
}

export default function PlayerRemapForm({
    pid,
    name,
    playerOptions,
    refresh,
}: Props) {
    const initialFormData: PlayerRemapFormData = {
        fromPlayer: initializeToDefaults({ pid, label: name }),
        toPlayer: {
            value: null,
            error: null,
            validate: function _() {
                return this.value
                    ? pid === this.value.pid
                        ? "Cannot remap player to themselves"
                        : null
                    : null;
            },
        },
    };

    const [
        formData,
        { errorOnSubmit, successMessage, submitting },
        { handleInputChange, validateField, handleSubmit },
    ] = useFormData<PlayerRemapFormData, ValidPlayerRemapFormData>({
        initialFormData,
        optionalFields: [],
        missingFieldErrorMessage: ErrorMessage.ExistingPlayerRequired,
        submit,
        toErrorMessage,
    });

    const [warningAlert, setWarningAlert] = useState<string | null>(null);

    useConditionalActionEffect(!!successMessage, refresh);

    const buttonText = "Remap player";

    return (
        <>
            <AdminFormLayout
                header={name}
                submitting={submitting}
                errorOnSubmit={errorOnSubmit}
                buttonText={buttonText}
                successMessage={successMessage}
                shouldHideFormOnSuccess
                formElementsProps={[
                    {
                        label: "Reassign this player's games to:",
                        error: formData.toPlayer.error,
                        element: (
                            <Autocomplete
                                current={formData.toPlayer.value}
                                options={playerOptions}
                                placeholder="Player name"
                                loading={false}
                                onChange={handleInputChange("toPlayer")}
                                validate={validateField("toPlayer")}
                            />
                        ),
                    },
                ]}
                handleSubmit={() => {
                    if (warningAlert || formData.toPlayer.error) {
                        handleSubmit();
                    } else {
                        setWarningAlert(
                            `Danger: ${name} will cease to exist. ${formData.toPlayer.value?.label} will absorb all of ${name}'s history. ${name}'s history cannot be recovered. If you're sure, press "${buttonText}" again to continue.`
                        );
                    }
                }}
            />

            {!successMessage && warningAlert && (
                <Alert color="warning" sx={{ my: "10px" }}>
                    {warningAlert}
                </Alert>
            )}
        </>
    );
}

async function submit(validFormData: ValidPlayerRemapFormData) {
    return await axios.post(
        "https://api.waroftheringcommunity.net:8080/remapPlayer",
        // "http://localhost:8081/remapPlayer",
        toPayload(validFormData),
        {
            headers: { "Content-Type": "application/json" },
        }
    );
}

type PlayerRemapPayload = {
    fromPid: number;
    toPid: number;
};

function toPayload(
    validFormData: ValidPlayerRemapFormData
): PlayerRemapPayload {
    return {
        fromPid: validFormData.fromPlayer.value.pid,
        toPid: validFormData.toPlayer.value.pid,
    };
}
