import axios from "axios";
import React from "react";
import { PlayerEditFormData, ValidPlayerEditFormData } from "./types";
import AdminFormLayout from "./AdminFormLayout";
import TextInput from "./TextInput";
import useConditionalActionEffect from "./hooks/useConditionalActionEffect";
import useFormData from "./hooks/useFormData";
import { toErrorMessage } from "./utils";

interface Props {
    pid: number;
    name: string;
    refresh: () => void;
}

export default function PlayerEditForm({ pid, name, refresh }: Props) {
    const initialFormData: PlayerEditFormData = {
        pid: {
            value: pid,
            error: null,
            validate: () => null,
        },
        newName: {
            value: null,
            error: null,
            validate: function _() {
                return this.value?.toLowerCase().trim() ===
                    name.toLowerCase().trim()
                    ? "Cannot rename a player to the same name"
                    : null;
            },
        },
    };

    const [
        formData,
        { errorOnSubmit, successMessage, submitting },
        { handleInputChange, validateField, handleSubmit },
    ] = useFormData<PlayerEditFormData, ValidPlayerEditFormData>({
        initialFormData,
        optionalFields: [],
        submit,
        toErrorMessage,
    });

    useConditionalActionEffect(!!successMessage, refresh);

    return (
        <AdminFormLayout
            header={name}
            handleSubmit={handleSubmit}
            submitting={submitting}
            errorOnSubmit={errorOnSubmit}
            successMessage={successMessage}
            shouldHideFormOnSuccess
            formElementsProps={[
                {
                    label: "New player name",
                    error: formData.newName.error,
                    element: (
                        <TextInput
                            value={formData.newName.value || ""}
                            placeholder="New player name"
                            onChange={handleInputChange("newName")}
                            validate={validateField("newName")}
                        />
                    ),
                },
            ]}
        />
    );
}

async function submit(validFormData: ValidPlayerEditFormData) {
    return await axios.post(
        "https://api.waroftheringcommunity.net:8080/renamePlayer",
        // "http://localhost:8081/renamePlayer",
        toPayload(validFormData),
        {
            headers: { "Content-Type": "application/json" },
        }
    );
}

type PlayerRenamePayload = {
    pid: number;
    newName: string;
};

function toPayload(formData: ValidPlayerEditFormData): PlayerRenamePayload {
    return {
        pid: formData.pid.value,
        newName: formData.newName.value,
    };
}
