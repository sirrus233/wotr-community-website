import axios from "axios";
import React from "react";
import { Country, PlayerEditFormData, ValidPlayerEditFormData } from "../types";
import AdminFormLayout from "./AdminFormLayout";
import TextInput from "./TextInput";
import useConditionalActionEffect from "../hooks/useConditionalActionEffect";
import useFormData, { initializeToDefaults } from "../hooks/useFormData";
import { toErrorMessage } from "../networkErrorHandlers";
import Autocomplete from "./Autocomplete";
import { COUNTRIES_DATA, optionalPlayerEditFields } from "../constants";
import { API_BASE_URL } from "../env";

interface Props {
    pid: number;
    name: string;
    country: Country | null;
    refresh: () => void;
}

export default function PlayerEditForm({ pid, name, country, refresh }: Props) {
    const initialFormData: PlayerEditFormData = {
        pid: initializeToDefaults(pid),
        country: initializeToDefaults(country),
        name: initializeToDefaults(name),
    };

    const [
        formData,
        { errorOnSubmit, successMessage, submitting },
        { handleInputChange, validateField, handleSubmit },
    ] = useFormData<PlayerEditFormData, ValidPlayerEditFormData>({
        initialFormData,
        optionalFields: optionalPlayerEditFields.slice(),
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
                    label: "Name",
                    error: formData.name.error,
                    element: (
                        <TextInput
                            value={formData.name.value || ""}
                            placeholder="Name"
                            onChange={handleInputChange("name")}
                            validate={validateField("name")}
                        />
                    ),
                },
                {
                    label: "Country",
                    error: formData.country.error,
                    element: (
                        <Autocomplete
                            options={Object.keys(COUNTRIES_DATA) as Country[]}
                            current={formData.country.value}
                            loading={false}
                            placeholder="Country"
                            onChange={handleInputChange("country")}
                            validate={validateField("country")}
                        />
                    ),
                },
            ]}
        />
    );
}

async function submit(validFormData: ValidPlayerEditFormData) {
    return await axios.post(
        `${API_BASE_URL}/editPlayer`,
        toPayload(validFormData),
        {
            headers: { "Content-Type": "application/json" },
        }
    );
}

type PlayerRenamePayload = {
    pid: number;
    name: string;
    country: Country | null;
};

function toPayload(formData: ValidPlayerEditFormData): PlayerRenamePayload {
    return {
        pid: formData.pid.value,
        name: formData.name.value,
        country: formData.country.value,
    };
}
