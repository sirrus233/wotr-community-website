import axios from "axios";
import React from "react";
import Box from "@mui/joy/Box";
import { ErrorMessage } from "./constants";
import { API_BASE_URL } from "./env";
import { toErrorMessage } from "./networkErrorHandlers";
import {
    League,
    LeaguePlayerFormData,
    LeagueTier,
    ValidLeaguePlayerFormData,
} from "./types";
import useConditionalActionEffect from "./hooks/useConditionalActionEffect";
import useFormData, { initializeToDefaults } from "./hooks/useFormData";
import { getLeagueLabel, getLeagueTierLabel } from "./utils";
import Autocomplete from "./Autocomplete";
import AdminFormLayout from "./AdminFormLayout";

interface Props {
    league: League;
    tier: LeagueTier;
    year: number;
    playerNames: string[];
    loadingPlayers: boolean;
    refresh: () => void;
}

export default function LeaguePlayerForm({
    league,
    tier,
    year,
    playerNames,
    loadingPlayers,
    refresh,
}: Props) {
    const initialFormData: LeaguePlayerFormData = {
        league: initializeToDefaults(league),
        tier: initializeToDefaults(tier),
        year: initializeToDefaults(year),
        playerName: initializeToDefaults(null),
    };

    const [
        formData,
        { errorOnSubmit, successMessage, submitting },
        { handleInputChange, validateField, handleSubmit },
    ] = useFormData<LeaguePlayerFormData, ValidLeaguePlayerFormData>({
        initialFormData,
        optionalFields: [],
        submit,
        toErrorMessage,
    });

    useConditionalActionEffect(!!successMessage, refresh);

    return (
        <AdminFormLayout
            header="Add League Player"
            handleSubmit={handleSubmit}
            submitting={submitting}
            errorOnSubmit={errorOnSubmit}
            successMessage={successMessage}
            shouldHideFormOnSuccess
            formElementsProps={[
                {
                    label: "Player",
                    error: formData.playerName.error,
                    element: (
                        <Autocomplete
                            options={playerNames}
                            current={formData.playerName.value || ""}
                            loading={loadingPlayers}
                            alertText={
                                !!formData.playerName.value &&
                                !playerNames.includes(formData.playerName.value)
                                    ? ErrorMessage.MissingPlayerName
                                    : ""
                            }
                            alertPosition="below"
                            placeholder="Player name"
                            onInputValueChange={handleInputChange("playerName")}
                            validate={validateField("playerName")}
                        />
                    ),
                },
            ]}
        >
            <AutoPopulatedField
                label="League:"
                value={getLeagueLabel(formData.league.value)}
            />

            {formData.league.value === "GeneralLeague" && (
                <AutoPopulatedField
                    label="Tier:"
                    value={getLeagueTierLabel(formData.tier.value)}
                />
            )}

            <AutoPopulatedField label="Year:" value={formData.year.value} />
        </AdminFormLayout>
    );
}

async function submit(validFormData: ValidLeaguePlayerFormData) {
    return await axios.post(`${API_BASE_URL}/addLeaguePlayer`, null, {
        headers: { "Content-Type": "application/json" },
        params: toQueryParams(validFormData),
    });
}

type LeaguePlayerQueryParams = {
    league: League;
    tier: LeagueTier;
    year: number;
    playerId: number | null;
    playerName: string | null;
};

function toQueryParams(
    formData: ValidLeaguePlayerFormData
): LeaguePlayerQueryParams {
    return {
        league: formData.league.value,
        tier: formData.tier.value,
        year: formData.year.value,
        playerId: null,
        playerName: formData.playerName.value,
    };
}

interface AutoPopulatedFieldProps {
    label: string;
    value: string | number;
}

function AutoPopulatedField({ label, value }: AutoPopulatedFieldProps) {
    return (
        <Box display="flex" flexDirection="row">
            {label}
            <Box ml="5px" fontWeight="bold">
                {value}
            </Box>
        </Box>
    );
}
