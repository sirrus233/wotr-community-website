import axios from "axios";
import React, { useEffect, useState } from "react";
import Alert from "@mui/joy/Alert";
import Box from "@mui/joy/Box";
import Button from "@mui/joy/Button";
import CircularProgress from "@mui/joy/CircularProgress";
import FormControl from "@mui/joy/FormControl";
import FormHelperText from "@mui/joy/FormHelperText";
import FormLabel from "@mui/joy/FormLabel";
import Sheet from "@mui/joy/Sheet";
import Typography from "@mui/joy/Typography";
import {
    PlayerOption,
    PlayerRemapFormData,
    ValidPlayerRemapFormData,
} from "./types";
import Autocomplete from "./Autocomplete";
import useConditionalActionEffect from "./hooks/useConditionalActionEffect";
import useFormData from "./hooks/useFormData";
import { ErrorMessage } from "./constants";
import { toErrorMessage } from "./utils";

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
        fromPlayer: {
            value: { pid, label: name },
            error: null,
            validate: () => null,
        },
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
        { errorOnSubmit, successMessage, loading },
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

    return (
        <Sheet
            sx={{
                display: "flex",
                flexDirection: "column",
                alignItems: "center",
                justifyContent: "center",
            }}
        >
            {successMessage ? (
                <Typography color="success">{successMessage}</Typography>
            ) : (
                <>
                    <Typography level="title-lg">{name}</Typography>

                    <Box sx={{ my: 2, width: "100%" }}>
                        <FormControl error={!!formData.toPlayer.error}>
                            <FormLabel>
                                Reassign this player's games to:
                            </FormLabel>

                            <Autocomplete
                                current={formData.toPlayer.value}
                                options={playerOptions}
                                placeholder="Player name"
                                loading={false}
                                onChange={handleInputChange("toPlayer")}
                                validate={validateField("toPlayer")}
                            />

                            {formData.toPlayer.error && (
                                <FormHelperText>
                                    {formData.toPlayer.error}
                                </FormHelperText>
                            )}
                        </FormControl>

                        {warningAlert && (
                            <Alert color="warning" sx={{ my: "10px" }}>
                                {warningAlert}
                            </Alert>
                        )}
                    </Box>

                    <Button
                        onClick={() => {
                            if (warningAlert || formData.toPlayer.error) {
                                handleSubmit();
                            } else {
                                setWarningAlert(
                                    `Danger: ${name} will cease to exist. ${formData.toPlayer.value?.label} will absorb all of ${name}'s history. ${name}'s history cannot be recovered. If you're sure, press "Submit" again to continue.`
                                );
                            }
                        }}
                        disabled={loading}
                        startDecorator={
                            loading ? <CircularProgress /> : undefined
                        }
                    >
                        {loading ? "Submitting..." : "Submit"}
                    </Button>

                    {errorOnSubmit && (
                        <Typography color="danger" mt={1}>
                            {errorOnSubmit}
                        </Typography>
                    )}
                </>
            )}
        </Sheet>
    );
}

async function submit(validFormData: ValidPlayerRemapFormData) {
    return await axios.post(
        "https://api.waroftheringcommunity.net:8080/remapPlayer",
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
