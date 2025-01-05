import axios from "axios";
import React, { useEffect } from "react";
import Box from "@mui/joy/Box";
import Button from "@mui/joy/Button";
import CircularProgress from "@mui/joy/CircularProgress";
import FormControl from "@mui/joy/FormControl";
import FormHelperText from "@mui/joy/FormHelperText";
import FormLabel from "@mui/joy/FormLabel";
import Sheet from "@mui/joy/Sheet";
import Typography from "@mui/joy/Typography";
import {
    PlayerEditFormData,
    ServerErrorBody,
    ValidPlayerEditFormData,
} from "./types";
import TextInput from "./TextInput";
import useFormData from "./hooks/useFormData";

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
        { errorOnSubmit, successMessage, loading },
        { handleInputChange, validateField, handleSubmit },
    ] = useFormData<PlayerEditFormData, ValidPlayerEditFormData>({
        initialFormData,
        optionalFields: [],
        submit,
        toErrorMessage,
    });

    useEffect(
        function refreshOnSubmit() {
            if (successMessage) {
                refresh();
            }
        },
        [successMessage]
    );

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
                        <FormControl error={!!formData.newName.error}>
                            <FormLabel>New player name</FormLabel>

                            <TextInput
                                value={formData.newName.value || ""}
                                placeholder="New player name"
                                onChange={handleInputChange("newName")}
                                validate={validateField("newName")}
                            />

                            {formData.newName.error && (
                                <FormHelperText>
                                    {formData.newName.error}
                                </FormHelperText>
                            )}
                        </FormControl>
                    </Box>

                    <Button
                        onClick={handleSubmit}
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

async function submit(validFormData: ValidPlayerEditFormData) {
    return await axios.post(
        "https://api.waroftheringcommunity.net:8080/renamePlayer",
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

function toErrorMessage(error: ServerErrorBody): string {
    if (error.status === 422) {
        return error.response.data;
    }
    return "Something went wrong.";
}
