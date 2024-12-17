import React from "react";
import { FormControl, FormHelperText } from "@mui/joy";
import Sheet from "@mui/joy/Sheet";
import FormLabel from "@mui/joy/FormLabel";
import { FieldError } from "./types";

interface GameReportElementProps {
    children: React.ReactNode;
    label: string;
    error?: FieldError;
}

export default function GameReportFormElement({
    children,
    label,
    error,
}: GameReportElementProps) {
    return (
        <Sheet variant="outlined" sx={{ p: 2, borderRadius: "lg" }}>
            <FormControl error={!!error}>
                <FormLabel sx={{ fontSize: 16, pb: 2 }}>{label}</FormLabel>
                {children}
                {error && <FormHelperText>{error}</FormHelperText>}
            </FormControl>
        </Sheet>
    );
}
