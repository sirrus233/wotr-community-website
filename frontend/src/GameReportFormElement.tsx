import React from "react";
import Box from "@mui/joy/Box";
import FormControl from "@mui/joy/FormControl";
import FormHelperText from "@mui/joy/FormHelperText";
import FormLabel from "@mui/joy/FormLabel";
import Sheet from "@mui/joy/Sheet";
import { useTheme } from "@mui/joy/styles";
import { FieldError } from "./types";

interface GameReportElementProps {
    children: React.ReactNode;
    label: string;
    error?: FieldError;
    hasSingleControl?: boolean;
}

export default function GameReportFormElement({
    children,
    label,
    error,
    hasSingleControl = true,
}: GameReportElementProps) {
    const theme = useTheme();

    const errorTextStyle = {
        color: theme.palette.danger.plainColor,
        marginTop: theme.spacing(1),
    };

    const formComponents = (
        <>
            <FormLabel sx={{ fontSize: 16, pb: 2 }}>{label}</FormLabel>
            {children}
            {error && (
                <FormHelperText sx={errorTextStyle}>{error}</FormHelperText>
            )}
        </>
    );

    return (
        <Sheet variant="outlined" sx={{ p: 2, borderRadius: "lg" }}>
            {hasSingleControl ? (
                <FormControl error={!!error}>{formComponents}</FormControl>
            ) : (
                <Box>{formComponents}</Box>
            )}
        </Sheet>
    );
}
