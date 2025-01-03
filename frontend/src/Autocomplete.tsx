import React, { ReactNode, useState } from "react";
import Alert from "@mui/joy/Alert";
import Box from "@mui/joy/Box";
import MaterialAutocomplete from "@mui/joy/Autocomplete";
import CircularProgress from "@mui/joy/CircularProgress";

interface Props {
    options: string[]; // could update to allow objects: https://mui.com/joy-ui/react-autocomplete/
    current: string;
    placeholder: string;
    loading: boolean;
    alertText?: string;
    onChange: (value: string) => void;
    validate: () => void;
}

export default function Autocomplete({
    options,
    current,
    placeholder,
    loading,
    alertText,
    onChange,
    validate,
}: Props) {
    const [displayedAlertText, setDisplayedAlertText] =
        useState<ReactNode>(null);

    return (
        <Box>
            {displayedAlertText && (
                <Alert color="warning" sx={{ mb: "10px" }}>
                    {displayedAlertText}
                </Alert>
            )}

            <MaterialAutocomplete
                clearOnEscape
                freeSolo
                openOnFocus
                options={options}
                inputValue={current}
                placeholder={placeholder}
                onInputChange={(_, value) => onChange(value)}
                onChange={(_, value) => {
                    if (value) setDisplayedAlertText(null);
                }}
                onBlur={() => {
                    validate();
                    setDisplayedAlertText(alertText);
                }}
                disabled={loading}
                startDecorator={
                    loading ? <CircularProgress size="sm" /> : undefined
                }
            />
        </Box>
    );
}
