import React, { ReactNode, useState } from "react";
import Alert from "@mui/joy/Alert";
import Box from "@mui/joy/Box";
import MaterialAutocomplete from "@mui/joy/Autocomplete";
import CircularProgress from "@mui/joy/CircularProgress";
import { PlayerOption } from "./types";

interface Props<O extends string | PlayerOption> {
    options: O[];
    current: O | null;
    placeholder: string;
    loading: boolean;
    alertText?: string;
    onChange?: (value: O | null) => void;
    onInputValueChange?: (value: string) => void;
    validate: () => void;
}

export default function Autocomplete<O extends string | PlayerOption>({
    options,
    current,
    placeholder,
    loading,
    alertText,
    onChange = () => {},
    onInputValueChange = () => {},
    validate,
}: Props<O>) {
    const [localInputValue, setLocalInputValue] = useState("");
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
                inputValue={
                    typeof current === "string" ? current : localInputValue
                }
                value={current}
                placeholder={placeholder}
                onInputChange={(_, value) => {
                    onInputValueChange(value);
                    setLocalInputValue(value);

                    const selected = options.find(
                        (option) =>
                            option === value ||
                            (typeof option !== "string" &&
                                option.label === value)
                    );

                    onChange(selected || null);
                }}
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
