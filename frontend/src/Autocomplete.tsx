import React, { ReactNode, useState } from "react";
import Alert from "@mui/joy/Alert";
import Box from "@mui/joy/Box";
import MaterialAutocomplete from "@mui/joy/Autocomplete";
import CircularProgress from "@mui/joy/CircularProgress";
import { MenuOption } from "./types";

interface Props<O extends string | MenuOption<number | string>> {
    options: O[];
    current: O | null;
    placeholder: string;
    loading: boolean;
    alertText?: string;
    alertPosition?: "above" | "below";
    onChange?: (value: O | null) => void;
    onInputValueChange?: (value: string) => void;
    validate: () => void;
}

export default function Autocomplete<
    O extends string | MenuOption<number | string>
>({
    options,
    current,
    placeholder,
    loading,
    alertText,
    alertPosition = "above",
    onChange = () => {},
    onInputValueChange = () => {},
    validate,
}: Props<O>) {
    const [localInputValue, setLocalInputValue] = useState("");
    const [displayedAlertText, setDisplayedAlertText] =
        useState<ReactNode>(null);

    const alert = (
        <Alert
            color="warning"
            sx={alertPosition === "above" ? { mb: "10px" } : { mt: "10px" }}
        >
            {displayedAlertText}
        </Alert>
    );

    return (
        <Box>
            {displayedAlertText && alertPosition === "above" && alert}

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
                    if (!current) setLocalInputValue("");
                }}
                disabled={loading}
                startDecorator={
                    loading ? <CircularProgress size="sm" /> : undefined
                }
            />

            {displayedAlertText && alertPosition === "below" && alert}
        </Box>
    );
}
