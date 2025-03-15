import React, { useState } from "react";
import MaterialAutocomplete from "@mui/joy/Autocomplete";
import Box from "@mui/joy/Box";
import FormControl from "@mui/joy/FormControl";
import FormHelperText from "@mui/joy/FormHelperText";
import { ErrorMessage } from "./constants";
import { MenuOption } from "./types";

interface Props<O extends string | MenuOption> {
    options: O[];
    current: O[];
    placeholder: string;
    loading: boolean;
    width: number;
    errorMessage?: ErrorMessage;
    onChange: (value: O[]) => void;
}

export default function TableFilter<O extends string | MenuOption>({
    options,
    current,
    placeholder,
    loading,
    width,
    errorMessage,
    onChange,
}: Props<O>) {
    const [isFocused, setIsFocused] = useState(false);

    return (
        <th style={{ overflow: "visible" }}>
            <FormControl error={!!errorMessage}>
                {errorMessage && (
                    <FormHelperText sx={{ mb: "3px", fontSize: "inherit" }}>
                        {errorMessage}
                    </FormHelperText>
                )}

                <Box
                    sx={{
                        position: "relative",
                        display: "flex",
                        justifyContent: "center",
                        width: `${width}px`,
                        height: "2em",
                    }}
                >
                    <MaterialAutocomplete
                        multiple
                        clearOnBlur
                        disableCloseOnSelect
                        openOnFocus
                        size="sm"
                        limitTags={0}
                        placeholder={placeholder}
                        options={options}
                        value={current}
                        loading={loading}
                        onFocus={() => setIsFocused(true)}
                        onBlur={() => setIsFocused(false)}
                        onChange={(_, values) => onChange(values)}
                        sx={{
                            background: "white",
                            fontSize: "inherit",
                            minWidth: 0,
                            lineHeight: 0,
                            button: { minHeight: 0, height: "100%" },
                            ...(isFocused
                                ? {
                                      position: "absolute",
                                      width: `${width}px`,
                                      minHeight: "100%",
                                  }
                                : {
                                      minHeight: 0,
                                      height: "100%",
                                      maxHeight: "100%",
                                  }),
                        }}
                    />
                </Box>
            </FormControl>
        </th>
    );
}
