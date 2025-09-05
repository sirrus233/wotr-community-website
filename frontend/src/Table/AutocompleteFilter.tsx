import React, { useState } from "react";
import MaterialAutocomplete from "@mui/joy/Autocomplete";
import Box from "@mui/joy/Box";
import FormControl from "@mui/joy/FormControl";
import FormHelperText from "@mui/joy/FormHelperText";
import { FILTER_ERROR_HEIGHT, TABLE_FILTER_HEIGHT } from "./constants";
import { AutocompleteProps, Option } from "./types";

export default function AutocompleteFilter<O extends Option>({
    options,
    current,
    placeholder,
    loading,
    width,
    errorMessage,
    allOption,
    emptyOption,
    onChange,
}: AutocompleteProps<O> & { width: number }) {
    const [isFocused, setIsFocused] = useState(false);

    const paddedWidth = `calc(${width}px - 10px)`;

    const specialOptions = [allOption, emptyOption].filter(
        (o) => o !== undefined
    );

    const isAllOption = (option: Option) =>
        !!allOption && isOptionEqual(option, allOption);

    const isEmptyOption = (option: Option) =>
        !!emptyOption && isOptionEqual(option, emptyOption);

    const isSpecialOption = (option: Option) =>
        isAllOption(option) || isEmptyOption(option);

    function translateSelections(selections: O[]): O[] {
        const normalSelections = selections.filter((o) => !isSpecialOption(o));

        if (selections.find(isAllOption)) {
            return options;
        }
        if (emptyOption && selections.find(isEmptyOption)) {
            return current.find(isEmptyOption)
                ? normalSelections
                : [emptyOption];
        }

        return normalSelections;
    }

    return (
        <Box
            display="flex"
            flexDirection="column"
            alignItems="center"
            justifyContent="end"
            height="100%"
        >
            <FormControl error={!!errorMessage}>
                {errorMessage && (
                    <FormHelperText
                        sx={{
                            boxSizing: "border-box",
                            height: FILTER_ERROR_HEIGHT,
                            mt: 0,
                            pb: "3px",
                            fontSize: "inherit",
                            fontWeight: "normal",
                            width,
                        }}
                    >
                        {errorMessage}
                    </FormHelperText>
                )}

                <Box
                    sx={{
                        boxSizing: "border-box",
                        height: TABLE_FILTER_HEIGHT,
                        position: "relative",
                        display: "flex",
                        justifyContent: "center",
                        maxWidth: paddedWidth,
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
                        options={specialOptions.concat(options)}
                        value={current}
                        loading={loading}
                        onFocus={() => setIsFocused(true)}
                        onBlur={() => setIsFocused(false)}
                        onChange={(_, values) => {
                            onChange(translateSelections(values));
                        }}
                        isOptionEqualToValue={(...args) =>
                            isOptionEqual(...args)
                        }
                        sx={{
                            background: "white",
                            fontSize: "inherit",
                            minWidth: 0,
                            lineHeight: 0,
                            button: { minHeight: 0, height: "100%" },
                            ...(isFocused
                                ? {
                                      position: "absolute",
                                      width: paddedWidth,
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
        </Box>
    );
}

function isOptionEqual(a: Option, b: Option) {
    return typeof a === "string" || typeof b === "string"
        ? a === b
        : a.id === b.id;
}
