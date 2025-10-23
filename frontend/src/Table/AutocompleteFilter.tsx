import React, { useState } from "react";
import MaterialAutocomplete from "@mui/joy/Autocomplete";
import Box from "@mui/joy/Box";
import FormControl from "@mui/joy/FormControl";
import FormHelperText from "@mui/joy/FormHelperText";
import { hasKey } from "../utils";
import { FILTER_ERROR_HEIGHT, TABLE_FILTER_HEIGHT } from "./constants";
import { FilterContainer } from "./styledComponents";
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
    listboxStyle,
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
        <FilterContainer
            sx={{
                flexDirection: "column",
                alignItems: "center",
                justifyContent: "end",
            }}
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
                        slotProps={{ listbox: { sx: listboxStyle } }}
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
        </FilterContainer>
    );
}

function isOptionEqual(a: Option, b: Option) {
    return typeof a === "string" || typeof b === "string"
        ? a === b
        : Array.isArray(a.id) && Array.isArray(b.id)
        ? areArraysShallowlyEqual(a.id, b.id)
        : typeof a.id === "object" && typeof b.id === "object"
        ? areObjectsShallowlyEqual(a.id, b.id)
        : a.id === b.id;
}

function areArraysShallowlyEqual(a: unknown[], b: unknown[]): boolean {
    return a.every((el, i) => el === b[i]) && b.every((el, i) => el === a[i]);
}

function areObjectsShallowlyEqual(a: object | null, b: object | null): boolean {
    return a === null || b === null
        ? a === b
        : Object.entries(a).every(([k, v]) => hasKey(b, k) && b[k] === v) &&
              Object.entries(b).every(([k, v]) => hasKey(a, k) && a[k] === v);
}
