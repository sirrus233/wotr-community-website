import React, { useState } from "react";
import MaterialAutocomplete from "@mui/joy/Autocomplete";
import Box from "@mui/joy/Box";
import FormControl from "@mui/joy/FormControl";
import FormHelperText from "@mui/joy/FormHelperText";
import { ErrorMessage } from "./constants";
import { MenuOption } from "./types";

type Option = string | MenuOption<unknown>;

interface Props<O extends Option> {
    options: O[];
    current: O[];
    placeholder: string;
    loading: boolean;
    width: number;
    errorMessage?: ErrorMessage;
    allOption?: O;
    emptyOption?: O;
    onChange: (value: O[]) => void;
}

export default function TableFilter<O extends Option>({
    options,
    current,
    placeholder,
    loading,
    width,
    errorMessage,
    allOption,
    emptyOption,
    onChange,
}: Props<O>) {
    const [isFocused, setIsFocused] = useState(false);

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

function isOptionEqual(a: Option, b: Option) {
    return typeof a === "string" || typeof b === "string"
        ? a === b
        : a.id === b.id;
}
