import React from "react";
import Select from "@mui/joy/Select";
import Option from "@mui/joy/Option";

interface SelectOptionInputProps<T> {
    values: T[];
    current: T;
    getLabel?: (value: T) => string;
    onChange: (value: T) => void;
    validate: () => void;
}

export default function SelectOptionInput<T>({
    values,
    current,
    getLabel = String,
    onChange,
    validate,
}: SelectOptionInputProps<T>) {
    return (
        <Select
            defaultValue={values[0]}
            onChange={(_, value) => onChange(value as T)}
            onClose={validate}
        >
            {values.map((value, i) => (
                <Option key={i} value={value}>
                    {getLabel(value)}
                </Option>
            ))}
        </Select>
    );
}
