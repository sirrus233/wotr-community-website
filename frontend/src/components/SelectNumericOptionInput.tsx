import React from "react";
import SelectOptionInput from "./SelectOptionInput";
import { INFINITE } from "../constants";

const NULL_MASK = -1;

interface SelectNumericOptionInputProps {
    start: number;
    end: number;
    current: number | null;
    initializeEmpty?: boolean;
    allowInfinite?: boolean;
    onChange: (value: number | null) => void;
    validate: () => void;
    display?: (x: any) => string;
}

export default function SelectNumericOptionInput({
    start,
    end,
    current,
    initializeEmpty = true,
    allowInfinite = false,
    onChange,
    validate,
    display = String,
}: SelectNumericOptionInputProps) {
    const values: number[] = initializeEmpty ? [NULL_MASK] : [];
    for (let i = start; i <= end; i++) {
        values.push(i);
    }
    if (allowInfinite) {
        values.push(INFINITE);
    }

    return (
        <SelectOptionInput
            values={values}
            current={maskNull(current)}
            getLabel={(value) => {
                if (value === NULL_MASK) {
                    return "";
                } else if (value === INFINITE) {
                    return "Unlimited!";
                } else {
                    return display(value);
                }
            }}
            onChange={(val) => onChange(unmaskNull(val))}
            validate={validate}
        />
    );
}

function maskNull(value: number | null) {
    return value === null ? NULL_MASK : value;
}

function unmaskNull(value: number) {
    return value === NULL_MASK ? null : value;
}
