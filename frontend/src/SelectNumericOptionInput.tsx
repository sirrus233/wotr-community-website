import React from "react";
import SelectOptionInput from "./SelectOptionInput";
import { INFINITE } from "./constants";

interface SelectNumericOptionInputProps {
    start: number;
    end: number;
    initializeEmpty?: boolean;
    allowInfinite?: boolean;
    onChange: (value: number | null) => void;
    validate: () => void;
}

export default function SelectNumericOptionInput({
    start,
    end,
    initializeEmpty = true,
    allowInfinite = false,
    onChange,
    validate,
}: SelectNumericOptionInputProps) {
    const values: (number | null)[] = initializeEmpty ? [null] : [];
    for (let i = start; i <= end; i++) {
        values.push(i);
    }
    if (allowInfinite) {
        values.push(INFINITE);
    }

    return (
        <SelectOptionInput
            values={values}
            current={start}
            getLabel={(value) => {
                if (value === null) {
                    return "";
                } else if (value === INFINITE) {
                    return "Unlimited!";
                } else {
                    return String(value);
                }
            }}
            onChange={onChange}
            validate={validate}
        />
    );
}
