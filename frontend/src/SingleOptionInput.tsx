import React from "react";
import Radio from "@mui/joy/Radio";
import RadioGroup from "@mui/joy/RadioGroup";

interface SingleOptionInputProps<T> {
    values: T[];
    current: T;
    getLabel?: (value: T) => string;
    onChange: (value: T) => void;
    validate: () => void;
}

export default function SingleOptionInput<T>({
    values,
    current,
    getLabel = String,
    onChange,
    validate,
}: SingleOptionInputProps<T>) {
    const handleChange = (event: React.ChangeEvent<HTMLInputElement>) => {
        const selectedValue = values.find(
            (value) => String(value) === event.target.value
        );
        if (selectedValue !== undefined) {
            onChange(selectedValue);
            validate();
        }
    };

    return (
        <RadioGroup
            value={current}
            orientation="horizontal"
            onChange={handleChange}
        >
            {values.map((value, i) => (
                <Radio key={i} value={value} label={getLabel(value)} />
            ))}
        </RadioGroup>
    );
}
