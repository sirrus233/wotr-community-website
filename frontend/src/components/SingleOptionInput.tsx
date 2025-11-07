import React from "react";
import Radio from "@mui/joy/Radio";
import RadioGroup from "@mui/joy/RadioGroup";
import useMediaQuery from "../hooks/useMediaQuery";

interface SingleOptionInputProps<T> {
    values: Exclude<T, null>[];
    current: T;
    orientation?: "horizontal" | "vertical";
    getLabel?: (value: Exclude<T, null>) => string;
    onChange: (value: T) => void;
    validate: () => void;
}

export default function SingleOptionInput<T>({
    values,
    current,
    orientation,
    getLabel = String,
    onChange,
    validate,
}: SingleOptionInputProps<T>) {
    const belowSmallBreakpoint = useMediaQuery("sm");

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
            orientation={
                orientation ||
                (belowSmallBreakpoint ? "vertical" : "horizontal")
            }
            onChange={handleChange}
        >
            {values.map((value, i) => (
                <Radio
                    key={i}
                    value={value}
                    label={getLabel(value)}
                    sx={{ fontSize: "inherit" }}
                />
            ))}
        </RadioGroup>
    );
}
