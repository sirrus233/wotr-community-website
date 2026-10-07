import React from "react";
import Checkbox from "@mui/joy/Checkbox";

interface Props {
    current: boolean;
    label?: string;
    onChange: (value: boolean) => void;
    validate: () => void;
    labelReversed?: boolean;
}

export default function BooleanInput<T>({
    current,
    label,
    onChange,
    validate,
    labelReversed = false,
}: Props) {
    const handleChange = (event: React.ChangeEvent<HTMLInputElement>) => {
        onChange(event.target.checked);
        validate();
    };

    return (
        <Checkbox
            label={label}
            checked={current}
            onChange={handleChange}
            sx={{
                fontSize: "inherit",
                ...(labelReversed
                    ? {
                          display: "flex",
                          flexDirection: "row-reverse",
                          width: "fit-content",
                          justifyContent: "left",
                      }
                    : undefined),
            }}
        />
    );
}
