import React from "react";
import Checkbox from "@mui/joy/Checkbox";
import List from "@mui/joy/List";
import ListItem from "@mui/joy/ListItem";

interface MultiOptionInputProps<T> {
    values: T[];
    current: T[];
    getLabel?: (value: T) => string;
    onChange: (value: T[]) => void;
    validate: () => void;
}

export default function MultiOptionInput<T>({
    values,
    current,
    getLabel = String,
    onChange,
    validate,
}: MultiOptionInputProps<T>) {
    const handleChange = (event: React.ChangeEvent<HTMLInputElement>) => {
        const selectedValue = values.find(
            (value) => String(value) === event.target.value
        );
        if (selectedValue !== undefined) {
            if (event.target.checked) {
                onChange(current.concat(selectedValue));
            } else {
                onChange(current.filter((value) => value !== selectedValue));
            }
            validate();
        }
    };

    return (
        <List
            orientation="horizontal"
            wrap
            sx={{
                "--List-gap": "10px",
                "--ListItem-radius": "30px",
                "--ListItem-minHeight": "32px",
            }}
        >
            {values.map((value, i) => (
                <ListItem key={i}>
                    <Checkbox
                        checked={current.includes(value)}
                        size="sm"
                        disableIcon
                        overlay
                        value={getLabel(value)}
                        label={getLabel(value)}
                        onChange={handleChange}
                    />
                </ListItem>
            ))}
        </List>
    );
}
