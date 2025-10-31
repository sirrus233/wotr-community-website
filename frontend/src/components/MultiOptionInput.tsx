import React, { ReactNode } from "react";
import Checkbox from "@mui/joy/Checkbox";
import List from "@mui/joy/List";
import ListItem from "@mui/joy/ListItem";

interface MultiOptionInputProps<T> {
    values: T[];
    current: T[];
    fakeOptions?: ReactNode;
    getLabel?: (value: T) => string;
    onChange: (value: T[]) => void;
    validate: () => void;
}

export default function MultiOptionInput<T>({
    values,
    current,
    fakeOptions,
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
            {fakeOptions}
            {values.map((value, i) => (
                <MultiOptionInputItem
                    key={i}
                    checked={current.includes(value)}
                    value={String(value)}
                    label={getLabel(value)}
                    onChange={handleChange}
                />
            ))}
        </List>
    );
}

interface MultiOptionInputItemProps {
    checked: boolean;
    value: string;
    label: string;
    onChange: (event: React.ChangeEvent<HTMLInputElement>) => void;
}

export function MultiOptionInputItem({
    checked,
    value,
    label,
    onChange,
}: MultiOptionInputItemProps) {
    return (
        <ListItem>
            <Checkbox
                checked={checked}
                size="sm"
                disableIcon
                overlay
                value={value}
                label={label}
                onChange={onChange}
            />
        </ListItem>
    );
}
