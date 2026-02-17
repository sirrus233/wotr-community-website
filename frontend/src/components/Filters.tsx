import React from "react";
import Checkbox from "@mui/joy/Checkbox";
import List from "@mui/joy/List";
import ListItem from "@mui/joy/ListItem";

interface Props<T> {
    options: T[];
    current: T[];
    onChange: (value: T[]) => void;
}

export default function Filters<T>({ options, current, onChange }: Props<T>) {
    const handleChange = (event: React.ChangeEvent<HTMLInputElement>) => {
        const selectedValue = options.find(
            (value) => String(value) === event.target.value,
        );
        if (selectedValue !== undefined) {
            if (event.target.checked) {
                onChange(current.concat(selectedValue));
            } else {
                onChange(current.filter((value) => value !== selectedValue));
            }
        }
    };

    return (
        <List
            orientation="horizontal"
            wrap
            sx={{
                display: "flex",
                flexDirection: "row",
                alignItems: "center",
                justifyContent: "center",
                "--List-gap": "10px",
                "--ListItem-radius": "30px",
                "--ListItem-minHeight": "32px",
            }}
        >
            {options.map((option, i) => (
                <ListItem key={i}>
                    <Checkbox
                        checked={current.includes(option)}
                        size="sm"
                        disableIcon
                        overlay
                        value={String(option)}
                        label={String(option)}
                        onChange={handleChange}
                    />
                </ListItem>
            ))}
        </List>
    );
}
