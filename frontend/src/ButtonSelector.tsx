import React, { CSSProperties } from "react";
import Box from "@mui/joy/Box";
import Button from "@mui/joy/Button";
import ButtonGroup from "@mui/joy/ButtonGroup";
import { BUTTON_SELECTOR_HEIGHT } from "./styles/sizes";

interface Props<T extends string | number> {
    current: T | null;
    options: T[];
    setCurrent: (year: T) => void;
    getLabel?: (option: T) => string;
    variant?: "outlined" | "plain" | "soft" | "solid";
    style?: CSSProperties;
}

export default function ButtonSelector<T extends string | number>({
    current,
    options,
    setCurrent,
    getLabel = String,
    variant = "plain",
    style = {},
}: Props<T>) {
    return (
        <Box
            sx={{
                display: "flex",
                justifyContent: "center",
                width: "100%",
                ...style,
            }}
        >
            <ButtonGroup style={{ height: `${BUTTON_SELECTOR_HEIGHT}px` }}>
                {options.map((option) => (
                    <Button
                        key={option}
                        variant={variant}
                        onClick={() => setCurrent(option)}
                        sx={{
                            fontWeight: current === option ? "bold" : "normal",
                        }}
                    >
                        {getLabel(option)}
                    </Button>
                ))}
            </ButtonGroup>
        </Box>
    );
}
