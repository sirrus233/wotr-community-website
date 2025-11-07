import React, { CSSProperties } from "react";
import Box from "@mui/joy/Box";
import Button from "@mui/joy/Button";
import ButtonGroup from "@mui/joy/ButtonGroup";
import sizes from "../styles/sizes";

type Variant = "outlined" | "plain" | "soft" | "solid";

interface Props<T extends string | number> {
    current: T | null;
    options: T[];
    setCurrent: (year: T) => void;
    getLabel?: (option: T) => string;
    variant?: Variant;
    selectedVariant?: Variant;
    style?: CSSProperties;
}

export default function ButtonSelector<T extends string | number>({
    current,
    options,
    setCurrent,
    getLabel = String,
    variant = "plain",
    selectedVariant = variant,
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
            <ButtonGroup style={{ height: `${sizes.buttonSelectorHeight}px` }}>
                {options.map((option) => (
                    <Button
                        key={option}
                        variant={current === option ? selectedVariant : variant}
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
