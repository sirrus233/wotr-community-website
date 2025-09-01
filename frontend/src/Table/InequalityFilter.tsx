import React, { useEffect, useState } from "react";
import Box from "@mui/joy/Box";
import ClearIcon from "@mui/icons-material/CloseRounded";
import IconButton from "@mui/joy/IconButton";
import Option from "@mui/joy/Option";
import Select from "@mui/joy/Select";
import { InequalityFilter, InequalityOperator } from "../types";
import { isDefined, range } from "../utils";
import { TABLE_FILTER_HEIGHT } from "./constants";
import { InequalityFilterProps } from "./types";

type PseudoInequalityOperator = "GTE" | "LTE";

export default function InequalityFilter({
    current,
    min,
    max,
    placeholder,
    width,
    onChange,
}: InequalityFilterProps & { width: number }) {
    const [currentOperator, currentValue = null] = current || [];

    const [inputValue, setInputValue] = useState<number | null>(currentValue);
    const [inputOperator, setInputOperator] = useState<
        InequalityOperator | PseudoInequalityOperator
    >(currentOperator || "EQ");

    useEffect(() => {
        const didValueChange = currentValue !== inputValue;
        const didOperatorChange = currentOperator !== inputOperator;

        if (didValueChange) {
            onChange(
                isDefined(inputValue)
                    ? translateFilter(inputOperator, inputValue)
                    : null
            );
        } else if (didOperatorChange && isDefined(inputValue)) {
            onChange(translateFilter(inputOperator, inputValue));
        }
    }, [currentOperator, currentValue, inputOperator, inputValue]);

    const commonStyle = {
        background: "white",
        fontSize: "inherit",
        height: TABLE_FILTER_HEIGHT,
        maxHeight: "100%",
        minWidth: 0,
        minHeight: 0,
        lineHeight: 0,
    };

    return (
        <Box
            display="flex"
            alignItems="end"
            justifyContent="center"
            height="100%"
            maxWidth={width}
        >
            <Select
                size="sm"
                value={inputOperator}
                onChange={(_, o) => setInputOperator(o || "EQ")}
                sx={{
                    ...commonStyle,
                    width: "fit-content",
                    pl: "4px",
                    mr: "1px",
                }}
            >
                {(["EQ", "GT", "GTE", "LT", "LTE"] as const).map((o) => (
                    <Option key={o} value={o}>
                        {operatorLabel(o)}
                    </Option>
                ))}
            </Select>

            <Select
                size="sm"
                placeholder={placeholder}
                value={inputValue}
                onChange={(_, value) => setInputValue(value)}
                indicator={isDefined(inputValue) ? null : undefined}
                sx={{ ...commonStyle, flex: 1 }}
                endDecorator={
                    isDefined(inputValue) ? (
                        <ResetButton reset={() => setInputValue(null)} />
                    ) : undefined
                }
            >
                {range(min, max + 1).map((value) => (
                    <Option key={value} value={value}>
                        {value}
                    </Option>
                ))}
            </Select>
        </Box>
    );
}

function ResetButton(props: { reset: () => void }) {
    const { reset } = props;

    return (
        <IconButton
            size="sm"
            onMouseDown={(e) => e.stopPropagation()}
            onClick={reset}
            sx={{ minWidth: 0, minHeight: 0, lineHeight: 0, p: "1px" }}
        >
            <ClearIcon />
        </IconButton>
    );
}

function operatorLabel(
    operator: InequalityOperator | PseudoInequalityOperator
): string {
    switch (operator) {
        case "EQ":
            return "=";
        case "GT":
            return ">";
        case "LT":
            return "<";
        case "GTE":
            return ">=";
        case "LTE":
            return "<=";
    }
}

function translateFilter(
    operator: InequalityOperator | PseudoInequalityOperator,
    value: number
): [InequalityOperator, number] {
    switch (operator) {
        case "GTE":
            return ["GT", value - 1];
        case "LTE":
            return ["LT", value + 1];
        default:
            return [operator, value];
    }
}
