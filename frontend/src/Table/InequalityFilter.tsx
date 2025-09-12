import React, { useEffect, useState } from "react";
import Input from "@mui/joy/Input";
import Option from "@mui/joy/Option";
import Select from "@mui/joy/Select";
import { InequalityFilter, InequalityOperator } from "../types";
import { isDefined, noNansense } from "../utils";
import { TABLE_FILTER_HEIGHT } from "./constants";
import ResetButton from "./ResetButton";
import { FilterContainer } from "./styledComponents";
import { InequalityFilterProps } from "./types";

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
    const [inputOperator, setInputOperator] = useState<InequalityOperator>(
        currentOperator || "EQ"
    );

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
        <FilterContainer
            sx={{
                maxWidth: width,
                "&&": { "*": { margin: 0, marginInline: 0 } },
            }}
        >
            <Select
                size="sm"
                value={inputOperator}
                onChange={(_, o) => setInputOperator(o || "EQ")}
                sx={{
                    ...commonStyle,
                    width: "fit-content",
                    px: "4px",
                    mr: "1px",
                }}
            >
                {(["EQ", "GT", "GTE", "LT", "LTE"] as const).map((o) => (
                    <Option key={o} value={o}>
                        {operatorLabel(o)}
                    </Option>
                ))}
            </Select>

            <Input
                size="sm"
                type="number"
                placeholder={placeholder}
                value={inputValue === null ? "" : String(inputValue)}
                onChange={(e) =>
                    setInputValue(
                        constrainNumberInput(e.target.value, min, max)
                    )
                }
                sx={{ ...commonStyle, flex: 1 }}
                slotProps={{ input: { min, max } }}
                endDecorator={
                    isDefined(inputValue) ? (
                        <ResetButton reset={() => setInputValue(null)} />
                    ) : undefined
                }
            />
        </FilterContainer>
    );
}

function operatorLabel(operator: InequalityOperator): string {
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
    operator: InequalityOperator,
    value: number
): InequalityFilter {
    switch (operator) {
        case "GTE":
            return ["GT", value - 1];
        case "LTE":
            return ["LT", value + 1];
        default:
            return [operator, value];
    }
}

function constrainNumberInput(
    value: string,
    min?: number,
    max?: number
): number | null {
    if (value === "") return null;
    const num = noNansense(Number(value));
    if (isDefined(min) && num < min) return min;
    if (isDefined(max) && num > max) return max;
    return num;
}
