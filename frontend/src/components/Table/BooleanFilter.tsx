import React from "react";
import Option from "@mui/joy/Option";
import Select from "@mui/joy/Select";
import { isDefined } from "../../utils";
import { TABLE_FILTER_HEIGHT } from "./constants";
import ResetButton from "./ResetButton";
import { FilterContainer } from "./styledComponents";
import { BooleanFilterProps } from "./types";

export default function BooleanFilter({
    current,
    placeholder,
    width,
    trueLabel,
    falseLabel,
    onChange,
}: BooleanFilterProps & { width: number }) {
    return (
        <FilterContainer sx={{ width, minWidth: width, maxWidth: width }}>
            <Select
                size="sm"
                placeholder={placeholder}
                renderValue={(o) => o?.label}
                value={current}
                onChange={(_, value) => onChange(value)}
                endDecorator={
                    isDefined(current) ? (
                        <ResetButton reset={() => onChange(null)} />
                    ) : undefined
                }
                sx={{
                    boxSizing: "border-box",
                    height: TABLE_FILTER_HEIGHT,
                    minHeight: 0,
                    maxHeight: "100%",
                    width: "100%",
                    minWidth: 0,
                    background: "white",
                    fontSize: "inherit",
                }}
            >
                {[true, false].map((option) => (
                    <Option key={String(option)} value={option}>
                        {option ? trueLabel : falseLabel}
                    </Option>
                ))}
            </Select>
        </FilterContainer>
    );
}
