import React from "react";
import Box from "@mui/joy/Box";
import { TimestampFilterProps } from "./types";
import { FilterContainerVertical, StyledDateInput } from "./styledComponents";

export default function TimestampFilter({
    current,
    onChange,
    width,
}: TimestampFilterProps & { width: number }) {
    const [start, end] = current;

    return (
        <FilterContainerVertical width={width} sx={{ pt: "2px" }}>
            <DateInput
                name="from"
                value={fromTimestamp(start)}
                onChange={(value) => onChange([toTimestamp(value), end])}
            />

            <Box my="1px" fontSize="10px">
                ↓
            </Box>

            <DateInput
                name="to"
                value={fromTimestamp(end)}
                onChange={(value) =>
                    onChange([start, toTimestamp(value, { atEndOfDay: true })])
                }
            />
        </FilterContainerVertical>
    );
}

interface DateInputProps {
    name: string;
    value: string;
    onChange: (value: string) => void;
}

function DateInput({ name, value, onChange }: DateInputProps) {
    return (
        <StyledDateInput
            type="date"
            name={name}
            value={value}
            onChange={(e) => onChange(e.target.value)}
            sx={{ input: { color: !value ? "#888" : undefined } }}
        />
    );
}

function toTimestamp(
    dateString: string,
    { atEndOfDay = false } = {}
): Date | null {
    if (dateString) {
        const [year, month, day] = dateString.split("-").map(Number);

        const localDate = new Date();
        localDate.setMonth(month - 1);
        localDate.setDate(day);
        localDate.setFullYear(year);

        if (atEndOfDay) {
            localDate.setHours(23);
            localDate.setMinutes(59);
            localDate.setSeconds(59);
            localDate.setMilliseconds(99);
        } else {
            localDate.setHours(0);
            localDate.setMinutes(0);
            localDate.setSeconds(0);
            localDate.setMilliseconds(0);
        }

        return localDate;
    }

    return null;
}

function fromTimestamp(date: Date | null): string {
    if (date) {
        return [
            String(date.getFullYear()).padStart(4, "0"),
            String(date.getMonth() + 1).padStart(2, "0"),
            String(date.getDate()).padStart(2, "0"),
        ].join("-");
    }

    return "";
}
