import React from "react";
import { TimestampFilterProps } from "./types";
import {
    DateInput,
    FilterContainerVertical,
    FormControlGridRow,
    InputLabel,
} from "./styledComponents";

export default function TimestampFilter({
    current,
    onChange,
    width,
}: TimestampFilterProps & { width: number }) {
    const [start, end] = current;

    return (
        <FilterContainerVertical width={width} gap="5px">
            <DateField
                label="From:"
                value={dateToString(start)}
                onChange={(value) => onChange([stringToDate(value), end])}
            />

            <DateField
                label="To:"
                value={dateToString(end)}
                onChange={(value) =>
                    onChange([start, advanceToEndOfDay(stringToDate(value))])
                }
            />
        </FilterContainerVertical>
    );
}

interface DateFieldProps {
    label: string;
    value: string;
    onChange: (value: string) => void;
}

function DateField({ label, value, onChange }: DateFieldProps) {
    return (
        <FormControlGridRow columnWidths={["35px", "1fr"]}>
            <InputLabel>{label}</InputLabel>
            <DateInput
                type="date"
                value={value}
                onChange={(e) => onChange(e.target.value)}
                sx={{ input: { color: !value ? "#888" : undefined } }}
            />
        </FormControlGridRow>
    );
}

function stringToDate(dateString: string): Date | null {
    if (dateString) {
        const [year, month, day] = dateString.split("-").map(Number);

        const localDate = new Date();
        localDate.setMonth(month - 1);
        localDate.setDate(day);
        localDate.setFullYear(year);
        localDate.setHours(0);
        localDate.setMinutes(0);
        localDate.setSeconds(0);
        localDate.setMilliseconds(0);

        return localDate;
    }

    return null;
}

function dateToString(date: Date | null): string {
    if (date) {
        return [
            String(date.getFullYear()).padStart(4, "0"),
            String(date.getMonth() + 1).padStart(2, "0"),
            String(date.getDate()).padStart(2, "0"),
        ].join("-");
    }

    return "";
}

function advanceToEndOfDay(date: Date | null) {
    if (date) {
        date.setHours(23);
        date.setMinutes(59);
        date.setSeconds(59);
        date.setMilliseconds(999);
    }
    return date;
}
