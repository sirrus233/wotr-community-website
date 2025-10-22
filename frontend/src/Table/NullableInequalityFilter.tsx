import React from "react";
import Box from "@mui/joy/Box";
import Checkbox from "@mui/joy/Checkbox";
import InequalityFilter from "./InequalityFilter";
import { NullableInequalityFilterProps } from "./types";
import { FilterContainer } from "./styledComponents";

export default function NullableInequalityFilter({
    current,
    onChange,
    nullLabel,
    ...props
}: NullableInequalityFilterProps & { width: number }) {
    return (
        <Box
            height="100%"
            display="flex"
            flexDirection="column"
            justifyContent="end"
            fontWeight="normal"
        >
            <Box display="flex" flexDirection="column" gap="5px">
                <FilterContainer
                    sx={{ alignItems: "center", justifyContent: "start" }}
                >
                    <Checkbox
                        size="sm"
                        label={nullLabel}
                        checked={current === "NullFilter"}
                        onChange={(e) =>
                            onChange(e.target.checked ? "NullFilter" : null)
                        }
                        slotProps={{ label: { sx: { fontSize: "12px" } } }}
                    />
                </FilterContainer>

                <InequalityFilter
                    {...props}
                    filterType="inequality"
                    current={current === "NullFilter" ? null : current}
                    onChange={onChange}
                    disabled={current === "NullFilter"}
                />
            </Box>
        </Box>
    );
}
