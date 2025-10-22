import React from "react";
import CollapseIcon from "@mui/icons-material/KeyboardArrowDown";
import ExpandIcon from "@mui/icons-material/KeyboardArrowUp";
import FilterIconAlt from "@mui/icons-material/FilterAlt";
import IconButton from "@mui/joy/IconButton";
import { styled } from "@mui/joy/styles";
import AutocompleteFilter from "./AutocompleteFilter";
import BooleanFilter from "./BooleanFilter";
import InequalityFilter from "./InequalityFilter";
import sumPriorWidths from "./sumPriorWidths";
import { MenuOption } from "../types";
import { ColHeaderData, CornerHeaderData } from "./types";

import NullableInequalityFilter from "./NullableInequalityFilter";

const FILTER_BAR_Z_IDX = 3;

export const PINNED_COLS_Z_IDX = FILTER_BAR_Z_IDX - 1;

const Container = styled("tr")({
    position: "relative",
    overflow: "hidden",
    zIndex: FILTER_BAR_Z_IDX,
    th: { top: 0 },
});

const ColumnFilterSlot = styled("th")({
    overflow: "visible",
});

const CornerFilterSlot = styled(ColumnFilterSlot)({
    zIndex: PINNED_COLS_Z_IDX,
});

interface Props<
    CorH extends CornerHeaderData<MenuOption<any>>,
    ColH extends ColHeaderData<MenuOption<any>>
> {
    cornerHeaders: CorH[];
    colHeaders: ColH[];
    areFiltersOpen: boolean;
    setAreFiltersOpen: (areFiltersOpen: boolean) => void;
    height: string;
}

export default function FilterBar<
    CorH extends CornerHeaderData<MenuOption<any>>,
    ColH extends ColHeaderData<MenuOption<any>>
>({
    cornerHeaders,
    colHeaders,
    areFiltersOpen,
    setAreFiltersOpen,
    height,
}: Props<CorH, ColH>) {
    return (
        <Container>
            {cornerHeaders.map(({ key, filter, width, style = {} }, i, cHs) => {
                return (
                    <CornerFilterSlot
                        key={key}
                        sx={{
                            width,
                            height,
                            left: sumPriorWidths(cHs, i),
                            ...style,
                        }}
                    >
                        {i === 0 ? (
                            <ExpandButton
                                expanded={areFiltersOpen}
                                setExpanded={setAreFiltersOpen}
                            />
                        ) : (
                            <Filter filter={filter} width={width} />
                        )}
                    </CornerFilterSlot>
                );
            })}

            {colHeaders.map(({ key, filter, width = 0, style = {} }) => (
                <ColumnFilterSlot key={key} sx={style}>
                    <Filter filter={filter} width={width} />
                </ColumnFilterSlot>
            ))}
        </Container>
    );
}

interface ExpandButtonProps {
    expanded: boolean;
    setExpanded: (expanded: boolean) => void;
}

export function ExpandButton({ expanded, setExpanded }: ExpandButtonProps) {
    return (
        <IconButton
            onClick={() => setExpanded(!expanded)}
            color="primary"
            sx={{
                display: "flex",
                minWidth: 0,
                minHeight: 0,
                height: "100%",
            }}
        >
            <FilterIconAlt />
            {expanded ? <CollapseIcon /> : <ExpandIcon />}
        </IconButton>
    );
}

interface HeaderWithFilterProps {
    filter?: ColHeaderData["filter"] | CornerHeaderData["filter"];
    width: number;
}

export function Filter({ filter, width }: HeaderWithFilterProps): JSX.Element {
    switch (filter?.filterType) {
        case "autocomplete":
            return <AutocompleteFilter width={width} {...filter} />;
        case "inequality":
            return <InequalityFilter width={width} {...filter} />;
        case "nullableInequality":
            return <NullableInequalityFilter width={width} {...filter} />;
        case "boolean":
            return <BooleanFilter width={width} {...filter} />;
        case undefined:
            return <></>;
    }
}
