import React from "react";
import CollapseIcon from "@mui/icons-material/KeyboardArrowDown";
import ExpandIcon from "@mui/icons-material/KeyboardArrowUp";
import FilterIconAlt from "@mui/icons-material/FilterAlt";
import IconButton from "@mui/joy/IconButton";
import { styled } from "@mui/joy/styles";
import TableFilter from "./TableFilter";
import sumPriorWidths from "./sumPriorWidths";
import { MenuOption } from "../types";
import { ColHeaderData, CornerHeaderData } from "./types";

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
                        ) : filter ? (
                            <TableFilter width={width} {...filter} />
                        ) : (
                            <></>
                        )}
                    </CornerFilterSlot>
                );
            })}

            {colHeaders.map(({ key, filter, style = {} }) => (
                <ColumnFilterSlot key={key} sx={style}>
                    {filter ? <TableFilter {...filter} /> : <></>}
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
