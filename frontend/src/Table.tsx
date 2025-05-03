import React, { CSSProperties, ReactNode, useState } from "react";
import Box from "@mui/joy/Box";
import CollapseIcon from "@mui/icons-material/KeyboardArrowDown";
import ExpandIcon from "@mui/icons-material/KeyboardArrowUp";
import FilterIcon from "@mui/icons-material/FilterList";
import FilterIconAlt from "@mui/icons-material/FilterAlt";
import IconButton from "@mui/joy/IconButton";
import { styled } from "@mui/joy/styles";
import TableFilter, {
    FILTER_ERROR_HEIGHT,
    TABLE_FILTER_HEIGHT,
    TableFilterProps,
} from "./TableFilter";
import {
    TABLE_BORDER_COLOR,
    TABLE_FONT_COLOR,
    TABLE_HEADER_COLOR,
} from "./styles/colors";
import { MenuOption } from "./types";
import { fallback, sum } from "./utils";

/**
 * STYLED COMPONENTS
 */

const LIGHT_BORDER = `1px solid ${TABLE_BORDER_COLOR}`;
const STRONG_BORDER = `2px solid ${TABLE_BORDER_COLOR}`;
const CELL_PADDING = "5px";
const FILTER_BAR_Z_IDX = 3;
const PINNED_COLS_Z_IDX = FILTER_BAR_Z_IDX - 1;

interface ContainerProps {
    pinnedColCount: number;
}

const Container = styled("table")<{ ownerState: ContainerProps }>(
    ({ ownerState: { pinnedColCount } }) => ({
        "td, th, tr": { boxSizing: "border-box" },
        boxSizing: "border-box",
        width: "100%",
        height: "100%",
        fontSize: "12px",
        color: TABLE_FONT_COLOR,
        borderSpacing: 0,
        whiteSpace: "nowrap",
        textAlign: "center",
        th: {
            position: "sticky",
            background: TABLE_HEADER_COLOR,
            textOverflow: "ellipsis",
        },
        [`tr th:nth-of-type(${pinnedColCount})`]: {
            borderRight: LIGHT_BORDER,
        },
    })
);

const FilterBar = styled("tr")({
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
    "&:first-child": { paddingLeft: CELL_PADDING },
});

const TableHeader = styled("th")({
    overflow: "hidden",
    padding: CELL_PADDING,
});

const ColHeader = styled(TableHeader)({
    borderBottom: STRONG_BORDER,
    minWidth: "100px",
});

const CornerHeader = styled(ColHeader)({
    zIndex: PINNED_COLS_Z_IDX,
});

const RowHeader = styled(TableHeader)({
    borderBottom: LIGHT_BORDER,
    fontWeight: "normal",
    "&:last-of-type": { borderRight: LIGHT_BORDER },
});

const BodyCell = styled("td")({
    padding: CELL_PADDING,
    textOverflow: "ellipsis",
    borderBottom: LIGHT_BORDER,
});

/**
 * MAIN
 */

export interface ColHeaderData<T extends MenuOption<any> = MenuOption<any>> {
    key: string | number;
    content?: ReactNode;
    filter?: TableFilterProps<T> & { width: number; appliedCount: number };
    span?: number;
    style?: CSSProperties;
}

export interface RowHeaderData {
    key: string | number;
    content?: ReactNode;
    span?: number;
    style?: CSSProperties;
}

export interface CellData {
    key: string | number;
    content?: ReactNode;
    span?: number;
    style?: CSSProperties;
}

export interface RowData {
    key: string | number;
    cells: CellData[];
    style?: CSSProperties;
}

export interface CornerHeaderData<T extends MenuOption<any> = MenuOption<any>> {
    key: string | number;
    content?: ReactNode;
    width: number;
    filter?: Omit<TableFilterProps<T>, "width"> & { appliedCount: number };
    span?: number;
    style?: CSSProperties;
}

interface Props<
    CorH extends CornerHeaderData<MenuOption<any>>,
    ColH extends ColHeaderData<MenuOption<any>>
> {
    cornerHeaders: CorH[];
    colHeaders: ColH[];
    rows: RowData[];
    style?: CSSProperties;
    hasFilters?: boolean;
}

export default function Table<
    T extends CornerHeaderData<MenuOption<any>>,
    U extends ColHeaderData<MenuOption<any>>
>({
    cornerHeaders,
    colHeaders,
    rows,
    style = {},
    hasFilters = false,
}: Props<T, U>) {
    const [areFiltersOpen, setAreFiltersOpen] = useState(false);

    const filterBarHeight = [...cornerHeaders, ...colHeaders].find(
        (h) => h.filter?.errorMessage
    )
        ? `calc(${TABLE_FILTER_HEIGHT} + ${FILTER_ERROR_HEIGHT} + 10px)`
        : `calc(${TABLE_FILTER_HEIGHT} + 10px)`;

    return (
        <Container
            sx={style}
            ownerState={{ pinnedColCount: cornerHeaders.length }}
        >
            <thead>
                {areFiltersOpen && (
                    <FilterBar>
                        {cornerHeaders.map(
                            ({ key, filter, width, style = {} }, i, cHs) => {
                                return (
                                    <CornerFilterSlot
                                        key={key}
                                        sx={{
                                            width,
                                            height: filterBarHeight,
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
                                            <TableFilter
                                                width={width}
                                                {...filter}
                                            />
                                        ) : (
                                            <></>
                                        )}
                                    </CornerFilterSlot>
                                );
                            }
                        )}

                        {colHeaders.map(({ key, filter, style = {} }) => (
                            <ColumnFilterSlot key={key} sx={style}>
                                {filter ? <TableFilter {...filter} /> : <></>}
                            </ColumnFilterSlot>
                        ))}
                    </FilterBar>
                )}

                <tr>
                    {cornerHeaders.map(
                        (
                            { key, content, span, filter, width, style = {} },
                            i,
                            cHs
                        ) => (
                            <CornerHeader
                                scope="col"
                                key={key}
                                colSpan={span}
                                sx={{
                                    width,
                                    maxWidth: width,
                                    minWidth: width,
                                    top: areFiltersOpen ? filterBarHeight : 0,
                                    left: sumPriorWidths(cHs, i),
                                    ...style,
                                }}
                            >
                                {hasFilters && i === 0 && !areFiltersOpen && (
                                    <ExpandButton
                                        expanded={areFiltersOpen}
                                        setExpanded={setAreFiltersOpen}
                                    />
                                )}
                                {filter ? (
                                    <FilterHeader
                                        areFiltersOpen={areFiltersOpen}
                                        setAreFiltersOpen={setAreFiltersOpen}
                                        appliedCount={filter.appliedCount}
                                    >
                                        {fallback(content, key)}
                                    </FilterHeader>
                                ) : (
                                    fallback(content, key)
                                )}
                            </CornerHeader>
                        )
                    )}

                    {colHeaders.map(
                        ({ key, content, span, filter, style = {} }) => (
                            <ColHeader
                                scope="col"
                                key={key}
                                colSpan={span}
                                sx={{
                                    width: filter?.width,
                                    minWidth: filter?.width,
                                    top: areFiltersOpen ? filterBarHeight : 0,
                                    ...style,
                                }}
                            >
                                {filter ? (
                                    <FilterHeader
                                        areFiltersOpen={areFiltersOpen}
                                        setAreFiltersOpen={setAreFiltersOpen}
                                        appliedCount={filter.appliedCount}
                                    >
                                        {fallback(content, key)}
                                    </FilterHeader>
                                ) : (
                                    fallback(content, key)
                                )}
                            </ColHeader>
                        )
                    )}
                </tr>
            </thead>

            <tbody>
                {rows.map((row) => (
                    <tr key={row.key}>
                        {row.cells.map((cell, i) =>
                            cornerHeaders[i] ? (
                                <RowHeader
                                    scope="row"
                                    key={cell.key}
                                    colSpan={cell.span}
                                    sx={{
                                        maxWidth: cornerHeaders[i].width,
                                        left: sumPriorWidths(cornerHeaders, i),
                                        ...cell.style,
                                    }}
                                >
                                    {fallback(cell.content, cell.key)}
                                </RowHeader>
                            ) : (
                                <BodyCell
                                    key={cell.key}
                                    colSpan={cell.span}
                                    sx={cell.style}
                                >
                                    {cell.content}
                                </BodyCell>
                            )
                        )}
                    </tr>
                ))}
            </tbody>
        </Container>
    );
}

/**
 * HELPERS
 */

function sumPriorWidths(headers: { width: number }[], position: number) {
    return sum(headers.slice(0, position).map((h) => h.width));
}

interface ExpandButtonProps {
    expanded: boolean;
    setExpanded: (expanded: boolean) => void;
}

function ExpandButton({ expanded, setExpanded }: ExpandButtonProps) {
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

type FilterHeaderProps = {
    areFiltersOpen: boolean;
    setAreFiltersOpen: (areOpen: boolean) => void;
    appliedCount: number;
    children: ReactNode;
    style?: CSSProperties;
};

function FilterHeader({
    areFiltersOpen,
    setAreFiltersOpen,
    appliedCount,
    children,
    style = {},
}: FilterHeaderProps) {
    return (
        <Box
            sx={{
                display: "flex",
                alignItems: "center",
                justifyContent: "center",
                height: "100%",
                ...style,
            }}
        >
            {children}
            <FilterButton
                areFiltersOpen={areFiltersOpen}
                setAreFiltersOpen={setAreFiltersOpen}
                appliedCount={appliedCount}
            />
        </Box>
    );
}

interface FilterButtonProps {
    areFiltersOpen: boolean;
    setAreFiltersOpen: (areOpen: boolean) => void;
    appliedCount: number;
}

function FilterButton({
    areFiltersOpen,
    setAreFiltersOpen,
    appliedCount,
}: FilterButtonProps) {
    return (
        <IconButton
            onClick={() => setAreFiltersOpen(!areFiltersOpen)}
            size="sm"
            color="primary"
            variant={appliedCount ? "solid" : undefined}
            sx={{
                minWidth: 0,
                minHeight: 0,
                px: "4px",
                height: "1.5em",
                position: "absolute",
                justifyContent: "end",
                right: 0,
                mr: 1,
            }}
        >
            <FilterIcon />
            <Box fontSize="12px">{appliedCount ? appliedCount : null}</Box>
        </IconButton>
    );
}
