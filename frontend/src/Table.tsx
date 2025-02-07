import React, { CSSProperties, ReactNode } from "react";
import {
    TABLE_BORDER_COLOR,
    TABLE_FONT_COLOR,
    TABLE_HEADER_COLOR,
} from "./styles/colors";

export interface ColHeaderData {
    key: string | number;
    content: ReactNode;
    style?: CSSProperties;
}

export interface CellData {
    key: string | number;
    content: ReactNode;
    style?: CSSProperties;
}

export interface RowData {
    key: string | number;
    header: ReactNode;
    bodyCells: CellData[];
    style?: CSSProperties;
}

export interface CornerHeaderData {
    content: ReactNode;
    style?: CSSProperties;
}

interface Props {
    cornerHeader?: CornerHeaderData;
    colHeaders: ColHeaderData[];
    rows: RowData[];
    style?: CSSProperties;
}

export default function Table({
    cornerHeader,
    colHeaders,
    rows,
    style = {},
}: Props) {
    const headerCellStyle: CSSProperties = {
        width: `calc(100% / ${colHeaders.length + 1})`,
    };

    return (
        <table
            style={{
                width: "100%",
                fontSize: "14px",
                color: TABLE_FONT_COLOR,
                borderSpacing: 0,
                ...style,
            }}
        >
            <thead>
                <tr>
                    <CornerHeader
                        style={{ ...cornerHeader?.style, ...headerCellStyle }}
                    >
                        {cornerHeader?.content}
                    </CornerHeader>

                    {colHeaders.map((colHeader) => (
                        <ColHeader
                            key={colHeader.key}
                            style={{ ...colHeader.style, ...headerCellStyle }}
                        >
                            {colHeader.content}
                        </ColHeader>
                    ))}
                </tr>
            </thead>

            <tbody>
                {rows.map((row) => (
                    <tr key={row.key}>
                        <RowHeader style={row.style}>{row.header}</RowHeader>

                        {row.bodyCells.map((cell) => (
                            <BodyCell key={cell.key} style={cell.style}>
                                {cell.content}
                            </BodyCell>
                        ))}
                    </tr>
                ))}
            </tbody>
        </table>
    );
}

const HEADER_CELL_STYLE: CSSProperties = {
    minWidth: "100px",
    overflow: "hidden",
    whiteSpace: "nowrap",
    textOverflow: "ellipsis",
    padding: "5px",
    textAlign: "center",
    background: TABLE_HEADER_COLOR,
};

interface CornerHeaderProps {
    children?: ReactNode;
    style?: CSSProperties;
}

function CornerHeader({ children, style = {} }: CornerHeaderProps) {
    return (
        <th
            scope="col"
            style={{
                position: "sticky",
                left: 0,
                top: 0,
                zIndex: 2,
                borderBottom: `2px solid ${TABLE_BORDER_COLOR}`,
                borderRight: `2px solid ${TABLE_BORDER_COLOR}`,
                ...HEADER_CELL_STYLE,
                ...style,
            }}
        >
            {children}
        </th>
    );
}

interface RowHeaderProps {
    children?: ReactNode;
    style?: CSSProperties;
}

function RowHeader({ children, style = {} }: RowHeaderProps) {
    return (
        <th
            scope="row"
            style={{
                position: "sticky",
                left: 0,
                borderBottom: `1px solid ${TABLE_BORDER_COLOR}`,
                borderRight: `2px solid ${TABLE_BORDER_COLOR}`,
                ...HEADER_CELL_STYLE,
                ...style,
            }}
        >
            {children}
        </th>
    );
}

interface ColHeaderProps {
    children?: ReactNode;
    style?: CSSProperties;
}

function ColHeader({ children, style = {} }: ColHeaderProps) {
    return (
        <th
            scope="col"
            style={{
                position: "sticky",
                top: 0,
                borderBottom: `2px solid ${TABLE_BORDER_COLOR}`,
                ...HEADER_CELL_STYLE,
                ...style,
            }}
        >
            {children}
        </th>
    );
}

interface BodyCellProps {
    children?: ReactNode;
    style?: CSSProperties;
}

function BodyCell({ children, style = {} }: BodyCellProps) {
    return (
        <td
            style={{
                textAlign: "center",
                borderBottom: `1px solid ${TABLE_BORDER_COLOR}`,
                padding: "5px",
                ...style,
            }}
        >
            {children}
        </td>
    );
}
