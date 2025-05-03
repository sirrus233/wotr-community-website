import { CSSProperties, ReactNode } from "react";
import { TableFilterProps } from "./TableFilter";
import { MenuOption } from "../types";

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
