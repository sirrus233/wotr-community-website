import { CSSProperties, ReactNode } from "react";
import { ErrorMessage } from "../../constants";
import {
    InequalityFilter,
    MenuOption,
    NullableInequalityFilter,
} from "../../types";

export type Option = string | MenuOption<unknown>;

interface CommonFilterProps {
    filterType:
        | "autocomplete"
        | "inequality"
        | "nullableInequality"
        | "boolean";
    placeholder: string;
    errorMessage?: ErrorMessage;
    appliedCount: number;
}

export interface AutocompleteProps<Opt extends Option>
    extends CommonFilterProps {
    filterType: "autocomplete";
    options: Opt[];
    current: Opt[];
    loading: boolean;
    allOption?: Opt;
    emptyOption?: Opt;
    listboxStyle?: CSSProperties;
    onChange: (value: Opt[]) => void;
}

export interface InequalityFilterProps extends CommonFilterProps {
    filterType: "inequality";
    current: InequalityFilter | null;
    min?: number;
    max?: number;
    disabled?: boolean;
    onChange: (value: InequalityFilter | null) => void;
}

export interface NullableInequalityFilterProps extends CommonFilterProps {
    filterType: "nullableInequality";
    current: NullableInequalityFilter | null;
    min?: number;
    max?: number;
    nullLabel: string;
    onChange: (value: NullableInequalityFilter | null) => void;
}

export interface BooleanFilterProps extends CommonFilterProps {
    filterType: "boolean";
    current: boolean | null;
    trueLabel: string;
    falseLabel: string;
    onChange: (value: boolean | null) => void;
}

type Filter<T extends MenuOption<any> = MenuOption<any>> =
    | AutocompleteProps<T>
    | InequalityFilterProps
    | NullableInequalityFilterProps
    | BooleanFilterProps;

interface CommonColHeaderData {
    key: string | number;
    content?: ReactNode;
    span?: number;
    style?: CSSProperties;
}

interface ColHeaderDataA extends CommonColHeaderData {
    filter?: never;
    width?: number;
}

interface ColHeaderDataB<T extends MenuOption<any> = MenuOption<any>>
    extends CommonColHeaderData {
    filter: Filter<T>;
    width: number;
}

export type ColHeaderData<T extends MenuOption<any> = MenuOption<any>> =
    | ColHeaderDataA
    | ColHeaderDataB<T>;

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
    filter?: Filter<T>;
    span?: number;
    style?: CSSProperties;
}
