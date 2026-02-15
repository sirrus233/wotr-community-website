import {
    GameReportFilters,
    MenuOption,
    NullableInequalityFilter,
    SerializedNullableInequalityFilter,
    SerializedVictoryFilter,
} from "../../types";
import { EMPTY_OPTION_ID } from "./constants";

export function serializeNullableInequalityFilter(
    inequalityFilter: NullableInequalityFilter | null,
): SerializedNullableInequalityFilter | null {
    if (inequalityFilter) {
        return inequalityFilter === "NullFilter"
            ? { tag: "NullFilter" }
            : { tag: "ValueFilter", contents: inequalityFilter };
    }
    return null;
}

export function serializeVictoryFilter(
    victoryFilter: GameReportFilters["victory"],
): SerializedVictoryFilter | null {
    return (
        toFilterParam(victoryFilter)?.map(
            (contents): SerializedVictoryFilter[number] => {
                return typeof contents === "string"
                    ? contents === "Free" || contents === "Shadow"
                        ? { tag: "VictorySideFilter", contents }
                        : { tag: "VictoryKindFilter", contents }
                    : { tag: "VictoryComboFilter", contents };
            },
        ) || null
    );
}

export function getReportsOffset(currentPage: number, perPageCount: number) {
    return (currentPage - 1) * perPageCount;
}

export function toFilterParam<T>(options: MenuOption<T>[]): T[] | null {
    return options.find(({ id }) => id === EMPTY_OPTION_ID)
        ? []
        : nullifyEmpty(options.map(({ id }) => id));
}

export function nullifyEmpty<T>(arr: T[]) {
    return arr.length ? arr : null;
}
