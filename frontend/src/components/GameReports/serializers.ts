import {
    GameReportFilters,
    MenuOption,
    NullableInequalityFilter,
    SerializedNullableInequalityFilter,
    SerializedTimestampFilter,
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

export function serializeTimestampFilter(
    timestampFilter: GameReportFilters["timestamp"],
): SerializedTimestampFilter | null {
    // dates are entered in user's local time
    const [start = null, end = null] = timestampFilter || [];

    // Date.toJSON serializes to a UTC timestamp, matching how timestamps are stored in db
    if (start && end) {
        return { tag: "Between", contents: [start.toJSON(), end.toJSON()] };
    } else if (start) {
        return { tag: "After", contents: start.toJSON() };
    } else if (end) {
        return { tag: "Before", contents: end.toJSON() };
    }

    return null;
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
