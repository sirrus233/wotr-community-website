import React from "react";
import Box from "@mui/joy/Box";
import colors from "../../styles/colors";
import {
    Competition,
    Expansion,
    Match,
    Side,
    Sovereigns,
    Stronghold,
    Victory,
    SameElements,
} from "../../types";
import {
    strongholdPoints,
    strongholdSide,
    getExpansionLabel,
    toTitleCase,
} from "../../utils";
import { expansions } from "../../constants";

export function formatExpansions(values: readonly Expansion[]): string {
    const expansionOrder = [
        "LoME",
        "WoME",
        "KoME",
        "Cities",
        "FateOfErebor",
        "ReturnOfTheKing",
        "Treebeard",
    ] as const;

    expansionOrder satisfies SameElements<
        typeof expansionOrder,
        typeof expansions
    >;

    const indices = new Map(expansionOrder.map((v, i) => [v, i]));

    return [...values]
        .sort((a, b) => (indices.get(a) ?? 0) - (indices.get(b) ?? 0))
        .map(getExpansionLabel)
        .join(", ");
}

export function toVictoryTypeLabel(side: Side, victory: Victory): string {
    return `${side} ${
        side === "Shadow" && victory === "Ring"
            ? "Corruption"
            : victory === "Concession"
              ? "via Concession"
              : victory
    }`;
}

export function toVictoryKindLabel(victory: Victory): string {
    switch (victory) {
        case "Ring":
            return "Ring/Corruption";
        case "Military":
        case "Concession":
            return victory;
    }
}

export function countVictoryPoints(
    strongholds: Stronghold[],
    expansions: Expansion[],
    side: Side,
) {
    return strongholds
        .filter((stronghold) => strongholdSide(expansions, stronghold) === side)
        .map(strongholdPoints)
        .reduce((sum, points) => sum + points, 0);
}

export function summarizeVictoryType(side: Side, victory: Victory) {
    return (
        <Box
            style={{
                color: "white",
                background:
                    side === "Free" ? colors.freeAccent : colors.shadowPrimary,
                borderRadius: "12px",
                padding: "3px 8px",
            }}
        >
            {toVictoryTypeLabel(side, victory)}
        </Box>
    );
}

export function summarizeCompetitionType(
    match: Match,
    competition: Competition[],
) {
    return [match === "Rated" ? "Ladder" : "Friendly", ...competition]
        .filter(Boolean)
        .join(", ");
}

export function summarizeGameType(expansions: Expansion[]) {
    return expansions.some(isGameTypeExpansion)
        ? expansions.filter(isGameTypeExpansion).join("+")
        : "Base";
}

export function summarizeSovereigns(sovereigns: Sovereigns | null) {
    return sovereigns
        ? Object.entries(sovereigns)
              .map(
                  ([sovereign, state]) =>
                      `${toTitleCase(sovereign)}: ${state.status}${state.died ? ", died" : ""}`,
              )
              .join(" • ")
        : sovereigns;
}

function isGameTypeExpansion(expansion: Expansion) {
    return ["KoME", "WoME", "LoME"].includes(expansion);
}
