import React from "react";
import Box from "@mui/joy/Box";
import colors from "../../styles/colors";
import {
    Competition,
    Expansion,
    Match,
    Side,
    Stronghold,
    Victory,
} from "../../types";
import {
    strongholdPoints,
    strongholdSide,
    getExpansionLabel,
} from "../../utils";
import { expansions } from "../../constants";

const EXPANSION_ORDER_INDEX: Record<Expansion, number> = Object.fromEntries(
    expansions.map((exp, i) => [exp, i]),
) as Record<Expansion, number>;

export function formatExpansions(values: readonly Expansion[]): string {
    return values
        .slice()
        .sort((a, b) => EXPANSION_ORDER_INDEX[a] - EXPANSION_ORDER_INDEX[b])
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

function isGameTypeExpansion(expansion: Expansion) {
    return ["KoME", "WoME", "LoME"].includes(expansion);
}
