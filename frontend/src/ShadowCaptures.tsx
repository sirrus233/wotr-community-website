import React, { CSSProperties } from "react";
import Box from "@mui/joy/Box";
import { strongholds } from "./constants";
import SettlementBadge from "./SettlementBadge";
import colors from "./styles/colors";
import { ProcessedGameReport, SettlementLayout, Stronghold } from "./types";
import {
    getStrongholdAbbreviation,
    getStrongholdLabel,
    strongholdSide,
} from "./utils";

interface Badge {
    stronghold: Stronghold;
    style: CSSProperties;
}

const badgeGroups: Badge[][] = [
    [
        { style: { background: colors.gondor }, stronghold: "MinasTirith" },
        { style: { background: colors.gondor }, stronghold: "Pelargir" },
        { style: { background: colors.gondor }, stronghold: "DolAmroth" },
    ],
    [
        { style: { background: colors.north }, stronghold: "Dale" },
        { style: { background: colors.dwarves }, stronghold: "Erebor" },
        { style: { background: colors.elves }, stronghold: "WoodlandRealm" },
    ],
    [
        { style: { background: colors.rohan }, stronghold: "HelmsDeep" },
        { style: { background: colors.rohan }, stronghold: "Edoras" },
    ],
    [{ style: { background: colors.elves }, stronghold: "Lorien" }],
    [
        { style: { background: colors.elves }, stronghold: "Rivendell" },
        { style: { background: colors.north }, stronghold: "Shire" },
        { style: { background: colors.elves }, stronghold: "GreyHavens" },
    ],
    [
        { style: { background: colors.dwarves }, stronghold: "EredLuin" },
        { style: { background: colors.dwarves }, stronghold: "IronHills" },
    ],
];

interface Props {
    report: ProcessedGameReport;
    layout: SettlementLayout;
    isAbbreviated: boolean;
}

export default function ShadowCaptures({
    report,
    layout,
    isAbbreviated,
}: Props) {
    const allowedStrongholds: Stronghold[] = strongholds.filter(
        (stronghold) => strongholdSide(report.expansions, stronghold) === "Free"
    );

    return (
        <Box display="flex">
            {badgeGroups.map((badges, i) => (
                <Box key={i} display="flex" flexDirection={direction(layout)}>
                    {badges
                        .filter(({ stronghold }) =>
                            allowedStrongholds.includes(stronghold)
                        )
                        .map(({ stronghold, style }) => (
                            <SettlementBadge
                                key={stronghold}
                                style={
                                    report.strongholds.includes(stronghold)
                                        ? {
                                              ...style,
                                              border: "1px solid transparent",
                                          }
                                        : emptyBadgeStyle(layout)
                                }
                            >
                                {isAbbreviated
                                    ? getStrongholdAbbreviation(stronghold)
                                    : getStrongholdLabel(stronghold)}
                            </SettlementBadge>
                        ))}
                </Box>
            ))}
        </Box>
    );
}

function direction(layout: SettlementLayout): "row" | "column" {
    switch (layout) {
        case "Standard":
        case "Horizontal Analysis":
            return "row";
        case "Vertical Analysis":
            return "column";
    }
}

function emptyBadgeStyle(layout: SettlementLayout): CSSProperties {
    switch (layout) {
        case "Standard":
            return { display: "none" };
        case "Horizontal Analysis":
        case "Vertical Analysis":
            return {
                color: "transparent",
                background: "white",
                border: "1px solid #eee",
            };
    }
}
