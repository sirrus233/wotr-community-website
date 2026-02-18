import React, { CSSProperties } from "react";
import Box from "@mui/joy/Box";
import { strongholds } from "../../constants";
import colors from "../../styles/colors";
import { ProcessedGameReport, SettlementLayout, Stronghold } from "../../types";
import {
    getStrongholdAbbreviation,
    getStrongholdLabel,
    isStrongholdInPlay,
    strongholdSide,
} from "../../utils";
import SettlementBadge from "./SettlementBadge";

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
        { style: { background: colors.dwarves }, stronghold: "IronHills" },
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
        /**
         * EredLuin switches on and off; keep in last position to keep analysis
         * views aligned between games
         */
        { style: { background: colors.dwarves }, stronghold: "EredLuin" },
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
        (stronghold) =>
            isStrongholdInPlay(report.expansions, stronghold) &&
            strongholdSide(report.expansions, stronghold) === "Free",
    );

    return (
        <Box display="flex">
            {badgeGroups.map((badges, i) => (
                <Box key={i} display="flex" flexDirection={direction(layout)}>
                    {badges
                        .filter(({ stronghold }) =>
                            allowedStrongholds.includes(stronghold),
                        )
                        .map(({ stronghold, style }) => (
                            <SettlementBadge
                                key={stronghold}
                                style={{
                                    minWidth: maybeSetBadgeWidth(
                                        stronghold,
                                        isAbbreviated,
                                    ),
                                    ...(report.strongholds.includes(stronghold)
                                        ? capturedStyle(style)
                                        : emptyBadgeStyle(layout)),
                                }}
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

function capturedStyle(baseStyle: CSSProperties) {
    return { ...baseStyle, border: "1px solid transparent" };
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

/**
 * Erebor badge slot switches to Iron Hills for FOE; make both badge versions
 * the same width to keep analysis views aligned between games
 */
function maybeSetBadgeWidth(
    stronghold: Stronghold,
    isAbbreviated: boolean,
): string | undefined {
    const ereborBadgeWidth = isAbbreviated ? "25px" : "62px";

    return stronghold === "Erebor" || stronghold === "IronHills"
        ? ereborBadgeWidth
        : undefined;
}
