import React, { CSSProperties } from "react";
import Box from "@mui/joy/Box";
import { sovereigns } from "../../constants";
import colors from "../../styles/colors";
import { FlexBox } from "../../styles/styledComponents";
import { Sovereign, Sovereigns } from "../../types";
import { toTitleCase } from "../../utils";
import Badge from "./Badge";

const HEIGHT_CSS_VAR = "--sovereign-badge-height";
const WIDTH_CSS_VAR = "--sovereign-badge-width";

interface Props {
    reportedSovereigns: Sovereigns;
    hasStatus: boolean;
    isAbbreviated: boolean;
}

export default function Sovereigns({
    reportedSovereigns,
    hasStatus,
    isAbbreviated,
}: Props) {
    return (
        <FlexBox>
            {sovereigns.map((sovereign) => (
                <Badge
                    key={sovereign}
                    sx={{
                        fontSize: hasStatus ? "0.85em" : "1em",
                        [HEIGHT_CSS_VAR]: `calc(${hasStatus ? 1.5 : 1}lh + 6px)`,
                        [WIDTH_CSS_VAR]: `${isAbbreviated ? "30px" : "70px"}`,
                        height: `var(${HEIGHT_CSS_VAR})`,
                        width: `var(${WIDTH_CSS_VAR})`,
                        boxSizing: "border-box",
                        py: 0,
                        position: "relative",
                        display: "flex",
                        flexDirection: "column",
                        ...styleSovereignBadge(reportedSovereigns[sovereign]),
                    }}
                >
                    {reportedSovereigns[sovereign].died && <CrossOut />}

                    {toTitleCase(
                        isAbbreviated ? abbreviate(sovereign) : sovereign,
                    )}

                    {hasStatus && (
                        <FlexBox fontSize="0.9em">
                            {reportedSovereigns[sovereign].status.slice(
                                0,
                                isAbbreviated ? 1 : undefined,
                            )}
                        </FlexBox>
                    )}
                </Badge>
            ))}
        </FlexBox>
    );
}

function CrossOut() {
    return (
        <>
            <DiagonalLine direction="left" />
            <DiagonalLine direction="right" />
        </>
    );
}

function DiagonalLine({ direction }: { direction: "left" | "right" }) {
    return (
        <Box
            sx={{
                position: "absolute",
                boxSizing: "border-box",
                height: `var(${HEIGHT_CSS_VAR})`,
                width: `hypot(var(${HEIGHT_CSS_VAR}), var(${WIDTH_CSS_VAR}))`,
                borderTop: "1px solid black",
                [direction]: 0,
                transformOrigin: `top ${direction}`,
                transform: `rotate(calc(${direction === "left" ? 1 : -1} * atan2(var(${HEIGHT_CSS_VAR}), var(${WIDTH_CSS_VAR}))))`,
            }}
        />
    );
}

function styleSovereignBadge(
    sovereignState: Sovereigns[Sovereign],
): CSSProperties {
    switch (sovereignState.status) {
        case "Neither":
            return {
                color: colors.sovereignNeither,
                background: "white",
                border: "1px solid #eee",
            };
        case "Awakened":
            return { background: colors.sovereignAwakened };
        case "Corrupted":
            return { background: colors.sovereignCorrupted };
    }
}

function abbreviate(sovereign: Sovereign): string {
    switch (sovereign) {
        case "thranduil":
            return "thr";
        case "brand":
            return "br";
        case "dain":
            return "da";
        case "denethor":
            return "de";
        case "theoden":
            return "the";
    }
}
