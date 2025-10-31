import React from "react";
import Box from "@mui/joy/Box";
import SettlementBadge from "./SettlementBadge";
import colors from "../styles/colors";
import { ProcessedGameReport } from "../types";
import { getStrongholdLabel, strongholdSide } from "../utils";

export default function FreeCaptures(props: { report: ProcessedGameReport }) {
    const { strongholds, expansions } = props.report;

    return (
        <Box display="flex">
            {strongholds
                .filter((sh) => strongholdSide(expansions, sh) === "Shadow")
                .map(getStrongholdLabel)
                .map((label) => (
                    <SettlementBadge
                        key={label}
                        style={{ background: colors.shadowPrimary }}
                    >
                        {label}
                    </SettlementBadge>
                ))}
        </Box>
    );
}
