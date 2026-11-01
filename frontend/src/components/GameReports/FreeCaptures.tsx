import React from "react";
import Box from "@mui/joy/Box";
import colors from "../../styles/colors";
import { ProcessedGameReport } from "../../types";
import { getStrongholdLabel, strongholdSide } from "../../utils";
import Badge from "./Badge";

export default function FreeCaptures(props: { report: ProcessedGameReport }) {
    const { strongholds, expansions } = props.report;

    return (
        <Box display="flex">
            {strongholds
                .filter((sh) => strongholdSide(expansions, sh) === "Shadow")
                .map(getStrongholdLabel)
                .map((label) => (
                    <Badge
                        key={label}
                        sx={{ background: colors.shadowPrimary }}
                    >
                        {label}
                    </Badge>
                ))}
        </Box>
    );
}
