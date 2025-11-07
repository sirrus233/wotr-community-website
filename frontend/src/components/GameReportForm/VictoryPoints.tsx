import React from "react";
import Typography from "@mui/joy/Typography";
import { Stronghold } from "../../types";
import { strongholdPoints } from "../../utils";

interface VictoryPointsProps {
    strongholds: Stronghold[];
    strongholdOptions: Stronghold[];
}

export default function VictoryPoints({
    strongholds,
    strongholdOptions,
}: VictoryPointsProps) {
    const points = strongholds
        .filter((stronghold) => strongholdOptions.includes(stronghold))
        .map(strongholdPoints)
        .reduce((sum, current) => sum + current, 0);
    return (
        <Typography sx={{ fontWeight: "bold", pb: 2 }}>VP: {points}</Typography>
    );
}
