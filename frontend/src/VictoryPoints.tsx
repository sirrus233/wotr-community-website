import React from "react";
import Typography from "@mui/joy/Typography";
import { Stronghold } from "./types";

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

function strongholdPoints(stronghold: Stronghold): 1 | 2 {
    switch (stronghold) {
        case "Shire":
        case "Edoras":
        case "Dale":
        case "Pelargir":
        case "EredLuin":
        case "IronHills":
        case "Angmar":
        case "FarHarad":
        case "SouthRhun":
            return 1;
        case "Rivendell":
        case "GreyHavens":
        case "HelmsDeep":
        case "Lorien":
        case "WoodlandRealm":
        case "MinasTirith":
        case "DolAmroth":
        case "Erebor":
        case "MountGundabad":
        case "Moria":
        case "DolGuldur":
        case "Orthanc":
        case "Morannon":
        case "BaradDur":
        case "MinasMorgul":
        case "Umbar":
            return 2;
    }
}
