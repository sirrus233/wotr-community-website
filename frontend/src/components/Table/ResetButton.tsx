import React from "react";
import ClearIcon from "@mui/icons-material/CloseRounded";
import IconButton from "@mui/joy/IconButton";

interface Props {
    reset: () => void;
}

export default function ResetButton({ reset }: Props) {
    return (
        <IconButton
            size="sm"
            onMouseDown={(e) => e.stopPropagation()}
            onClick={reset}
            sx={{ minWidth: 0, minHeight: 0, lineHeight: 0, p: "1px" }}
        >
            <ClearIcon />
        </IconButton>
    );
}
