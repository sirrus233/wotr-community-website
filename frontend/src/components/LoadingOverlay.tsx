import React, { CSSProperties } from "react";
import Box from "@mui/joy/Box";
import CircularProgress from "@mui/joy/CircularProgress";

interface Props {
    overlayStyle?: CSSProperties;
}

export default function LoadingOverlay({ overlayStyle = {} }: Props) {
    return (
        <Box
            sx={{
                position: "absolute",
                zIndex: 20,
                width: "100%",
                height: "100%",
                display: "flex",
                justifyContent: "center",
                alignItems: "center",
                background: "rgba(0 0 0 / 40%)",
                ...overlayStyle,
            }}
        >
            <CircularProgress />
        </Box>
    );
}
