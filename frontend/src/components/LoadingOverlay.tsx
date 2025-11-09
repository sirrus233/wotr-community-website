import React from "react";
import Box from "@mui/joy/Box";
import CircularProgress from "@mui/joy/CircularProgress";

interface Props {
    bounds?: DOMRect;
}

export default function LoadingOverlay({ bounds }: Props) {
    return (
        <Box
            sx={{
                position: bounds ? "fixed" : "absolute",
                zIndex: 20,
                width: bounds?.width || "100%",
                height: bounds?.height || "100%",
                display: "flex",
                justifyContent: "center",
                alignItems: "center",
                background: "rgba(0 0 0 / 40%)",
            }}
        >
            <CircularProgress />
        </Box>
    );
}
