import React, { CSSProperties, ReactNode } from "react";
import Box from "@mui/joy/Box";

interface Props {
    children: ReactNode;
    style?: CSSProperties;
}

export default function SettlementBadge({ children, style }: Props) {
    return (
        <Box
            boxSizing="border-box"
            display="flex"
            alignItems="center"
            justifyContent="center"
            whiteSpace="nowrap"
            overflow="hidden"
            textOverflow="ellipsis"
            lineHeight="1em"
            px="5px"
            py="3px"
            m="1px"
            borderRadius="5px"
            color="white"
            sx={style}
        >
            {children}
        </Box>
    );
}
