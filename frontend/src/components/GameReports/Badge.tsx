import React, { ReactNode } from "react";
import Box from "@mui/joy/Box";
import { SxProps } from "@mui/joy/styles/types";

interface Props {
    children: ReactNode;
    sx?: SxProps;
}

export default function Badge({ children, sx }: Props) {
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
            sx={sx}
        >
            {children}
        </Box>
    );
}
