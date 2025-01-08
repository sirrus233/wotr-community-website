import React, { ReactNode } from "react";
import Typography from "@mui/joy/Typography";
import ExternalLink from "./ExternalLink";
import { SxProps } from "@mui/joy/styles/types";
import { ErrorMessage } from "./constants";

interface Props {
    message: ReactNode;
    sx?: SxProps;
}

export default function ErrorDisplay({ message, sx = {} }: Props) {
    return (
        <Typography color="danger" display="flex" alignItems="center" sx={sx}>
            {message}
            {message === ErrorMessage.Default && (
                <ExternalLink
                    href="https://discord.com/channels/590789678890745857/818775910748258326"
                    style={{ paddingLeft: "4px" }}
                >
                    Discord support channel
                </ExternalLink>
            )}
        </Typography>
    );
}
