import React, { ReactNode } from "react";
import Typography from "@mui/joy/Typography";
import { SxProps } from "@mui/joy/styles/types";
import { ErrorMessage } from "../constants";
import SupportLink from "./SupportLink";

interface Props {
    message: ReactNode;
    sx?: SxProps;
}

export default function ErrorDisplay({ message, sx = {} }: Props) {
    return (
        <Typography color="danger" sx={sx}>
            {message}
            {message === ErrorMessage.Default && <SupportLink />}
        </Typography>
    );
}
