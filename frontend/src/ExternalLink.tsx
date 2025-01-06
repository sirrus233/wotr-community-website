import React, { ReactNode, useEffect, useState } from "react";
import DownloadIcon from "@mui/icons-material/Download";
import MuiLink from "@mui/joy/Link";
import OpenInNewIcon from "@mui/icons-material/OpenInNew";

interface ExternalLinkProps {
    href: string;
    isDownload?: boolean;
    children: ReactNode;
}

export default function ExternalLink({
    href,
    isDownload = false,
    children,
}: ExternalLinkProps) {
    const Icon = isDownload ? DownloadIcon : OpenInNewIcon;
    return (
        <MuiLink href={href} target="_blank" rel="noopener noreferrer">
            <Icon sx={{ paddingRight: "5px" }} />
            {children}
        </MuiLink>
    );
}
