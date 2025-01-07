import React, { CSSProperties, ReactNode } from "react";
import DownloadIcon from "@mui/icons-material/Download";
import MuiLink from "@mui/joy/Link";
import OpenInNewIcon from "@mui/icons-material/OpenInNew";

interface ExternalLinkProps {
    href: string;
    isDownload?: boolean;
    style?: CSSProperties;
    children: ReactNode;
}

export default function ExternalLink({
    href,
    isDownload = false,
    style = {},
    children,
}: ExternalLinkProps) {
    const Icon = isDownload ? DownloadIcon : OpenInNewIcon;
    return (
        <MuiLink
            href={href}
            target="_blank"
            rel="noopener noreferrer"
            style={style}
        >
            <Icon sx={{ paddingRight: "5px" }} />
            {children}
        </MuiLink>
    );
}
