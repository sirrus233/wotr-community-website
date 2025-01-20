import React, { CSSProperties, ReactNode } from "react";
import HelpOutlinedIcon from "@mui/icons-material/HelpOutline";
import Tooltip from "@mui/joy/Tooltip";

interface Props {
    content: ReactNode;
    iconStyle?: CSSProperties;
}

export default function HelpIcon({ content, iconStyle = {} }: Props) {
    return (
        <Tooltip title={content} enterTouchDelay={0}>
            {
                <HelpOutlinedIcon
                    style={{
                        cursor: "pointer",
                        fontSize: "inherit",
                        ...iconStyle,
                    }}
                />
            }
        </Tooltip>
    );
}
