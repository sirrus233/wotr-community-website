import React, { ReactNode } from "react";
import ViewIcon from "@mui/icons-material/Visibility";
import IconButton from "@mui/joy/IconButton";
import { FlexBox } from "../../styles/styledComponents";

interface Props {
    children: ReactNode;
    setSettingsOpen: (open: boolean) => void;
}

export default function ColHeaderWithSettingsBtn({
    children,
    setSettingsOpen,
}: Props) {
    return (
        <FlexBox alignItems="center" justifyContent="center">
            <FlexBox mr="5px">{children}</FlexBox>
            <IconButton
                aria-label="Open settings"
                size="sm"
                color="primary"
                sx={{ height: "1em", minHeight: "fit-content", py: "2px" }}
                onClick={() => setSettingsOpen(true)}
            >
                <ViewIcon />
            </IconButton>
        </FlexBox>
    );
}
