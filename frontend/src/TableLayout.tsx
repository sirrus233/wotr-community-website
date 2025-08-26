import React, { CSSProperties, ReactNode } from "react";
import Box from "@mui/joy/Box";
import Button from "@mui/joy/Button";
import Drawer from "@mui/joy/Drawer";
import IconButton from "@mui/joy/IconButton";
import SettingsIcon from "@mui/icons-material/Settings";
import Table from "@mui/joy/Table";
import { SxProps } from "@mui/joy/styles/types";
import { TABLE_BTN_HEIGHT_PX, TABLE_ELEMENTS_GAP } from "./styles/sizes";
import ErrorDisplay from "./ErrorDisplay";
import LoadingOverlay from "./LoadingOverlay";

interface CommonProps {
    refresh: () => void;
    error: string | null;
    loading: boolean;
    label?: string;
    table?: ReactNode;
    header?: ReactNode;
    body?: ReactNode;
    filters?: ReactNode;
    containerStyle?: CSSProperties;
    tableStyle?: SxProps;
}

interface PropsA extends CommonProps {
    settingsPanel?: never;
    settingsOpen?: never;
    toggleSettings?: never;
}

interface PropsB extends CommonProps {
    settingsPanel: ReactNode;
    settingsOpen: boolean;
    toggleSettings: () => void;
}

export default function TableLayout({
    refresh,
    error,
    loading,
    label,
    filters,
    table,
    header,
    body,
    settingsPanel,
    settingsOpen,
    toggleSettings,
    containerStyle = {},
    tableStyle = {},
}: PropsA | PropsB) {
    return (
        <Box
            sx={{
                display: "flex",
                flexDirection: "column",
                mx: 2,
                mb: `${TABLE_ELEMENTS_GAP}px`,
            }}
        >
            <Box
                sx={{
                    display: "flex",
                    width: "100%",
                    alignItems: "center",
                    justifyContent: "center",
                    m: `${TABLE_ELEMENTS_GAP}px 0`,
                    "button + button": { mx: "5px" },
                }}
            >
                <Button
                    onClick={refresh}
                    sx={{ height: `${TABLE_BTN_HEIGHT_PX}px` }}
                >
                    Refresh
                </Button>

                {settingsPanel && (
                    <IconButton
                        variant="solid"
                        color="primary"
                        onClick={toggleSettings}
                    >
                        <SettingsIcon />
                    </IconButton>
                )}
            </Box>

            {settingsOpen && (
                <Drawer
                    open={settingsOpen}
                    onClose={toggleSettings}
                    anchor="right"
                >
                    {settingsPanel}
                </Drawer>
            )}

            {filters && (
                <Box
                    sx={{
                        display: "flex",
                        width: "100%",
                        alignItems: "center",
                        justifyContent: "center",
                        height: `${TABLE_BTN_HEIGHT_PX}px`,
                        mb: `${TABLE_ELEMENTS_GAP}px`,
                    }}
                >
                    {filters}
                </Box>
            )}

            {error && <ErrorDisplay message={error} sx={{ pb: 2 }} />}

            <Box
                sx={{
                    overflow: loading ? "hidden" : "auto",
                    boxShadow: "lg",
                    borderRadius: "sm",
                    position: "relative",
                    ...containerStyle,
                }}
            >
                {loading && <LoadingOverlay />}

                {/* TODO: always pass in custom Table component, then remove this `or` handling */}
                {table || (
                    <Table
                        aria-label={label}
                        noWrap
                        size="sm"
                        variant="outlined"
                        stickyHeader
                        sx={{
                            tableLayout: "auto",
                            border: "none",
                            "& tr > *": { textAlign: "center" },
                            ...tableStyle,
                        }}
                    >
                        <thead>{header}</thead>
                        <tbody>{body}</tbody>
                    </Table>
                )}
            </Box>
        </Box>
    );
}
