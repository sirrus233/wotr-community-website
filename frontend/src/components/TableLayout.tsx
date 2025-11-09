import React, { ReactNode, useRef } from "react";
import Box from "@mui/joy/Box";
import Button from "@mui/joy/Button";
import Drawer from "@mui/joy/Drawer";
import IconButton from "@mui/joy/IconButton";
import SettingsIcon from "@mui/icons-material/Settings";
import Table from "@mui/joy/Table";
import { SxProps } from "@mui/joy/styles/types";
import sizes from "../styles/sizes";
import ErrorDisplay from "./ErrorDisplay";
import LoadingOverlay from "./LoadingOverlay";

const BUTTON_HEIGHT = "36px";

interface Props {
    refresh: () => void;
    error: string | null;
    loading: boolean;
    label?: string;
    table?: ReactNode;
    header?: ReactNode;
    body?: ReactNode;
    filters?: ReactNode;
    tableStyle?: SxProps;
    settingsProps?: {
        panel: ReactNode;
        isOpen: boolean;
        togglePanel: () => void;
    };
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
    tableStyle = {},
    settingsProps,
}: Props) {
    const tableContainerRef = useRef<HTMLDivElement>(null);

    return (
        <Box
            sx={{
                display: "flex",
                flexDirection: "column",
                flex: 1,
                minHeight: 0,
                mx: 2,
                mb: `${sizes.tableElementsGap}px`,
            }}
        >
            <Box
                sx={{
                    display: "flex",
                    width: "100%",
                    alignItems: "center",
                    justifyContent: "center",
                    m: `${sizes.tableElementsGap}px 0`,
                    "button + button": { mx: "5px" },
                }}
            >
                <Button onClick={refresh} sx={{ height: BUTTON_HEIGHT }}>
                    Refresh
                </Button>

                {settingsProps && (
                    <IconButton
                        variant="solid"
                        color="primary"
                        onClick={settingsProps.togglePanel}
                    >
                        <SettingsIcon />
                    </IconButton>
                )}
            </Box>

            {settingsProps && (
                <Drawer
                    open={settingsProps.isOpen}
                    onClose={settingsProps.togglePanel}
                    anchor="right"
                >
                    {settingsProps.panel}
                </Drawer>
            )}

            {filters && (
                <Box
                    sx={{
                        display: "flex",
                        width: "100%",
                        alignItems: "center",
                        justifyContent: "center",
                        height: BUTTON_HEIGHT,
                        mb: `${sizes.tableElementsGap}px`,
                    }}
                >
                    {filters}
                </Box>
            )}

            {error && <ErrorDisplay message={error} sx={{ pb: 2 }} />}

            <Box
                ref={tableContainerRef}
                sx={{
                    overflow: loading ? "hidden" : "auto",
                    boxShadow: "lg",
                    borderRadius: "sm",
                    position: "relative",
                }}
            >
                {loading && (
                    <LoadingOverlay
                        bounds={tableContainerRef.current?.getBoundingClientRect()}
                    />
                )}

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
