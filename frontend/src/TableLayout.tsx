import React, { CSSProperties, ReactNode } from "react";
import Box from "@mui/joy/Box";
import Button from "@mui/joy/Button";
import Table from "@mui/joy/Table";
import { SxProps } from "@mui/joy/styles/types";
import { TABLE_BTN_HEIGHT_PX, TABLE_ELEMENTS_GAP } from "./styles/sizes";
import ErrorDisplay from "./ErrorDisplay";
import LoadingOverlay from "./LoadingOverlay";

interface Props {
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

export default function TableLayout({
    refresh,
    error,
    loading,
    label,
    filters,
    table,
    header,
    body,
    containerStyle = {},
    tableStyle = {},
}: Props) {
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
                }}
            >
                <Button
                    onClick={refresh}
                    sx={{ height: `${TABLE_BTN_HEIGHT_PX}px` }}
                >
                    Refresh
                </Button>
            </Box>

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
