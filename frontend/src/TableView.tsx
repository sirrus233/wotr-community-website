import React, { CSSProperties, ReactNode } from "react";
import Box from "@mui/joy/Box";
import Button from "@mui/joy/Button";
import Table from "@mui/joy/Table";
import Typography from "@mui/joy/Typography";
import { SxProps } from "@mui/joy/styles/types";
import { TABLE_BTN_HEIGHT_PX, TABLE_ELEMENTS_GAP } from "./styles/sizes";

interface Props {
    refresh: () => void;
    error: string | null;
    loading: boolean;
    label: string;
    header: ReactNode;
    body: ReactNode;
    filters?: ReactNode;
    containerStyle?: CSSProperties;
    tableStyle?: SxProps;
}

export default function TableView({
    refresh,
    error,
    loading,
    label,
    filters,
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

            {error && <Typography color="danger">{error}</Typography>}

            {loading ? (
                "Loading..."
            ) : (
                <Box
                    sx={{
                        overflow: "auto",
                        boxShadow: "lg",
                        borderRadius: "sm",
                        ...containerStyle,
                    }}
                >
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
                </Box>
            )}
        </Box>
    );
}
