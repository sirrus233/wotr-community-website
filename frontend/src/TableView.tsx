import React, { CSSProperties, ReactNode } from "react";
import Box from "@mui/joy/Box";
import Button from "@mui/joy/Button";
import Table from "@mui/joy/Table";
import Typography from "@mui/joy/Typography";
import {
    TABLE_REFRESH_BTN_HEIGHT_PX,
    TABLE_ELEMENTS_GAP,
} from "./styles/sizes";

interface Props {
    refresh: () => void;
    error: string | null;
    loading: boolean;
    label: string;
    header: ReactNode;
    body: ReactNode;
    containerStyle?: CSSProperties;
}

export default function TableView({
    refresh,
    error,
    loading,
    label,
    header,
    body,
    containerStyle = {},
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
                    margin: `${TABLE_ELEMENTS_GAP}px 0`,
                }}
            >
                <Button
                    onClick={refresh}
                    sx={{ height: `${TABLE_REFRESH_BTN_HEIGHT_PX}px` }}
                >
                    Refresh
                </Button>
            </Box>

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
                            "& thead > tr:first-child > *:first-child": {
                                pl: 2,
                            },
                            "& tbody > tr > *:first-child": { pl: 2 },
                            "& thead > tr:first-child > *:last-child": {
                                pr: 2,
                            },
                            "& tbody > tr > *:last-child": { pr: 2 },
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
