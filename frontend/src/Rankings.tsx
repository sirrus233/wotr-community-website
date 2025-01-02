import axios from "axios";
import React, { CSSProperties, ReactNode, useEffect, useState } from "react";
import Box from "@mui/joy/Box";
import Button from "@mui/joy/Button";
import ButtonGroup from "@mui/joy/ButtonGroup";
import { LeaderboardEntry, Side } from "./types";
import TableView from "./TableView";

const DEFAULT_YEAR = 2024;
const AVAILABLE_YEARS = [DEFAULT_YEAR];
const COMING_SOON_YEARS = [2023, 2022];

function Rankings() {
    const [entries, setEntries] = useState<LeaderboardEntry[]>([]);
    const [loading, setLoading] = useState(false);
    const [error, setError] = useState<string | null>(null);
    const [year, setYear] = useState(DEFAULT_YEAR);

    const getLeaderboard = async () => {
        try {
            const response = await axios.get(
                // "http://localhost:8081/leaderboard"
                "https://api.waroftheringcommunity.net:8080/leaderboard",
                { data: { year } }
            );
            setEntries(response.data.entries);
        } catch (error) {
            setError("Something went wrong");
            console.error(error);
        }
        setLoading(false);
    };

    const refresh = () => {
        setError(null);
        setLoading(true);
        getLeaderboard();
    };

    useEffect(refresh, [year]);

    return (
        <Box>
            <Box
                sx={{
                    display: "flex",
                    justifyContent: "center",
                    width: "100%",
                }}
            >
                <ButtonGroup>
                    {AVAILABLE_YEARS.map((yearOption) => (
                        <Button
                            key={yearOption}
                            variant="plain"
                            onClick={() => setYear(yearOption)}
                            sx={{
                                fontWeight:
                                    year === yearOption ? "bold" : "normal",
                            }}
                        >
                            {yearOption}
                        </Button>
                    ))}
                    {COMING_SOON_YEARS.map((yearOption) => (
                        <Button key={yearOption} disabled variant="plain">
                            {yearOption}
                        </Button>
                    ))}
                </ButtonGroup>
            </Box>

            <TableView
                refresh={refresh}
                error={error}
                loading={loading}
                label="Rankings"
                header={
                    <>
                        <tr>
                            <TableHeader rowSpan={3}>Rank</TableHeader>
                            <TableHeader rowSpan={2} colSpan={2}>
                                Player
                            </TableHeader>
                            <TableHeader
                                rowSpan={2}
                                style={{ borderBottom: "none" }}
                            >
                                Balanced
                            </TableHeader>
                            <TableHeader side="Shadow" rowSpan={3}>
                                Shadow Rating
                            </TableHeader>
                            <TableHeader side="Free" rowSpan={3}>
                                Free Rating
                            </TableHeader>
                            <TableHeader rowSpan={3}>Games</TableHeader>
                            <TableHeader rowSpan={3}>Games {year}</TableHeader>
                            <TableHeader colSpan={6}>
                                Base Game {year}
                            </TableHeader>
                        </tr>
                        <tr>
                            {/* Base */}
                            <TableHeader side="Free" colSpan={3}>
                                FP
                            </TableHeader>
                            <TableHeader side="Shadow" colSpan={3}>
                                SP
                            </TableHeader>
                        </tr>
                        <tr>
                            <TableHeader>Country</TableHeader>
                            <TableHeader>Name</TableHeader>
                            <TableHeader>Avg. Rating</TableHeader>

                            {/* FP Base */}
                            <TableHeader side="Free">Win</TableHeader>
                            <TableHeader side="Free">Loss</TableHeader>
                            <TableHeader>%</TableHeader>

                            {/* SP Base */}
                            <TableHeader side="Shadow">Win</TableHeader>
                            <TableHeader side="Shadow">Loss</TableHeader>
                            <TableHeader>%</TableHeader>
                        </tr>
                    </>
                }
                body={entries.map((entry, i) => (
                    <tr key={entry.pid}>
                        <TableCell>{i + 1}</TableCell>
                        <TableCell>{entry.country}</TableCell>
                        <TableCell>{entry.name}</TableCell>
                        <TableCell>{entry.averageRating}</TableCell>
                        <TableCell side="Shadow">
                            {entry.currentRatingShadow}
                        </TableCell>
                        <TableCell side="Free">
                            {entry.currentRatingFree}
                        </TableCell>
                        <TableCell>{entry.totalGames}</TableCell>
                        <TableCell>{entry.yearlyGames}</TableCell>
                        <TableCell side="Free" light>
                            {entry.yearlyWinsFree}
                        </TableCell>
                        <TableCell side="Free">
                            {entry.yearlyLossesFree}
                        </TableCell>
                        <TableCell>
                            {toPercent(entry.yearlyWinRateFree)}
                        </TableCell>
                        <TableCell side="Shadow" light>
                            {entry.yearlyWinsShadow}
                        </TableCell>
                        <TableCell side="Shadow">
                            {entry.yearlyLossesShadow}
                        </TableCell>
                        <TableCell>
                            {toPercent(entry.yearlyWinRateShadow)}
                        </TableCell>
                    </tr>
                ))}
            />
        </Box>
    );
}

export default Rankings;

interface TableCellProps {
    children?: ReactNode;
    side?: Side;
    light?: boolean;
    style?: CSSProperties;
}

function TableCell({
    children,
    side,
    light = false,
    style = {},
}: TableCellProps) {
    return (
        <td
            style={
                side
                    ? {
                          ...style,
                          backgroundColor:
                              side === "Shadow" ? "#990200" : "#0b5394",
                          color: "white",
                          opacity: light ? "60%" : "100%",
                      }
                    : style
            }
        >
            {children}
        </td>
    );
}

interface TableHeaderProps {
    children?: ReactNode;
    side?: Side;
    rowSpan?: number;
    colSpan?: number;
    style?: CSSProperties;
}

function TableHeader({
    children,
    side,
    rowSpan,
    colSpan,
    style = {},
}: TableHeaderProps) {
    return (
        <th
            rowSpan={rowSpan}
            colSpan={colSpan}
            style={
                side
                    ? {
                          ...style,
                          color: side === "Shadow" ? "#990200" : "#0b5394",
                      }
                    : style
            }
        >
            {children}
        </th>
    );
}

function toPercent(num: number) {
    return Math.round(num * 100) + "%";
}
