import axios from "axios";
import React, { CSSProperties, ReactNode, useEffect, useState } from "react";
import { LeaderboardEntry, Side } from "./types";
import TableView from "./TableView";

const CURRENT_YEAR = 2024;

function Rankings() {
    const [entries, setEntries] = useState<LeaderboardEntry[]>([]);
    const [loading, setLoading] = useState(false);
    const [error, setError] = useState<string | null>(null);

    const getLeaderboard = async () => {
        try {
            const response = await axios.get(
                // "http://localhost:8081/leaderboard"
                "https://api.waroftheringcommunity.net:8080/leaderboard"
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

    useEffect(refresh, []);

    return (
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
                        <TableHeader rowSpan={3}>Games 2024</TableHeader>
                        <TableHeader colSpan={6}>Base Game 2024</TableHeader>
                        <TableHeader colSpan={6}>LoME 2024</TableHeader>
                        <TableHeader side="Free" rowSpan={3}>
                            # FPMV
                        </TableHeader>
                        <TableHeader rowSpan={3}>Tournament Wins</TableHeader>
                    </tr>
                    <tr>
                        {/* Base */}
                        <TableHeader side="Free" colSpan={3}>
                            FP
                        </TableHeader>
                        <TableHeader side="Shadow" colSpan={3}>
                            SP
                        </TableHeader>

                        {/* LoME */}
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

                        {/* FP LOME */}
                        <TableHeader side="Free">Win</TableHeader>
                        <TableHeader side="Free">Loss</TableHeader>
                        <TableHeader>%</TableHeader>

                        {/* SP LOME */}
                        <TableHeader side="Shadow">Win</TableHeader>
                        <TableHeader side="Shadow">Loss</TableHeader>
                        <TableHeader>%</TableHeader>
                    </tr>
                </>
            }
            body={entries.map((entry, i) => (
                <tr key={entry.name}>
                    <TableCell>{i + 1}</TableCell>
                    <TableCell>{entry.country}</TableCell>
                    <TableCell>{entry.name}</TableCell>
                    <TableCell>{entry.averageRating}</TableCell>
                    <TableCell side="Shadow">
                        {entry.currentRatingShadow}
                    </TableCell>
                    <TableCell side="Free">{entry.currentRatingFree}</TableCell>
                    <TableCell>{entry.totalGames}</TableCell>
                    <TableCell>
                        {entry.year === CURRENT_YEAR && entry.yearlyGames}
                    </TableCell>
                    <TableCell side="Free" light>
                        {entry.year === CURRENT_YEAR && entry.yearlyWinsFree}
                    </TableCell>
                    <TableCell side="Free">
                        {entry.year === CURRENT_YEAR && entry.yearlyLossesFree}
                    </TableCell>
                    <TableCell>
                        {entry.year === CURRENT_YEAR &&
                            toPercent(entry.yearlyWinRateFree)}
                    </TableCell>
                    <TableCell side="Shadow" light>
                        {entry.year === CURRENT_YEAR && entry.yearlyWinsShadow}
                    </TableCell>
                    <TableCell side="Shadow">
                        {entry.year === CURRENT_YEAR &&
                            entry.yearlyLossesShadow}
                    </TableCell>
                    <TableCell>
                        {entry.year === CURRENT_YEAR &&
                            toPercent(entry.yearlyWinRateShadow)}
                    </TableCell>
                    <TableCell side="Free" light />
                    <TableCell side="Free" />
                    <TableCell />
                    <TableCell side="Shadow" light />
                    <TableCell side="Shadow" />
                    <TableCell />
                    <TableCell side="Free" light />
                    <TableCell />
                </tr>
            ))}
        />
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
