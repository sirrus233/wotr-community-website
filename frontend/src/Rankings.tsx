import axios from "axios";
import React, { CSSProperties, ReactNode, useEffect, useState } from "react";
import Box from "@mui/joy/Box";
import Button from "@mui/joy/Button";
import ButtonGroup from "@mui/joy/ButtonGroup";
import { LeaderboardEntry, Side } from "./types";
import { FREE_PRIMARY_COLOR, SHADOW_PRIMARY_COLOR } from "./styles/colors";
import {
    HEADER_HEIGHT_PX,
    HEADER_MARGIN_PX,
    TABLE_REFRESH_BTN_HEIGHT_PX,
    TABLE_ELEMENTS_GAP,
} from "./styles/sizes";
import TableView from "./TableView";

const DEFAULT_YEAR = 2024;
const AVAILABLE_YEARS = [DEFAULT_YEAR, 2023];
const COMING_SOON_YEARS = [2022];

const YEAR_SELECTOR_HEIGHT = 36;
const HEADER_ROW_HEIGHT = 32;

const TABLE_TOP_POSITION =
    HEADER_HEIGHT_PX +
    HEADER_MARGIN_PX +
    YEAR_SELECTOR_HEIGHT +
    TABLE_REFRESH_BTN_HEIGHT_PX +
    TABLE_ELEMENTS_GAP * 2;

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
                { params: { year } }
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
                <ButtonGroup style={{ height: `${YEAR_SELECTOR_HEIGHT}px` }}>
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
                containerStyle={{
                    maxHeight: `calc(100vh - ${TABLE_TOP_POSITION}px - ${TABLE_ELEMENTS_GAP}px)`,
                }}
                header={
                    <>
                        <TableHeaderRow>
                            <TableHeaderCell level={0} rowSpan={3}>
                                Rank
                            </TableHeaderCell>
                            <TableHeaderCell level={0} rowSpan={2} colSpan={2}>
                                Player
                            </TableHeaderCell>
                            <TableHeaderCell
                                level={0}
                                rowSpan={2}
                                style={{ borderBottom: "none" }}
                            >
                                Balanced
                            </TableHeaderCell>
                            <TableHeaderCell
                                level={0}
                                side="Shadow"
                                rowSpan={3}
                            >
                                Shadow Rating
                            </TableHeaderCell>
                            <TableHeaderCell level={0} side="Free" rowSpan={3}>
                                Free Rating
                            </TableHeaderCell>
                            <TableHeaderCell level={0} rowSpan={3}>
                                Games
                            </TableHeaderCell>
                            <TableHeaderCell level={0} rowSpan={3}>
                                Games {year}
                            </TableHeaderCell>
                            <TableHeaderCell level={0} colSpan={6}>
                                Base Game {year}
                            </TableHeaderCell>
                        </TableHeaderRow>

                        <TableHeaderRow>
                            {/* Base */}
                            <TableHeaderCell level={1} side="Free" colSpan={3}>
                                FP
                            </TableHeaderCell>
                            <TableHeaderCell
                                level={1}
                                side="Shadow"
                                colSpan={3}
                            >
                                SP
                            </TableHeaderCell>
                        </TableHeaderRow>

                        <TableHeaderRow>
                            <TableHeaderCell level={2}>Country</TableHeaderCell>
                            <TableHeaderCell level={2}>Name</TableHeaderCell>
                            <TableHeaderCell level={2}>
                                Avg. Rating
                            </TableHeaderCell>

                            {/* FP Base */}
                            <TableHeaderCell level={2} side="Free">
                                Win
                            </TableHeaderCell>
                            <TableHeaderCell level={2} side="Free">
                                Loss
                            </TableHeaderCell>
                            <TableHeaderCell level={2}>%</TableHeaderCell>

                            {/* SP Base */}
                            <TableHeaderCell level={2} side="Shadow">
                                Win
                            </TableHeaderCell>
                            <TableHeaderCell level={2} side="Shadow">
                                Loss
                            </TableHeaderCell>
                            <TableHeaderCell level={2}>%</TableHeaderCell>
                        </TableHeaderRow>
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
                              side === "Shadow"
                                  ? SHADOW_PRIMARY_COLOR
                                  : FREE_PRIMARY_COLOR,
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

interface TableHeaderRowProps {
    children: ReactNode;
}

function TableHeaderRow({ children }: TableHeaderRowProps) {
    return <tr style={{ height: `${HEADER_ROW_HEIGHT}px` }}>{children}</tr>;
}

interface TableHeaderCellProps {
    level: number;
    children?: ReactNode;
    side?: Side;
    rowSpan?: number;
    colSpan?: number;
    style?: CSSProperties;
}

function TableHeaderCell({
    level,
    children,
    side,
    rowSpan,
    colSpan,
    style = { top: `${HEADER_ROW_HEIGHT * level}px` },
}: TableHeaderCellProps) {
    return (
        <th
            rowSpan={rowSpan}
            colSpan={colSpan}
            style={
                side
                    ? {
                          ...style,
                          color:
                              side === "Shadow"
                                  ? SHADOW_PRIMARY_COLOR
                                  : FREE_PRIMARY_COLOR,
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
