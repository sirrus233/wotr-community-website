import axios from "axios";
import React, { ReactNode, useEffect, useState } from "react";
import Box from "@mui/joy/Box";
import Typography from "@mui/joy/Typography";
import {
    Competition,
    Expansion,
    League,
    Match,
    ProcessedGameReport,
    Side,
    Stronghold,
    Victory,
} from "./types";
import { FREE_ACCENT_COLOR, SHADOW_PRIMARY_COLOR } from "./styles/colors";
import {
    HEADER_HEIGHT_PX,
    HEADER_MARGIN_PX,
    TABLE_BTN_HEIGHT_PX,
    TABLE_ELEMENTS_GAP,
} from "./styles/sizes";
import {
    getExpansionLabel,
    getLeagueLabel,
    getStrongholdLabel,
    strongholdPoints,
    strongholdSide,
} from "./utils";
import TableView from "./TableView";
import ExternalLink from "./ExternalLink";

const TABLE_TOP_POSITION =
    HEADER_HEIGHT_PX +
    HEADER_MARGIN_PX +
    TABLE_BTN_HEIGHT_PX +
    TABLE_ELEMENTS_GAP * 2;

export default function GameReports() {
    const [reports, setReports] = useState<ProcessedGameReport[]>([]);
    const [loading, setLoading] = useState(false);
    const [error, setError] = useState<string | null>(null);

    const getReports = async () => {
        try {
            const response = await axios.get(
                // "http://localhost:8081/leaderboard"
                "https://api.waroftheringcommunity.net:8080/reports"
            );
            setReports(response.data.reports);
        } catch (error) {
            setError("Something went wrong");
            console.error(error);
        }
        setLoading(false);
    };

    const refresh = () => {
        setError(null);
        setLoading(true);
        getReports();
    };

    useEffect(refresh, []);

    return (
        <Box>
            <TableView
                refresh={refresh}
                error={error}
                loading={loading}
                label="Game Reports"
                containerStyle={{
                    maxHeight: `calc(100vh - ${TABLE_TOP_POSITION}px - ${TABLE_ELEMENTS_GAP}px)`,
                }}
                tableStyle={{
                    "& thead > tr:first-child > *:first-child": {
                        pl: 2,
                    },
                    "& tbody > tr > *:first-child": { pl: 2 },
                    "& thead > tr:first-child > *:last-child": {
                        pr: 2,
                    },
                    "& tbody > tr > *:last-child": { pr: 2 },
                }}
                header={
                    <tr>
                        <th />
                        <th />
                        <th>No.</th>
                        <th>Pairing</th>
                        <th>Timestamp</th>
                        <th>Turn</th>
                        <th>Winner</th>
                        <th>Loser</th>
                        <th>Game Type</th>
                        <th>Victory Type</th>
                        <th>Competition Type</th>
                        <th>Expansions</th>
                        <th>Tokens</th>
                        <th>Dwarven Rings</th>
                        <th>Corruption</th>
                        <th>Mordor</th>
                        <th>Aragorn</th>
                        <th>Treebeard</th>
                        <th>Initial Eyes</th>
                        <th>SP-Captured Settlements</th>
                        <th>SPVP</th>
                        <th>FP-Captured Settlements</th>
                        <th>FPVP</th>
                        <th>Interest Rating</th>
                        <th>Comments</th>
                        <th>Game Log</th>
                    </tr>
                }
                body={reports.map((report, i) => (
                    <tr key={report.rid}>
                        <RowAccent side={report.side} />
                        <td style={{ padding: "0 10px 0 0" }}>
                            {report.side === "Free" ? "💍" : "🌋"}
                        </td>

                        {/* TODO: account for pagination */}
                        <td style={{ fontWeight: "bold" }}>
                            {reports.length - i}
                        </td>

                        <td>
                            {[report.winner, report.loser].sort().join("-")}
                        </td>
                        <td>
                            {Intl.DateTimeFormat("en-GB").format(
                                new Date(Date.parse(report.timestamp))
                            )}
                        </td>
                        <td>{report.turns}</td>
                        <td>{report.winner}</td>
                        <td>{report.loser}</td>
                        <td>{summarizeGameType(report.expansions)}</td>
                        <td>
                            {summarizeVictoryType(report.side, report.victory)}
                        </td>
                        <td>
                            {summarizeCompetitionType(
                                report.match,
                                report.competition,
                                report.league
                            )}
                        </td>
                        <td>
                            {report.expansions
                                .map(getExpansionLabel)
                                .join(", ")}
                        </td>
                        <td>{report.actionTokens}</td>
                        <td>{report.dwarvenRings}</td>
                        <td>{report.corruption}</td>
                        <td>{report.mordor}</td>
                        <td>{report.aragornTurn}</td>
                        <td>{report.treebeard && "✓"}</td>
                        <td>{report.initialEyes}</td>
                        <td>
                            {summarizeCapturedSettlements(
                                report.strongholds,
                                report.expansions,
                                "Free"
                            )}
                        </td>
                        <td>
                            {countVictoryPoints(
                                report.strongholds,
                                report.expansions,
                                "Free"
                            )}
                        </td>
                        <td>
                            {summarizeCapturedSettlements(
                                report.strongholds,
                                report.expansions,
                                "Shadow"
                            )}
                        </td>
                        <td>
                            {countVictoryPoints(
                                report.strongholds,
                                report.expansions,
                                "Shadow"
                            )}
                        </td>
                        <td>{report.interestRating}</td>
                        <WrappedCell>{report.comment}</WrappedCell>
                        <td>
                            {report.logFile && (
                                <ExternalLink isDownload href={report.logFile}>
                                    Download Report
                                </ExternalLink>
                            )}
                        </td>
                    </tr>
                ))}
            />
        </Box>
    );
}

interface WrappedCellProps {
    children: ReactNode;
}

function WrappedCell({ children }: WrappedCellProps) {
    return (
        <td style={{ whiteSpace: "wrap" }}>
            <Typography width="400px">{children}</Typography>
        </td>
    );
}

interface RowAccentProps {
    side: Side;
}

function RowAccent({ side }: RowAccentProps) {
    return (
        <td style={{ padding: 0 }}>
            <div
                style={{
                    height: "100%",
                    width: "5px",
                    borderBottom: "1px solid white",
                    background:
                        side === "Free"
                            ? FREE_ACCENT_COLOR
                            : SHADOW_PRIMARY_COLOR,
                }}
            />
        </td>
    );
}

function summarizeGameType(expansions: Expansion[]) {
    return expansions.some(isGameTypeExpansion)
        ? expansions.filter(isGameTypeExpansion).join("+")
        : "Base";
}

function isGameTypeExpansion(expansion: Expansion) {
    return ["KoME", "WoME", "LoME"].includes(expansion);
}

function summarizeVictoryType(side: Side, victory: Victory) {
    return (
        <Box
            style={{
                color: "white",
                background:
                    side === "Free" ? FREE_ACCENT_COLOR : SHADOW_PRIMARY_COLOR,
                borderRadius: "12px",
                padding: "3px 8px",
            }}
        >
            {`${side} ${
                side === "Shadow" && victory === "Ring"
                    ? "Corruption"
                    : victory === "Concession"
                    ? "via Concession"
                    : victory
            }`}
        </Box>
    );
}

function summarizeCompetitionType(
    match: Match,
    competition: Competition[],
    league: League | null
) {
    return [
        match === "Ranked" ? "Ladder" : "Friendly",
        competition.includes("Tournament") ? "Tournament" : "",
        competition.includes("League")
            ? `League${league ? ` (${getLeagueLabel(league)})` : ""}`
            : "",
    ]
        .filter(Boolean)
        .join(", ");
}

function summarizeCapturedSettlements(
    strongholds: Stronghold[],
    expansions: Expansion[],
    side: Side
) {
    return strongholds
        .filter((stronghold) => strongholdSide(expansions, stronghold) === side)
        .map(getStrongholdLabel)
        .join(" • ");
}

function countVictoryPoints(
    strongholds: Stronghold[],
    expansions: Expansion[],
    side: Side
) {
    return strongholds
        .filter((stronghold) => strongholdSide(expansions, stronghold) === side)
        .map(strongholdPoints)
        .reduce((sum, points) => sum + points, 0);
}
