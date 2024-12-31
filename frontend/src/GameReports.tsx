import axios from "axios";
import React, { useEffect, useState } from "react";
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
import {
    getExpansionLabel,
    getLeagueLabel,
    getStrongholdLabel,
    strongholdPoints,
    strongholdSide,
} from "./utils";
import TableView from "./TableView";

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
        <TableView
            refresh={refresh}
            error={error}
            loading={loading}
            label="Game Reports"
            header={
                <tr>
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
                    <th>Winning Side</th>
                    <th>League</th>
                    <th>Tournament</th>
                </tr>
            }
            body={reports.map((report) => (
                <tr key={report.rid}>
                    <td>{[report.winner, report.loser].sort().join("-")}</td>
                    <td>
                        {Intl.DateTimeFormat("en-GB").format(
                            new Date(Date.parse(report.timestamp))
                        )}
                    </td>
                    <td>{report.turns}</td>
                    <td>{report.winner}</td>
                    <td>{report.loser}</td>
                    <td>{summarizeGameType(report.expansions)}</td>
                    <td>{summarizeVictoryType(report.side, report.victory)}</td>
                    <td>
                        {summarizeCompetitionType(
                            report.match,
                            report.competition,
                            report.league
                        )}
                    </td>
                    <td>
                        {report.expansions.map(getExpansionLabel).join(", ")}
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
                    <td>{report.comments}</td>
                    <td></td>
                    <td>{report.side}</td>
                    <td>{report.league && "✓"}</td>
                    <td>{report.competition.includes("Tournament") && "✓"}</td>
                </tr>
            ))}
        />
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
    return `${side} ${
        side === "Shadow" && victory === "Ring" ? "Corruption" : victory
    }`;
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
        .join(", ");
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
