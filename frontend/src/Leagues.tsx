import React from "react";
import Box from "@mui/joy/Box";
import { expansionLeagues, leagueTiers, START_YEAR } from "./constants";
import {
    HEADER_HEIGHT_PX,
    HEADER_MARGIN_PX,
    TABLE_BTN_HEIGHT_PX,
    TABLE_ELEMENTS_GAP,
    BUTTON_SELECTOR_HEIGHT,
} from "./styles/sizes";
import { LeagueParams, LeagueStats, LeagueTier } from "./types";
import { getLeagueLabel, range, toPercent } from "./utils";
import ButtonSelector from "./ButtonSelector";
import Table, { ColHeaderData, RowData } from "./Table";
import TableLayout from "./TableLayout";

const TABLE_TOP_POSITION =
    HEADER_HEIGHT_PX +
    HEADER_MARGIN_PX +
    BUTTON_SELECTOR_HEIGHT * 2 +
    TABLE_BTN_HEIGHT_PX +
    TABLE_ELEMENTS_GAP * 4;

interface Props {
    params: LeagueParams;
    setParams: React.Dispatch<React.SetStateAction<LeagueParams>>;
    stats: LeagueStats;
    loading: boolean;
    error: string | null;
    refresh: () => void;
}

export default function Leagues({
    params,
    setParams,
    stats,
    loading,
    error,
    refresh,
}: Props) {
    const availableYears = range(START_YEAR, new Date().getFullYear() + 1);

    return (
        <Box>
            <ButtonSelector
                current={params.year}
                options={availableYears.reverse()}
                setCurrent={(year) =>
                    setParams((params) => ({ ...params, year }))
                }
                style={{ marginBottom: `${TABLE_ELEMENTS_GAP}px` }}
            />

            <Box
                display="flex"
                alignItems="center"
                justifyContent="center"
                overflow="auto"
            >
                <Box display="flex" alignItems="center" mr={1} fontSize="sm">
                    Base:
                    <ButtonSelector
                        current={params.tier}
                        options={leagueTiers.slice()}
                        setCurrent={(tier) =>
                            setParams((params) => ({
                                ...params,
                                league: "GeneralLeague",
                                tier,
                            }))
                        }
                        getLabel={getLeagueTierLabel}
                        variant="outlined"
                        style={{ marginLeft: "5px" }}
                    />
                </Box>

                <Box display="flex" alignItems="center" fontSize="sm">
                    Expansions:
                    <ButtonSelector
                        current={params.league}
                        options={expansionLeagues.slice()}
                        setCurrent={(league) =>
                            setParams((params) => ({
                                ...params,
                                league,
                                tier: null,
                            }))
                        }
                        getLabel={getLeagueLabel}
                        variant="outlined"
                        style={{ marginLeft: "5px" }}
                    />
                </Box>
            </Box>

            <TableLayout
                loading={loading}
                error={error}
                refresh={refresh}
                containerStyle={{
                    maxHeight: `calc(100vh - ${TABLE_TOP_POSITION}px - ${TABLE_ELEMENTS_GAP}px)`,
                }}
                table={<LeagueTable stats={stats} />}
            />
        </Box>
    );
}

interface LeagueTableProps {
    stats: LeagueStats;
}

function LeagueTable({ stats }: LeagueTableProps) {
    const entries = Object.entries(stats);

    const FIXED_HEADERS = ["Win Rate", "Games", "Wins", "Points"];

    return (
        <Table
            colHeaders={[
                ...FIXED_HEADERS.map(
                    (headerLabel): ColHeaderData => ({
                        key: headerLabel,
                        content: headerLabel,
                    })
                ),
                ...entries.map(
                    ([playerId, playerStats]): ColHeaderData => ({
                        key: playerId,
                        content: playerStats.name,
                    })
                ),
            ]}
            rows={entries.map(
                ([playerId, playerStats]): RowData => ({
                    key: playerId,
                    header: playerStats.name,
                    bodyCells: [
                        {
                            key: `${playerId}-winRate`,
                            content: toPercent(
                                playerStats.summary.wins /
                                    playerStats.summary.gameCount
                            ),
                        },
                        {
                            key: `${playerId}-gameCount`,
                            content: playerStats.summary.gameCount,
                        },
                        {
                            key: `${playerId}-wins`,
                            content: playerStats.summary.wins,
                        },
                        {
                            key: `${playerId}-points`,
                            content: playerStats.summary.points,
                        },
                        ...entries.map(([opponentId]) => {
                            const gameStatsForOpponent =
                                playerStats.gameStatsByOpponent[
                                    Number(opponentId)
                                ];
                            return {
                                key: `${playerId}-${opponentId}`,
                                content: (
                                    <Box
                                        sx={
                                            playerId === opponentId
                                                ? {
                                                      background: "#ccc",
                                                      color: "#ccc",
                                                      borderRadius: "5px",
                                                  }
                                                : {}
                                        }
                                    >
                                        {gameStatsForOpponent
                                            ? `${gameStatsForOpponent.wins}-${gameStatsForOpponent.losses}`
                                            : "-"}
                                    </Box>
                                ),
                            };
                        }),
                    ],
                })
            )}
        />
    );
}

function getLeagueTierLabel(tier: LeagueTier): string {
    switch (tier) {
        case "Tier1":
            return "Elven";
        case "Tier2":
            return "Dwarf";
        case "Tier3":
            return "Hobbit";
    }
}
