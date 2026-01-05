import React, { useState } from "react";
import AddIcon from "@mui/icons-material/Add";
import Box from "@mui/joy/Box";
import IconButton from "@mui/joy/IconButton";
import Modal from "@mui/joy/Modal";
import ModalClose from "@mui/joy/ModalClose";
import ModalDialog from "@mui/joy/ModalDialog";
import {
    expansionLeagues,
    leagueTiers,
    LEAGUE_START_YEAR,
} from "../../constants";
import { RefreshRequest } from "../../hooks/useRequestState";
import sizes from "../../styles/sizes";
import { PageContainer } from "../../styles/styledComponents";
import { LeagueParams, LeagueStats } from "../../types";
import {
    getLeagueLabel,
    getLeagueTierLabel,
    isDefined,
    noNansense,
    range,
    toPercent,
} from "../../utils";
import ButtonSelector from "../ButtonSelector";
import Table from "../Table";
import { ColHeaderData, RowData } from "../Table/types";
import TableLayout from "../TableLayout";
import PlayerForm from "./PlayerForm";
import { LeagueSelector, SubLeagueSelector } from "./styledComponents";

interface Props {
    activeYear: number;
    stats: LeagueStats;
    playerNames: string[];
    loading: boolean;
    loadingPlayers: boolean;
    isAdmin: boolean;
    error: string | null;
    params: LeagueParams;
    setParams: React.Dispatch<React.SetStateAction<LeagueParams>>;
    refresh: RefreshRequest;
}

export default function Leagues({
    activeYear,
    stats,
    playerNames,
    loading,
    loadingPlayers,
    isAdmin,
    error,
    params,
    setParams,
    refresh,
}: Props) {
    const [leaguePlayerFormOpen, setLeaguePlayerFormOpen] = useState(false);

    const availableYears = range(LEAGUE_START_YEAR, activeYear + 1);

    return (
        <PageContainer>
            {leaguePlayerFormOpen && (
                <Modal open onClose={() => setLeaguePlayerFormOpen(false)}>
                    <ModalDialog maxWidth="200px">
                        <ModalClose />

                        <PlayerForm
                            league={params.league}
                            tier={params.tier}
                            year={params.year}
                            playerNames={playerNames}
                            loadingPlayers={loadingPlayers}
                            refresh={refresh}
                        />
                    </ModalDialog>
                </Modal>
            )}

            <ButtonSelector
                current={params.year}
                options={availableYears}
                setCurrent={(year) =>
                    setParams((params) => ({ ...params, year }))
                }
                style={{ marginBottom: `${sizes.tableElementsGap}px` }}
            />

            <LeagueSelector>
                <SubLeagueSelector>
                    Base:
                    <ButtonSelector
                        current={
                            params.league === "GeneralLeague"
                                ? params.tier
                                : null
                        }
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
                        selectedVariant="solid"
                    />
                </SubLeagueSelector>

                <SubLeagueSelector>
                    Expansions:
                    <ButtonSelector
                        current={params.league}
                        options={expansionLeagues.slice()}
                        setCurrent={(league) =>
                            setParams((params) => ({
                                ...params,
                                league,
                                tier: "Tier1",
                            }))
                        }
                        getLabel={getLeagueLabel}
                        variant="outlined"
                        selectedVariant="solid"
                    />
                </SubLeagueSelector>
            </LeagueSelector>

            <TableLayout
                loading={loading}
                error={error}
                refresh={refresh}
                table={
                    <LeagueTable
                        year={params.year}
                        stats={stats}
                        loading={loading}
                        isAdmin={isAdmin}
                        openLeaguePlayerForm={() =>
                            setLeaguePlayerFormOpen(true)
                        }
                    />
                }
            />
        </PageContainer>
    );
}

interface LeagueTableProps {
    year: number;
    stats: LeagueStats;
    loading: boolean;
    isAdmin: boolean;
    openLeaguePlayerForm: () => void;
}

function LeagueTable({
    year,
    stats,
    loading,
    isAdmin,
    openLeaguePlayerForm,
}: LeagueTableProps) {
    const entries = Object.entries(stats).sort(
        ([, playerStatsA], [, playerStatsB]) =>
            playerStatsB.summary.points - playerStatsA.summary.points
    );

    const FIXED_HEADERS = ["Points", "Win Rate", "Games", "Wins"];

    return (
        <Table
            cornerHeaders={[
                {
                    key: "player",
                    width: 145,
                    content: isAdmin ? (
                        <IconButton
                            size="sm"
                            disabled={
                                loading || year < new Date().getFullYear()
                            }
                            onClick={openLeaguePlayerForm}
                            variant="solid"
                            color="primary"
                        >
                            <AddIcon />
                        </IconButton>
                    ) : (
                        ""
                    ),
                },
            ].filter(isDefined)}
            colHeaders={[
                ...FIXED_HEADERS.map(
                    (headerLabel): ColHeaderData => ({
                        key: headerLabel,
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
                    cells: [
                        { key: playerStats.name },
                        {
                            key: `${playerId}-points`,
                            content: playerStats.summary.points.toFixed(1),
                        },
                        {
                            key: `${playerId}-winRate`,
                            content: toPercent(
                                noNansense(
                                    playerStats.summary.totalWins /
                                        playerStats.summary.totalGames
                                )
                            ),
                        },
                        {
                            key: `${playerId}-totalGames`,
                            content: playerStats.summary.totalGames,
                        },
                        {
                            key: `${playerId}-wins`,
                            content: playerStats.summary.totalWins,
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
