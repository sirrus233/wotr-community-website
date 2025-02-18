import React, { useState } from "react";
import AddIcon from "@mui/icons-material/Add";
import Box from "@mui/joy/Box";
import IconButton from "@mui/joy/IconButton";
import Modal from "@mui/joy/Modal";
import ModalClose from "@mui/joy/ModalClose";
import ModalDialog from "@mui/joy/ModalDialog";
import { expansionLeagues, leagueTiers, LEAGUE_START_YEAR } from "./constants";
import {
    HEADER_HEIGHT_PX,
    HEADER_MARGIN_PX,
    TABLE_BTN_HEIGHT_PX,
    TABLE_ELEMENTS_GAP,
    BUTTON_SELECTOR_HEIGHT,
} from "./styles/sizes";
import { LeagueParams, LeagueStats } from "./types";
import {
    getLeagueLabel,
    getLeagueTierLabel,
    noNansense,
    range,
    toPercent,
} from "./utils";
import ButtonSelector from "./ButtonSelector";
import LeaguePlayerForm from "./LeaguePlayerForm";
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
    playerNames: string[];
    loading: boolean;
    loadingPlayers: boolean;
    isAdmin: boolean;
    error: string | null;
    refresh: () => void;
}

export default function Leagues({
    params,
    setParams,
    stats,
    playerNames,
    loading,
    loadingPlayers,
    isAdmin,
    error,
    refresh,
}: Props) {
    const [leaguePlayerFormOpen, setLeaguePlayerFormOpen] = useState(false);

    const availableYears = range(
        LEAGUE_START_YEAR,
        new Date().getFullYear() + 1
    );

    return (
        <Box>
            {leaguePlayerFormOpen && (
                <Modal open onClose={() => setLeaguePlayerFormOpen(false)}>
                    <ModalDialog maxWidth="200px">
                        <ModalClose />

                        <LeaguePlayerForm
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
                                tier: "Tier1",
                            }))
                        }
                        getLabel={getLeagueLabel}
                        variant="outlined"
                        selectedVariant="solid"
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
                table={
                    <LeagueTable
                        stats={stats}
                        loading={loading}
                        isAdmin={isAdmin}
                        openLeaguePlayerForm={() =>
                            setLeaguePlayerFormOpen(true)
                        }
                    />
                }
            />
        </Box>
    );
}

interface LeagueTableProps {
    stats: LeagueStats;
    loading: boolean;
    isAdmin: boolean;
    openLeaguePlayerForm: () => void;
}

function LeagueTable({
    stats,
    loading,
    isAdmin,
    openLeaguePlayerForm,
}: LeagueTableProps) {
    const entries = Object.entries(stats).sort(
        ([, playerStatsA], [, playerStatsB]) =>
            playerStatsB.summary.points - playerStatsA.summary.points
    );

    const FIXED_HEADERS = ["Win Rate", "Games", "Wins", "Points"];

    return (
        <Table
            cornerHeader={
                isAdmin
                    ? {
                          content: (
                              <IconButton
                                  size="sm"
                                  disabled={loading}
                                  onClick={openLeaguePlayerForm}
                                  variant="solid"
                                  color="primary"
                              >
                                  <AddIcon />
                              </IconButton>
                          ),
                      }
                    : undefined
            }
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
                        {
                            key: `${playerId}-points`,
                            content: playerStats.summary.points.toFixed(1),
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
