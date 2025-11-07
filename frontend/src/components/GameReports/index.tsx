import React, { useState } from "react";
import Box from "@mui/joy/Box";
import Modal from "@mui/joy/Modal";
import ModalClose from "@mui/joy/ModalClose";
import ModalDialog from "@mui/joy/ModalDialog";
import { ErrorMessage } from "../../constants";
import useMediaQuery from "../../hooks/useMediaQuery";
import { RefreshRequest } from "../../hooks/useRequestState";
import { PageContainer } from "../../styles/styledComponents";
import {
    GameReportFilters,
    GameReportParams,
    MenuOption,
    ProcessedGameReport,
} from "../../types";
import GameReportForm from "../GameReportForm";
import Pagination from "../Pagination";
import Table from "../Table";
import TableLayout from "../TableLayout";
import colHeaders from "./colHeaders";
import ReportDeleteForm from "./ReportDeleteForm";
import rows from "./rows";
import Settings, { defaultSettings, GameReportSettings } from "./Settings";
import {
    getReportsOffset,
    serializeNullableInequalityFilter,
    serializeVictoryFilter,
    toFilterParam,
} from "./serializers";
import { ReportEditParams } from "./types";
import { isPairingValid } from "./validators";

interface Props {
    reports: ProcessedGameReport[];
    totalReportCount: number;
    playerOptions: MenuOption<number>[];
    loadingReports: boolean;
    loadingPlayers: boolean;
    isAdmin: boolean;
    error: ErrorMessage | null;
    params: GameReportParams;
    setParams: React.Dispatch<React.SetStateAction<GameReportParams>>;
    refresh: RefreshRequest;
    refreshLeaderboard: RefreshRequest;
}

export default function GameReports({
    reports,
    totalReportCount,
    playerOptions,
    loadingReports,
    loadingPlayers,
    isAdmin,
    error,
    params,
    params: { currentPage, filters, limit },
    setParams,
    refresh,
    refreshLeaderboard,
}: Props) {
    const [reportEditParams, setReportEditParams] =
        useState<ReportEditParams | null>(null);
    const [settingsOpen, setSettingsOpen] = useState(false);
    const [settings, setSettings] =
        useState<GameReportSettings>(defaultSettings);

    const belowSmallBreakpoint = useMediaQuery("sm");

    const setCurrentPage = (currentPage: number) =>
        setParams((params) => ({ ...params, currentPage }));

    const setFilters = (filters: GameReportFilters) =>
        setParams((params) => ({ ...params, currentPage: 1, filters }));

    const { cornerHeaders, switchHeaders, standardHeaders } = colHeaders({
        isAdmin,
        loadingPlayers,
        playerOptions,
        filters,
        setFilters,
        setSettingsOpen,
    });

    return (
        <PageContainer>
            {reportEditParams && (
                <Modal open onClose={() => setReportEditParams(null)}>
                    <ModalDialog>
                        <ModalClose />
                        {reportEditParams.mode === "delete" ? (
                            <ReportDeleteForm
                                report={reportEditParams}
                                refresh={refresh}
                            />
                        ) : (
                            <Box overflow="auto" mt={3}>
                                <GameReportForm
                                    report={reportEditParams}
                                    playerNames={playerOptions.map(
                                        ({ label }) => label
                                    )}
                                    loadingPlayers={loadingPlayers}
                                    refreshGameReports={refresh}
                                    refreshLeaderboard={refreshLeaderboard}
                                    exit={() => setReportEditParams(null)}
                                />
                            </Box>
                        )}
                    </ModalDialog>
                </Modal>
            )}

            <TableLayout
                refresh={refresh}
                error={error}
                loading={loadingReports}
                label="Game Reports"
                settingsProps={{
                    togglePanel: () => setSettingsOpen((prev) => !prev),
                    isOpen: settingsOpen,
                    panel: (
                        <Settings
                            settings={settings}
                            setSettings={setSettings}
                        />
                    ),
                }}
                table={
                    <Table
                        hasFilters
                        cornerHeaders={[
                            ...cornerHeaders,
                            ...(belowSmallBreakpoint ? [] : switchHeaders),
                        ]}
                        colHeaders={[
                            ...(belowSmallBreakpoint ? switchHeaders : []),
                            ...standardHeaders,
                        ]}
                        rows={rows({
                            isAdmin,
                            loadingReports,
                            params,
                            reports,
                            settings,
                            totalReportCount,
                            setReportEditParams,
                        })}
                    />
                }
            />

            <Box height="50px">
                <Pagination
                    currentPage={currentPage}
                    setCurrentPage={setCurrentPage}
                    totalCount={totalReportCount}
                    perPageCount={limit}
                />
            </Box>
        </PageContainer>
    );
}

export function serializeReportsParams(params: GameReportParams) {
    const pairing = isPairingValid(params.filters.pairing)
        ? toFilterParam(params.filters.pairing)
        : null;

    return {
        limit: params.limit,
        offset: getReportsOffset(params.currentPage, params.limit),
        filter: JSON.stringify({
            ...params.filters,
            pairing: pairing?.length === 1 ? [...pairing, null] : pairing,
            winners: toFilterParam(params.filters.winners),
            losers: toFilterParam(params.filters.losers),
            players: toFilterParam(params.filters.players),
            leagues: toFilterParam(params.filters.leagues),
            mordor: serializeNullableInequalityFilter(params.filters.mordor),
            aragorn: serializeNullableInequalityFilter(params.filters.aragorn),
            victory: serializeVictoryFilter(params.filters.victory),
        }),
    };
}
