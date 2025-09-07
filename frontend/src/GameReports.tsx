import React, { ReactNode, useState } from "react";
import Box from "@mui/joy/Box";
import EditIcon from "@mui/icons-material/EditTwoTone";
import DeleteIcon from "@mui/icons-material/DeleteTwoTone";
import ViewIcon from "@mui/icons-material/Visibility";
import IconButton from "@mui/joy/IconButton";
import Modal from "@mui/joy/Modal";
import ModalClose from "@mui/joy/ModalClose";
import ModalDialog from "@mui/joy/ModalDialog";
import Tooltip from "@mui/joy/Tooltip";
import freeIconPath from "./assets/ring-emoji.png";
import shadowIconPath from "./assets/volcano-emoji.png";
import { ErrorMessage, leagues } from "./constants";
import useMediaQuery from "./hooks/useMediaQuery";
import { RefreshRequest } from "./hooks/useRequestState";
import {
    Competition,
    Expansion,
    GameReportFilters,
    GameReportParams,
    Match,
    MenuOption,
    ProcessedGameReport,
    ReportEditMode,
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
    displayTime,
    getExpansionLabel,
    getLeagueLabel,
    getStrongholdLabel,
    isDefined,
    strongholdPoints,
    strongholdSide,
} from "./utils";
import TableLayout from "./TableLayout";
import ExternalLink from "./ExternalLink";
import GameReportForm from "./GameReportForm";
import ReportDeleteForm from "./ReportDeleteForm";
import Pagination from "./Pagination";
import Table from "./Table";
import { ColHeaderData, CornerHeaderData, RowData } from "./Table/types";

type ReportEditParams = ProcessedGameReport & { mode: ReportEditMode };

const TABLE_TOP_POSITION =
    HEADER_HEIGHT_PX +
    HEADER_MARGIN_PX +
    TABLE_BTN_HEIGHT_PX +
    TABLE_ELEMENTS_GAP * 2;

const PAGE_FOOTER_HEIGHT = 50;
const PAIRING_COL_WIDTH = 200;
const PLAYER_COL_WIDTH = 140;
const LEAGUE_COL_WIDTH = 140;

const ALL_OPTION_ID = "ALL";
const EMPTY_OPTION_ID = "EMPTY";

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
    setParams,
    refresh,
    refreshLeaderboard,
}: Props) {
    const [reportEditParams, setReportEditParams] =
        useState<ReportEditParams | null>(null);

    const belowSmallBreakpoint = useMediaQuery("sm");

    const { currentPage, filters, limit } = params;

    const setCurrentPage = (currentPage: number) =>
        setParams((params) => ({ ...params, currentPage }));

    const setFilters = (filters: GameReportFilters) =>
        setParams((params) => ({ ...params, currentPage: 1, filters }));

    const isPairingFilterValid = isPairingValid(filters.pairing);

    const cornerHeaders: CornerHeaderData[] = [
        { key: "expand", width: 64, content: <></> },
        isAdmin ? { key: "Edit", width: 58 } : null,
    ].filter(isDefined);

    const switchHeaders: (CornerHeaderData & ColHeaderData)[] = [
        { key: "No.", width: 50 },
        {
            key: "Pairing",
            width: PAIRING_COL_WIDTH,
            filter: {
                filterType: "autocomplete",
                placeholder: "Select pairing",
                loading: loadingPlayers,
                options: playerOptions,
                current: filters.pairing,
                appliedCount: isPairingFilterValid ? filters.pairing.length : 0,
                onChange: (values: MenuOption<number>[]) =>
                    setFilters({ ...filters, pairing: values }),
                errorMessage: isPairingFilterValid
                    ? undefined
                    : ErrorMessage.PairingFilterInvalid,
            },
        },
    ];

    const colHeaders: ColHeaderData[] = [
        { key: "Timestamp" },
        {
            key: "Turn",
            width: 120,
            filter: {
                filterType: "inequality",
                placeholder: "Turn",
                min: 1,
                max: 999,
                current: filters.turns,
                appliedCount: isDefined(filters.turns?.[1]) ? 1 : 0,
                onChange: (value) => setFilters({ ...filters, turns: value }),
            },
        },
        {
            key: "Winner",
            width: PLAYER_COL_WIDTH,
            filter: {
                filterType: "autocomplete",
                placeholder: "Select winner",
                loading: loadingPlayers,
                options: playerOptions,
                current: filters.winners,
                appliedCount: filters.winners.length,
                onChange: (values: MenuOption<number>[]) =>
                    setFilters({ ...filters, winners: values }),
            },
        },
        {
            key: "Loser",
            width: PLAYER_COL_WIDTH,
            filter: {
                filterType: "autocomplete",
                placeholder: "Select loser",
                loading: loadingPlayers,
                options: playerOptions,
                current: filters.losers,
                appliedCount: filters.losers.length,
                onChange: (values: MenuOption<number>[]) =>
                    setFilters({ ...filters, losers: values }),
            },
        },
        { key: "Game Type" },
        { key: "Victory Type" },
        { key: "Competition Type" },
        {
            key: "League",
            width: LEAGUE_COL_WIDTH,
            filter: {
                filterType: "autocomplete",
                placeholder: "Select leagues",
                loading: false,
                allOption: { id: ALL_OPTION_ID, label: "Any league" },
                emptyOption: { id: EMPTY_OPTION_ID, label: "No league" },
                options: leagues.map((league) => ({
                    id: league,
                    label: getLeagueLabel(league),
                })),
                current: filters.leagues,
                appliedCount: filters.leagues.length,
                onChange: (values: MenuOption<string>[]) => {
                    setFilters({ ...filters, leagues: values });
                },
            },
        },
        { key: "Expansions" },
        {
            key: "Tokens",
            width: 130,
            filter: {
                filterType: "inequality",
                placeholder: "Tokens",
                min: 0,
                max: 999,
                current: filters.tokens,
                appliedCount: isDefined(filters.tokens?.[1]) ? 1 : 0,
                onChange: (value) => setFilters({ ...filters, tokens: value }),
            },
        },
        {
            key: "Dwarven Rings",
            width: 170,
            filter: {
                filterType: "inequality",
                placeholder: "Rings",
                min: 0,
                max: 999,
                current: filters.dwarvenRings,
                appliedCount: isDefined(filters.dwarvenRings?.[1]) ? 1 : 0,
                onChange: (value) =>
                    setFilters({ ...filters, dwarvenRings: value }),
            },
        },
        {
            key: "Corruption",
            width: 150,
            filter: {
                filterType: "inequality",
                placeholder: "Corruption",
                min: 0,
                max: 18,
                current: filters.corruption,
                appliedCount: isDefined(filters.corruption?.[1]) ? 1 : 0,
                onChange: (value) =>
                    setFilters({ ...filters, corruption: value }),
            },
        },
        { key: "Mordor" },
        { key: "Aragorn" },
        { key: "Treebeard" },
        {
            key: "Initial Eyes",
            width: 150,
            filter: {
                filterType: "inequality",
                placeholder: "Eyes",
                min: 0,
                max: 7,
                current: filters.initialEyes,
                appliedCount: isDefined(filters.initialEyes?.[1]) ? 1 : 0,
                onChange: (value) =>
                    setFilters({ ...filters, initialEyes: value }),
            },
        },
        { key: "SP-Captured Settlements" },
        { key: "SPVP" },
        { key: "FP-Captured Settlements" },
        { key: "FPVP" },
        {
            key: "Interest Rating",
            width: 170,
            filter: {
                filterType: "inequality",
                placeholder: "Rating",
                min: 1,
                max: 10,
                current: filters.interestRating,
                appliedCount: isDefined(filters.interestRating?.[1]) ? 1 : 0,
                onChange: (value) =>
                    setFilters({ ...filters, interestRating: value }),
            },
        },
        { key: "Comments" },
        { key: "Game Log" },
    ];

    const rows = reports.map(
        (report, i): RowData => ({
            key: report.rid,
            cells: [
                {
                    key: `side`,
                    style: { padding: "2px 0 2px 2px" },
                    content: <GameAccent side={report.side} />,
                },
                isAdmin
                    ? {
                          key: `edit`,
                          style: { padding: "1px" },
                          content: (
                              <>
                                  <IconButton
                                      size="sm"
                                      sx={{ minWidth: 0 }}
                                      disabled={loadingReports}
                                      onClick={() =>
                                          setReportEditParams({
                                              ...report,
                                              mode: "edit",
                                          })
                                      }
                                  >
                                      <EditIcon />
                                  </IconButton>

                                  <IconButton
                                      size="sm"
                                      sx={{ minWidth: 0 }}
                                      disabled={loadingReports}
                                      onClick={() =>
                                          setReportEditParams({
                                              ...report,
                                              mode: "delete",
                                          })
                                      }
                                  >
                                      <DeleteIcon />
                                  </IconButton>
                              </>
                          ),
                      }
                    : null,
                {
                    key: `number`,
                    content:
                        totalReportCount -
                        i -
                        getReportsOffset(currentPage, limit),
                },
                {
                    key: `pairing`,
                    content: [report.winner, report.loser]
                        .sort((a, b) => a.localeCompare(b))
                        .join("-"),
                },
                { key: `timestamp`, content: displayTime(report.timestamp) },
                { key: "turns", content: report.turns },
                {
                    key: "winner",
                    content: (
                        <FixedWidthText width={PLAYER_COL_WIDTH}>
                            {report.winner}
                        </FixedWidthText>
                    ),
                },
                {
                    key: "loser",
                    content: (
                        <FixedWidthText width={PLAYER_COL_WIDTH}>
                            {report.loser}
                        </FixedWidthText>
                    ),
                },
                {
                    key: "game-type",
                    content: summarizeGameType(report.expansions),
                },
                {
                    key: "victory-type",
                    content: summarizeVictoryType(report.side, report.victory),
                },
                {
                    key: "competition-type",
                    content: summarizeCompetitionType(
                        report.match,
                        report.competition
                    ),
                },
                {
                    key: "league",
                    content: report.league ? getLeagueLabel(report.league) : "",
                },
                {
                    key: "expansions",
                    content: report.expansions
                        .map(getExpansionLabel)
                        .join(", "),
                },
                { key: "tokens", content: report.actionTokens },
                { key: "dwarven-rings", content: report.dwarvenRings },
                { key: "corruption", content: report.corruption },
                { key: "mordor", content: report.mordor },
                { key: "aragorn", content: report.aragornTurn },
                { key: "treebeard", content: report.treebeard && "✓" },
                { key: "eyes", content: report.initialEyes },
                {
                    key: "sp-settlements",
                    content: summarizeCapturedSettlements(
                        report.strongholds,
                        report.expansions,
                        "Free"
                    ),
                },
                {
                    key: "spvp",
                    content: countVictoryPoints(
                        report.strongholds,
                        report.expansions,
                        "Free"
                    ),
                },
                {
                    key: "fp-settlements",
                    content: summarizeCapturedSettlements(
                        report.strongholds,
                        report.expansions,
                        "Shadow"
                    ),
                },
                {
                    key: "fpvp",
                    content: countVictoryPoints(
                        report.strongholds,
                        report.expansions,
                        "Shadow"
                    ),
                },
                { key: "interest", content: report.interestRating },
                {
                    key: "comments",
                    content: report.comment ? (
                        <CommentText>{report.comment}</CommentText>
                    ) : null,
                },
                {
                    key: "log",
                    content: report.logFile && (
                        <ExternalLink isDownload href={report.logFile}>
                            Download Report
                        </ExternalLink>
                    ),
                },
            ].filter(isDefined),
        })
    );

    return (
        <Box>
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
                containerStyle={{
                    maxHeight: `calc(100vh - ${TABLE_TOP_POSITION}px - ${TABLE_ELEMENTS_GAP}px - ${PAGE_FOOTER_HEIGHT}px)`,
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
                            ...colHeaders,
                        ]}
                        rows={rows}
                    />
                }
            />

            <Box height={`${PAGE_FOOTER_HEIGHT}px`}>
                <Pagination
                    currentPage={currentPage}
                    setCurrentPage={setCurrentPage}
                    totalCount={totalReportCount}
                    perPageCount={limit}
                />
            </Box>
        </Box>
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
        }),
    };
}

interface ContainerProps {
    children: ReactNode;
}

type FixedWidthTextProps = ContainerProps & {
    width: number;
};

function FixedWidthText({ width, children }: FixedWidthTextProps) {
    return (
        <Box
            width={`${width}px`}
            overflow="hidden"
            whiteSpace="nowrap"
            textOverflow="ellipsis"
        >
            {children}
        </Box>
    );
}

function CommentText({ children }: ContainerProps) {
    return (
        <Tooltip
            title={<Box maxWidth="300px">{children}</Box>}
            enterTouchDelay={0}
            size="sm"
        >
            <Box
                sx={{
                    display: "flex",
                    alignItems: "center",
                    justifyContent: "center",
                    cursor: "pointer",
                    color: "primary.500",
                    px: "5px",
                }}
            >
                <ViewIcon
                    color="primary"
                    sx={{ width: "1.5em", height: "1.5em", pr: "5px" }}
                />
                View comments
            </Box>
        </Tooltip>
    );
}

function GameAccent({ side }: { side: Side }) {
    const [color, src, alt] =
        side === "Free"
            ? [FREE_ACCENT_COLOR, freeIconPath, "FP Icon"]
            : [SHADOW_PRIMARY_COLOR, shadowIconPath, "SP Icon"];

    return (
        <Box
            display="flex"
            flexDirection="column"
            justifyContent="center"
            alignItems="center"
            p={0}
            height="100%"
            borderLeft={`5px solid ${color}`}
        >
            <img
                src={src}
                alt={alt}
                style={{ width: "14px", height: "14px" }}
            />
        </Box>
    );
}

function getReportsOffset(currentPage: number, perPageCount: number) {
    return (currentPage - 1) * perPageCount;
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

function summarizeCompetitionType(match: Match, competition: Competition[]) {
    return [match === "Rated" ? "Ladder" : "Friendly", ...competition]
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

function isPairingValid(pairing: unknown[]) {
    return pairing.length <= 2;
}

function toFilterParam(options: MenuOption<unknown>[]): unknown[] | null {
    return options.find(({ id }) => id === EMPTY_OPTION_ID)
        ? []
        : nullifyEmpty(options.map(({ id }) => id));
}

function nullifyEmpty<T>(arr: T[]) {
    return arr.length ? arr : null;
}
