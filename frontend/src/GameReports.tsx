import React, { CSSProperties, ReactNode, useState } from "react";
import Box from "@mui/joy/Box";
import CollapseIcon from "@mui/icons-material/KeyboardArrowDown";
import EditIcon from "@mui/icons-material/EditTwoTone";
import DeleteIcon from "@mui/icons-material/DeleteTwoTone";
import ExpandIcon from "@mui/icons-material/KeyboardArrowUp";
import FilterIcon from "@mui/icons-material/FilterList";
import IconButton from "@mui/joy/IconButton";
import Modal from "@mui/joy/Modal";
import ModalClose from "@mui/joy/ModalClose";
import ModalDialog from "@mui/joy/ModalDialog";
import Typography from "@mui/joy/Typography";
import { ErrorMessage, leagues } from "./constants";
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
    strongholdPoints,
    strongholdSide,
} from "./utils";
import TableFilter from "./TableFilter";
import TableLayout from "./TableLayout";
import ExternalLink from "./ExternalLink";
import GameReportForm from "./GameReportForm";
import ReportDeleteForm from "./ReportDeleteForm";
import Pagination from "./Pagination";

type ReportEditParams = ProcessedGameReport & { mode: ReportEditMode };

const TABLE_TOP_POSITION =
    HEADER_HEIGHT_PX +
    HEADER_MARGIN_PX +
    TABLE_BTN_HEIGHT_PX +
    TABLE_ELEMENTS_GAP * 2;

const PAGE_FOOTER_HEIGHT = 50;
const PAIRING_COL_WIDTH = 190;
const PLAYER_COL_WIDTH = 130;
const LEAGUE_COL_WIDTH = 130;

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
    const [areFiltersOpen, setAreFiltersOpen] = useState(false);

    const { currentPage, filters, limit } = params;

    const setCurrentPage = (currentPage: number) =>
        setParams((params) => ({ ...params, currentPage }));

    const setFilters = (filters: GameReportFilters) =>
        setParams((params) => ({ ...params, currentPage: 1, filters }));

    const isPairingFilterValid = isPairingValid(filters.pairing);

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
                tableStyle={{
                    "& thead > tr:first-child > *:first-child": {
                        pl: 2,
                    },
                    "& thead > tr:last-child > *:first-child": {
                        pl: 2,
                        borderBottomWidth: "2px",
                    },
                    "& thead > tr > th": {
                        verticalAlign: "middle",
                    },
                    "& thead > tr:first-child > th": {
                        borderBottom: areFiltersOpen ? "none" : undefined,
                        verticalAlign: areFiltersOpen ? "bottom" : undefined,
                    },
                    "& tbody > tr > *:first-child": { pl: 2 },
                    "& thead > tr:last-child > *:last-child": {
                        pr: 2,
                    },
                    "& tbody > tr > *:last-child": { pr: 2 },
                }}
                header={
                    <>
                        {areFiltersOpen && (
                            <FilterBar>
                                <th colSpan={2}>
                                    <ExpandButton
                                        expanded={areFiltersOpen}
                                        setExpanded={setAreFiltersOpen}
                                    />
                                </th>

                                {isAdmin && <th />}
                                <th />

                                <TableFilter
                                    placeholder={"Select pairing"}
                                    loading={loadingPlayers}
                                    options={playerOptions}
                                    width={PAIRING_COL_WIDTH}
                                    current={filters.pairing}
                                    onChange={(values) =>
                                        setFilters({
                                            ...filters,
                                            pairing: values,
                                        })
                                    }
                                    errorMessage={
                                        isPairingFilterValid
                                            ? undefined
                                            : ErrorMessage.PairingFilterInvalid
                                    }
                                />

                                <th />
                                <th />

                                <TableFilter
                                    placeholder="Select winner"
                                    loading={loadingPlayers}
                                    options={playerOptions}
                                    width={PLAYER_COL_WIDTH}
                                    current={filters.winners}
                                    onChange={(values) =>
                                        setFilters({
                                            ...filters,
                                            winners: values,
                                        })
                                    }
                                />

                                <TableFilter
                                    placeholder="Select loser"
                                    loading={loadingPlayers}
                                    options={playerOptions}
                                    width={PLAYER_COL_WIDTH}
                                    current={filters.losers}
                                    onChange={(values) =>
                                        setFilters({
                                            ...filters,
                                            losers: values,
                                        })
                                    }
                                />

                                <th />
                                <th />
                                <th />

                                <TableFilter
                                    placeholder="Select leagues"
                                    loading={false}
                                    allOption={{
                                        id: ALL_OPTION_ID,
                                        label: "Any league",
                                    }}
                                    emptyOption={{
                                        id: EMPTY_OPTION_ID,
                                        label: "No league",
                                    }}
                                    options={leagues.map((league) => ({
                                        id: league,
                                        label: getLeagueLabel(league),
                                    }))}
                                    width={LEAGUE_COL_WIDTH}
                                    current={filters.leagues}
                                    onChange={(values) => {
                                        setFilters({
                                            ...filters,
                                            leagues: values,
                                        });
                                    }}
                                />

                                <th />
                                <th />
                                <th />
                                <th />
                                <th />
                                <th />
                                <th />
                                <th />
                                <th />
                                <th />
                                <th />
                                <th />
                                <th />
                                <th />
                                <th />
                            </FilterBar>
                        )}
                        <tr>
                            <th colSpan={2}>
                                {!areFiltersOpen && (
                                    <ExpandButton
                                        expanded={areFiltersOpen}
                                        setExpanded={setAreFiltersOpen}
                                    />
                                )}
                            </th>

                            {isAdmin && <th>Edit</th>}
                            <th>No.</th>

                            <FilterHeader
                                areFiltersOpen={areFiltersOpen}
                                setAreFiltersOpen={setAreFiltersOpen}
                                appliedCount={
                                    isPairingFilterValid
                                        ? filters.pairing.length
                                        : 0
                                }
                                style={{ width: `${PAIRING_COL_WIDTH}px` }}
                            >
                                Pairing
                            </FilterHeader>

                            <th>Timestamp</th>
                            <th>Turn</th>

                            <FilterHeader
                                areFiltersOpen={areFiltersOpen}
                                setAreFiltersOpen={setAreFiltersOpen}
                                appliedCount={filters.winners.length}
                                style={{ width: `${PLAYER_COL_WIDTH}px` }}
                            >
                                Winner
                            </FilterHeader>

                            <FilterHeader
                                areFiltersOpen={areFiltersOpen}
                                setAreFiltersOpen={setAreFiltersOpen}
                                appliedCount={filters.losers.length}
                                style={{ width: `${PLAYER_COL_WIDTH}px` }}
                            >
                                Loser
                            </FilterHeader>

                            <th>Game Type</th>
                            <th>Victory Type</th>
                            <th>Competition Type</th>

                            <FilterHeader
                                areFiltersOpen={areFiltersOpen}
                                setAreFiltersOpen={setAreFiltersOpen}
                                appliedCount={filters.leagues.length}
                                style={{ width: `${LEAGUE_COL_WIDTH}px` }}
                            >
                                League
                            </FilterHeader>

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
                    </>
                }
                body={reports.map((report, i) => (
                    <tr key={report.rid}>
                        <RowAccent side={report.side} />
                        <td style={{ padding: "0 8px 0 0" }}>
                            {report.side === "Free" ? "💍" : "🌋"}
                        </td>

                        {isAdmin && (
                            <td>
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
                            </td>
                        )}

                        <td style={{ fontWeight: "bold" }}>
                            {totalReportCount -
                                i -
                                getReportsOffset(currentPage, limit)}
                        </td>

                        <FixedWidthCell width={PAIRING_COL_WIDTH}>
                            {[report.winner, report.loser]
                                .sort((a, b) => a.localeCompare(b))
                                .join("-")}
                        </FixedWidthCell>

                        <td>{displayTime(report.timestamp)}</td>
                        <td>{report.turns}</td>

                        <FixedWidthCell width={PLAYER_COL_WIDTH}>
                            {report.winner}
                        </FixedWidthCell>

                        <FixedWidthCell width={PLAYER_COL_WIDTH}>
                            {report.loser}
                        </FixedWidthCell>

                        <td>{summarizeGameType(report.expansions)}</td>
                        <td>
                            {summarizeVictoryType(report.side, report.victory)}
                        </td>
                        <td>
                            {summarizeCompetitionType(
                                report.match,
                                report.competition
                            )}
                        </td>

                        <FixedWidthCell width={LEAGUE_COL_WIDTH}>
                            {report.league ? getLeagueLabel(report.league) : ""}
                        </FixedWidthCell>

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

type FixedWidthCellProps = ContainerProps & {
    width: number;
};

function FixedWidthCell({ width, children }: FixedWidthCellProps) {
    return (
        <td>
            <Typography
                width={`${width}px`}
                overflow="hidden"
                whiteSpace="nowrap"
                textOverflow="ellipsis"
            >
                {children}
            </Typography>
        </td>
    );
}

function WrappedCell({ children }: ContainerProps) {
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

function FilterBar({ children }: ContainerProps) {
    return (
        <tr
            style={{
                position: "relative",
                zIndex: "calc(var(--joy-zIndex-table) + 1)",
            }}
        >
            {children}
        </tr>
    );
}

type FilterHeaderProps = ContainerProps & {
    areFiltersOpen: boolean;
    setAreFiltersOpen: (areOpen: boolean) => void;
    appliedCount: number;
    style?: CSSProperties;
};

function FilterHeader({
    areFiltersOpen,
    setAreFiltersOpen,
    appliedCount,
    children,
    style = {},
}: FilterHeaderProps) {
    return (
        <th>
            <Box
                sx={{
                    display: "flex",
                    alignItems: "center",
                    justifyContent: "center",
                    height: "100%",
                    ...style,
                }}
            >
                {children}
                <FilterButton
                    areFiltersOpen={areFiltersOpen}
                    setAreFiltersOpen={setAreFiltersOpen}
                    appliedCount={appliedCount}
                />
            </Box>
        </th>
    );
}

interface FilterButtonProps {
    areFiltersOpen: boolean;
    setAreFiltersOpen: (areOpen: boolean) => void;
    appliedCount: number;
}

function FilterButton({
    areFiltersOpen,
    setAreFiltersOpen,
    appliedCount,
}: FilterButtonProps) {
    return (
        <IconButton
            onClick={() => setAreFiltersOpen(!areFiltersOpen)}
            size="sm"
            color="primary"
            variant={appliedCount ? "solid" : undefined}
            sx={{
                minWidth: 0,
                minHeight: 0,
                px: "4px",
                height: "1.5em",
                position: "absolute",
                justifyContent: "end",
                right: 0,
                mr: 1,
            }}
        >
            <FilterIcon />
            <Box fontSize="12px">{appliedCount ? appliedCount : null}</Box>
        </IconButton>
    );
}

interface ExpandButtonProps {
    expanded: boolean;
    setExpanded: (expanded: boolean) => void;
}

function ExpandButton({ expanded, setExpanded }: ExpandButtonProps) {
    return (
        <IconButton
            onClick={() => setExpanded(!expanded)}
            sx={{
                display: "flex",
                minWidth: 0,
                minHeight: 0,
                height: "100%",
            }}
        >
            {expanded ? <CollapseIcon /> : <ExpandIcon />}
        </IconButton>
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
