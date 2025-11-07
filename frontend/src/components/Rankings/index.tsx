import React, { CSSProperties, ReactElement, ReactNode, useState } from "react";
import Badge from "@mui/joy/Badge";
import Box from "@mui/joy/Box";
import Dropdown from "@mui/joy/Dropdown";
import EditIcon from "@mui/icons-material/EditOutlined";
import ExpandIcon from "@mui/icons-material/ExpandCircleDown";
import IconButton from "@mui/joy/IconButton";
import InfoIcon from "@mui/icons-material/InfoOutlined";
import Menu from "@mui/joy/Menu";
import MenuButton from "@mui/joy/MenuButton";
import MenuItem from "@mui/joy/MenuItem";
import MergeIcon from "@mui/icons-material/Merge";
import Modal from "@mui/joy/Modal";
import ModalClose from "@mui/joy/ModalClose";
import ModalDialog from "@mui/joy/ModalDialog";
import Tooltip from "@mui/joy/Tooltip";
import Typography from "@mui/joy/Typography";
import {
    COUNTRIES_DATA,
    LEADERBOARD_START_YEAR,
    playerStates,
} from "../../constants";
import { RefreshRequest } from "../../hooks/useRequestState";
import colors from "../../styles/colors";
import {
    HEADER_HEIGHT_PX,
    HEADER_MARGIN_PX,
    TABLE_BTN_HEIGHT_PX,
    TABLE_ELEMENTS_GAP,
} from "../../styles/sizes";
import {
    Country,
    LeaderboardEntry,
    LeaderboardParams,
    PlayerEditMode,
    PlayerState,
    Side,
} from "../../types";
import { range, toPercent } from "../../utils";
import Filters from "../Filters";
import TableLayout from "../TableLayout";
import PlayerEditForm from "./PlayerEditForm";
import PlayerRemapForm from "./PlayerRemapForm";

type PlayerEditParams = {
    pid: number;
    name: string;
    country: Country | null;
    mode: PlayerEditMode;
};

interface Props {
    entries: LeaderboardEntry[];
    loading: boolean;
    error: string | null;
    isAdmin: boolean;
    params: LeaderboardParams;
    setParams: React.Dispatch<React.SetStateAction<LeaderboardParams>>;
    refresh: RefreshRequest;
}

const HEADER_ROW_HEIGHT = 32;

const TABLE_TOP_POSITION =
    HEADER_HEIGHT_PX +
    HEADER_MARGIN_PX +
    TABLE_BTN_HEIGHT_PX * 2 +
    TABLE_ELEMENTS_GAP * 3;

function Rankings({
    entries,
    loading,
    error,
    isAdmin,
    params,
    setParams,
    refresh,
}: Props) {
    const [filters, setFilters] = useState<PlayerState[]>(["Active"]);
    const [playerEditParams, setPlayerEditParams] =
        useState<PlayerEditParams | null>(null);

    const { year } = params;

    const availableYears = range(
        LEADERBOARD_START_YEAR,
        new Date().getFullYear() + 1
    ).reverse();

    return (
        <Box>
            {playerEditParams && (
                <Modal open onClose={() => setPlayerEditParams(null)}>
                    <ModalDialog>
                        <ModalClose />
                        {playerEditParams.mode === "remap" ? (
                            <PlayerRemapForm
                                {...playerEditParams}
                                refresh={refresh}
                                playerOptions={entries.map((entry) => ({
                                    label: entry.name,
                                    id: entry.pid,
                                }))}
                            />
                        ) : (
                            <PlayerEditForm
                                {...playerEditParams}
                                refresh={refresh}
                            />
                        )}
                    </ModalDialog>
                </Modal>
            )}

            <TableLayout
                refresh={refresh}
                error={error}
                loading={loading}
                label="Rankings"
                filters={
                    <Box display="flex" position="relative">
                        <Filters
                            options={playerStates.slice()}
                            current={filters}
                            onChange={setFilters}
                        />
                        <Tooltip
                            enterTouchDelay={0}
                            size="sm"
                            title={
                                <Box
                                    display="flex"
                                    flexDirection="column"
                                    gap="5px"
                                    maxWidth="450px"
                                >
                                    <Box>
                                        A player rated {"<"} 700 is active if
                                        they've played <strong>4+</strong> games
                                        in the last 12 months.
                                    </Box>
                                    <Box>
                                        A player rated ≥ 700 is active if
                                        they've played <strong>12+</strong>{" "}
                                        games in the last 12 months.
                                    </Box>
                                    <Box>
                                        Active statuses are updated every{" "}
                                        <strong>24 hours</strong>.
                                    </Box>
                                </Box>
                            }
                        >
                            <Box
                                sx={{
                                    position: "absolute",
                                    left: "calc(100% + 10px)",
                                    height: "100%",
                                    display: "flex",
                                    alignItems: "center",
                                    justifyContent: "center",
                                    cursor: "pointer",
                                }}
                            >
                                <InfoIcon />
                            </Box>
                        </Tooltip>
                    </Box>
                }
                containerStyle={{
                    maxHeight: `calc(100vh - ${TABLE_TOP_POSITION}px - ${TABLE_ELEMENTS_GAP}px)`,
                }}
                header={
                    <>
                        <TableHeaderRow>
                            {isAdmin && (
                                <TableHeaderCell level={0} rowSpan={3}>
                                    Edit Player
                                </TableHeaderCell>
                            )}
                            <TableHeaderCell level={0} rowSpan={3}>
                                Rank
                            </TableHeaderCell>
                            <TableHeaderCell level={0} rowSpan={2} colSpan={2}>
                                Player
                            </TableHeaderCell>
                            <TableHeaderCell level={0} rowSpan={3}>
                                Avg. Rating
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
                            <TableHeaderCell level={0} colSpan={7}>
                                <Box
                                    sx={{
                                        display: "flex",
                                        alignItems: "center",
                                        justifyContent: "center",
                                    }}
                                >
                                    Yearly Stats {year}
                                    <YearSelector
                                        years={availableYears}
                                        setYear={(year) => setParams({ year })}
                                    />
                                </Box>
                            </TableHeaderCell>
                        </TableHeaderRow>

                        <TableHeaderRow>
                            {/* Base */}
                            <TableHeaderCell level={1} rowSpan={2}>
                                Games {year}
                            </TableHeaderCell>
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
                body={entries
                    .filter(
                        (entry) =>
                            (entry.isActive && filters.includes("Active")) ||
                            (!entry.isActive && filters.includes("Inactive"))
                    )
                    .map((entry, i) => (
                        <tr key={entry.pid}>
                            {isAdmin && (
                                <TableCell>
                                    <IconButton
                                        size="sm"
                                        disabled={loading}
                                        onClick={() =>
                                            setPlayerEditParams({
                                                pid: entry.pid,
                                                name: entry.name,
                                                country: entry.country,
                                                mode: "edit",
                                            })
                                        }
                                    >
                                        <EditIcon />
                                    </IconButton>

                                    <IconButton
                                        size="sm"
                                        disabled={loading}
                                        onClick={() =>
                                            setPlayerEditParams({
                                                pid: entry.pid,
                                                name: entry.name,
                                                country: entry.country,
                                                mode: "remap",
                                            })
                                        }
                                    >
                                        <MergeIcon />
                                    </IconButton>
                                </TableCell>
                            )}

                            <TableCell>{i + 1}</TableCell>
                            <TableCell
                                style={{
                                    lineHeight: 0,
                                    overflow: "visible",
                                }}
                            >
                                {toFlagImage(entry.country)}
                            </TableCell>
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
                                  ? colors.shadowPrimary
                                  : colors.freePrimary,
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
                                  ? colors.shadowPrimary
                                  : colors.freePrimary,
                      }
                    : style
            }
        >
            {children}
        </th>
    );
}

interface YearSelectorProps {
    years: number[];
    setYear: (year: number) => void;
}

function YearSelector({ years, setYear }: YearSelectorProps) {
    return (
        <Dropdown>
            <MenuButton
                variant="plain"
                color="primary"
                sx={{
                    py: "1px",
                    px: "3px",
                    mx: "3px",
                    my: 0,
                    minHeight: 0,
                }}
            >
                <ExpandIcon />
            </MenuButton>
            <Menu>
                {years.map((year) => (
                    <MenuItem key={year} onClick={() => setYear(year)}>
                        {year}
                    </MenuItem>
                ))}
            </Menu>
        </Dropdown>
    );
}

interface FlagContainerProps {
    children: ReactElement;
    country: string;
    code: string | null;
}

function FlagContainer({ children, country, code }: FlagContainerProps) {
    return (
        <Tooltip title={country} enterTouchDelay={0}>
            <Badge
                badgeContent={
                    <Typography sx={{ fontSize: "10px" }}>{code}</Typography>
                }
                variant="soft"
                size="sm"
                badgeInset="12px -11px"
            >
                {children}
            </Badge>
        </Tooltip>
    );
}

function toFlagImage(country: Country | null): ReactNode {
    const countryData = country ? COUNTRIES_DATA[country] : null;

    if (country && countryData) {
        // https://flagpedia.net/download/api
        const lowerCode = countryData.code.toLowerCase();
        const WIDTH = 32;
        const HEIGHT = 24;
        const dimensions = `${WIDTH}x${HEIGHT}`;
        const dimensions2x = `${WIDTH * 2}x${HEIGHT * 2}`;
        const dimensions3x = `${WIDTH * 3}x${HEIGHT * 3}`;

        return (
            <FlagContainer
                country={country}
                code={countryData.code.split("-").at(-1) || ""}
            >
                <img
                    src={`https://flagcdn.com/${dimensions}/${lowerCode}.png`}
                    srcSet={`https://flagcdn.com/${dimensions2x}/${lowerCode}.png 2x, https://flagcdn.com/${dimensions3x}/${lowerCode}.png 3x`}
                    width={WIDTH}
                    height={HEIGHT}
                    alt={country}
                    style={{
                        background: "#ddd",
                        boxShadow: "inset 0 0 2px 2px white",
                    }}
                    loading="eager"
                />
            </FlagContainer>
        );
    }

    return null;
}
