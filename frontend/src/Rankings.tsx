import React, {
    CSSProperties,
    DetailedHTMLProps,
    ImgHTMLAttributes,
    ReactElement,
    ReactNode,
    useState,
} from "react";
import Badge from "@mui/joy/Badge";
import Box from "@mui/joy/Box";
import EditIcon from "@mui/icons-material/EditOutlined";
import IconButton from "@mui/joy/IconButton";
import MergeIcon from "@mui/icons-material/Merge";
import Modal from "@mui/joy/Modal";
import ModalClose from "@mui/joy/ModalClose";
import ModalDialog from "@mui/joy/ModalDialog";
import Tooltip from "@mui/joy/Tooltip";
import Typography from "@mui/joy/Typography";
import {
    Country,
    LeaderboardEntry,
    PlayerEditMode,
    PlayerState,
    Side,
} from "./types";
import { FREE_PRIMARY_COLOR, SHADOW_PRIMARY_COLOR } from "./styles/colors";
import {
    HEADER_HEIGHT_PX,
    HEADER_MARGIN_PX,
    TABLE_BTN_HEIGHT_PX,
    TABLE_ELEMENTS_GAP,
    BUTTON_SELECTOR_HEIGHT,
} from "./styles/sizes";
import ButtonSelector from "./ButtonSelector";
import Filters from "./Filters";
import PlayerEditForm from "./PlayerEditForm";
import PlayerRemapForm from "./PlayerRemapForm";
import TableLayout from "./TableLayout";
import { range, toPercent } from "./utils";
import {
    COUNTRIES_DATA,
    LEADERBOARD_START_YEAR,
    playerStates,
} from "./constants";

type PlayerEditParams = {
    pid: number;
    name: string;
    country: Country | null;
    mode: PlayerEditMode;
};

interface Props {
    leaderboard: LeaderboardEntry[];
    year: number;
    loading: boolean;
    error: string | null;
    isAdmin: boolean;
    getLeaderboard: () => void;
    setYear: (year: number) => void;
    setError: (error: string | null) => void;
}

const FLAG_WIDTH = 32;
const FLAG_HEIGHT = 24;
const HEADER_ROW_HEIGHT = 32;

const TABLE_TOP_POSITION =
    HEADER_HEIGHT_PX +
    HEADER_MARGIN_PX +
    BUTTON_SELECTOR_HEIGHT +
    TABLE_BTN_HEIGHT_PX * 2 +
    TABLE_ELEMENTS_GAP * 3;

function Rankings({
    leaderboard,
    year,
    loading,
    error,
    isAdmin,
    getLeaderboard,
    setYear,
    setError,
}: Props) {
    const [filters, setFilters] = useState<PlayerState[]>(["Active"]);

    const [playerEditParams, setPlayerEditParams] =
        useState<PlayerEditParams | null>(null);

    const refresh = () => {
        setError(null);
        getLeaderboard();
    };

    const availableYears = range(
        LEADERBOARD_START_YEAR,
        new Date().getFullYear() + 1
    );

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
                                playerOptions={leaderboard.map((entry) => ({
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

            <ButtonSelector
                current={year}
                options={availableYears.reverse()}
                setCurrent={setYear}
            />

            <TableLayout
                refresh={refresh}
                error={error}
                loading={loading}
                label="Rankings"
                filters={
                    <Filters
                        options={playerStates.slice()}
                        current={filters}
                        onChange={setFilters}
                    />
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
                                Yearly Stats {year}
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
                body={leaderboard
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
        const imgProps: DetailedHTMLProps<
            ImgHTMLAttributes<HTMLImageElement>,
            HTMLImageElement
        > = {
            width: FLAG_WIDTH,
            alt: country,
            style: { background: "#ddd", boxShadow: "inset 0 0 2px 2px white" },
            loading: "eager",
        };

        const { code, src } = countryData;

        if (src) {
            return (
                <FlagContainer country={country} code={code || country}>
                    <img src={src} {...imgProps} />
                </FlagContainer>
            );
        }

        if (code) {
            // https://flagpedia.net/download/api
            const lowerCode = code.toLowerCase();
            const dimensions = `${FLAG_WIDTH}x${FLAG_HEIGHT}`;
            const dimensions2x = `${FLAG_WIDTH * 2}x${FLAG_HEIGHT * 2}`;
            const dimensions3x = `${FLAG_WIDTH * 3}x${FLAG_HEIGHT * 3}`;

            return (
                <FlagContainer country={country} code={code}>
                    <img
                        src={`https://flagcdn.com/${dimensions}/${lowerCode}.png`}
                        srcSet={`https://flagcdn.com/${dimensions2x}/${lowerCode}.png 2x, https://flagcdn.com/${dimensions3x}/${lowerCode}.png 3x`}
                        height={FLAG_HEIGHT}
                        {...imgProps}
                    />
                </FlagContainer>
            );
        }
    }

    return null;
}
