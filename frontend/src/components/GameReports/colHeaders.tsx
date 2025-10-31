import React from "react";
import Box from "@mui/joy/Box";
import IconButton from "@mui/joy/IconButton";
import ViewIcon from "@mui/icons-material/Visibility";
import {
    ErrorMessage,
    GAME_LIMITS,
    leagues,
    sides,
    victoryTypes,
} from "../../constants";
import {
    GameReportFilters,
    MenuOption,
    Side,
    Victory,
    VictoryOption,
} from "../../types";
import { getLeagueLabel, isDefined } from "../../utils";
import { ColHeaderData, CornerHeaderData } from "../Table/types";
import { ALL_OPTION_ID, EMPTY_OPTION_ID, PLAYER_COL_WIDTH } from "./constants";
import { toVictoryKindLabel, toVictoryTypeLabel } from "./formatters";
import { isPairingValid } from "./validators";

type SwitchHeader = CornerHeaderData<MenuOption<number>> &
    ColHeaderData<MenuOption<number>>;

type StandardHeader =
    | ColHeaderData<MenuOption<number>>
    | ColHeaderData<MenuOption<string>>
    | ColHeaderData<MenuOption<VictoryOption>>;

interface Args {
    isAdmin: boolean;
    loadingPlayers: boolean;
    playerOptions: MenuOption<number>[];
    filters: GameReportFilters;
    setFilters: (filters: GameReportFilters) => void;
    setSettingsOpen: (isOpen: boolean) => void;
}

export default function colHeaders({
    isAdmin,
    loadingPlayers,
    playerOptions,
    filters,
    setFilters,
    setSettingsOpen,
}: Args): {
    cornerHeaders: CornerHeaderData[];
    switchHeaders: SwitchHeader[];
    standardHeaders: StandardHeader[];
} {
    const isPairingFilterValid = isPairingValid(filters.pairing);

    return {
        cornerHeaders: [
            { key: "expand", width: 64, content: <></> },
            isAdmin ? { key: "Edit", width: 58 } : null,
        ].filter(isDefined),

        switchHeaders: [
            { key: "No.", width: 50 },
            {
                key: "Pairing",
                width: 200,
                filter: {
                    filterType: "autocomplete",
                    placeholder: "Select pairing",
                    loading: loadingPlayers,
                    options: playerOptions,
                    current: filters.pairing,
                    appliedCount: isPairingFilterValid
                        ? filters.pairing.length
                        : 0,
                    onChange: (values: MenuOption<number>[]) =>
                        setFilters({ ...filters, pairing: values }),
                    errorMessage: isPairingFilterValid
                        ? undefined
                        : ErrorMessage.PairingFilterInvalid,
                },
            },
        ],

        standardHeaders: [
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
                    onChange: (value) =>
                        setFilters({ ...filters, turns: value }),
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
                    onChange: (values) =>
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
                    onChange: (values) =>
                        setFilters({ ...filters, losers: values }),
                },
            },
            { key: "Game Type" },
            {
                key: "Victory Type",
                width: 170,
                filter: {
                    filterType: "autocomplete",
                    placeholder: "Select type",
                    loading: false,
                    options: [
                        ...sides.map((s) => ({ id: s, label: s })),
                        ...victoryTypes.map((v) => ({
                            id: v,
                            label: toVictoryKindLabel(v),
                        })),
                        ...sides.flatMap((s) =>
                            victoryTypes.map(
                                (
                                    v
                                ): { id: [Side, Victory]; label: string } => ({
                                    id: [s, v],
                                    label: toVictoryTypeLabel(s, v),
                                })
                            )
                        ),
                    ],
                    current: filters.victory,
                    appliedCount: filters.victory.length,
                    listboxStyle: { fontSize: "12px" },
                    onChange: (values) =>
                        setFilters({ ...filters, victory: values }),
                },
            },
            { key: "Competition Type" },
            {
                key: "League",
                width: 140,
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
                    onChange: (values) => {
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
                    onChange: (value) =>
                        setFilters({ ...filters, tokens: value }),
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
                    min: GAME_LIMITS.corruption.min,
                    max: GAME_LIMITS.corruption.max,
                    current: filters.corruption,
                    appliedCount: isDefined(filters.corruption?.[1]) ? 1 : 0,
                    onChange: (value) =>
                        setFilters({ ...filters, corruption: value }),
                },
            },
            {
                key: "Mordor",
                width: 130,
                filter: {
                    filterType: "nullableInequality",
                    placeholder: "Step",
                    nullLabel: "Not reached",
                    min: 0,
                    max: 5,
                    current: filters.mordor,
                    appliedCount: isDefined(filters.mordor?.[1]) ? 1 : 0,
                    onChange: (value) =>
                        setFilters({ ...filters, mordor: value }),
                },
            },
            {
                key: "Aragorn",
                width: 140,
                filter: {
                    filterType: "nullableInequality",
                    placeholder: "Turn",
                    nullLabel: "Not played",
                    min: 1,
                    max: 999,
                    current: filters.aragorn,
                    appliedCount: isDefined(filters.aragorn?.[1]) ? 1 : 0,
                    onChange: (value) =>
                        setFilters({ ...filters, aragorn: value }),
                },
            },
            {
                key: "Treebeard",
                width: 150,
                filter: {
                    filterType: "boolean",
                    placeholder: "Treebeard",
                    current: filters.treebeard,
                    trueLabel: "Mustered",
                    falseLabel: "Not mustered",
                    appliedCount: isDefined(filters.treebeard) ? 1 : 0,
                    onChange: (treebeard) =>
                        setFilters({ ...filters, treebeard }),
                },
            },
            {
                key: "Initial Eyes",
                width: 150,
                filter: {
                    filterType: "inequality",
                    placeholder: "Eyes",
                    min: GAME_LIMITS.initialEyes.min,
                    max: GAME_LIMITS.initialEyes.max,
                    current: filters.initialEyes,
                    appliedCount: isDefined(filters.initialEyes?.[1]) ? 1 : 0,
                    onChange: (value) =>
                        setFilters({ ...filters, initialEyes: value }),
                },
            },
            {
                key: "SP-Captured Settlements",
                content: (
                    <Box
                        display="flex"
                        alignItems="center"
                        justifyContent="center"
                    >
                        <Box mr="5px">SP-Captured Settlements</Box>
                        <IconButton
                            size="sm"
                            color="primary"
                            onClick={() => setSettingsOpen(true)}
                            sx={{
                                height: "1em",
                                minHeight: "fit-content",
                                py: "2px",
                            }}
                        >
                            <ViewIcon />
                        </IconButton>
                    </Box>
                ),
            },
            { key: "SPVP" },
            { key: "FP-Captured Settlements" },
            { key: "FPVP" },
            {
                key: "Interest Rating",
                width: 170,
                filter: {
                    filterType: "inequality",
                    placeholder: "Rating",
                    min: GAME_LIMITS.interestRating.min,
                    max: GAME_LIMITS.interestRating.max,
                    current: filters.interestRating,
                    appliedCount: isDefined(filters.interestRating?.[1])
                        ? 1
                        : 0,
                    onChange: (value) =>
                        setFilters({ ...filters, interestRating: value }),
                },
            },
            { key: "Comments" },
            {
                key: "Game Log",
                width: 150,
                filter: {
                    filterType: "boolean",
                    placeholder: "Select",
                    current: filters.hasLog,
                    trueLabel: "Log",
                    falseLabel: "No log",
                    appliedCount: isDefined(filters.hasLog) ? 1 : 0,
                    onChange: (hasLog) => setFilters({ ...filters, hasLog }),
                },
            },
        ],
    };
}
