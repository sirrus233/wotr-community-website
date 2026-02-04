import React, { ReactNode } from "react";
import Box from "@mui/joy/Box";
import DeleteIcon from "@mui/icons-material/DeleteTwoTone";
import EditIcon from "@mui/icons-material/EditTwoTone";
import IconButton from "@mui/joy/IconButton";
import Tooltip from "@mui/joy/Tooltip";
import ViewIcon from "@mui/icons-material/Visibility";
import freeIconPath from "../../assets/ring-emoji.png";
import shadowIconPath from "../../assets/volcano-emoji.png";
import colors from "../../styles/colors";
import { GameReportParams, ProcessedGameReport, Side } from "../../types";
import {
    displayTime,
    getExpansionLabel,
    getLeagueLabel,
    isDefined,
} from "../../utils";
import ExternalLink from "../ExternalLink";
import { RowData } from "../Table/types";
import { PLAYER_COL_WIDTH } from "./constants";
import {
    countVictoryPoints,
    summarizeCompetitionType,
    summarizeGameType,
    summarizeVictoryType,
    formatExpansions,
} from "./formatters";
import FreeCaptures from "./FreeCaptures";
import { ReportEditParams } from "./types";
import { getReportsOffset } from "./serializers";
import { GameReportSettings } from "./Settings";
import ShadowCaptures from "./ShadowCaptures";

interface Args {
    isAdmin: boolean;
    loadingReports: boolean;
    params: GameReportParams;
    reports: ProcessedGameReport[];
    settings: GameReportSettings;
    totalReportCount: number;
    setReportEditParams: (params: ReportEditParams) => void;
}

export default function rows({
    isAdmin,
    loadingReports,
    params: { currentPage, limit },
    reports,
    settings,
    totalReportCount,
    setReportEditParams,
}: Args): RowData[] {
    return reports.map(
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
                        report.competition,
                    ),
                },
                {
                    key: "league",
                    content: report.league ? getLeagueLabel(report.league) : "",
                },
                {
                    key: "expansions",
                    content: formatExpansions(report.expansions),
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
                    content: (
                        <ShadowCaptures
                            report={report}
                            layout={settings.settlementLayout}
                            isAbbreviated={settings.areSettlementsAbbreviated}
                        />
                    ),
                },
                {
                    key: "spvp",
                    content: countVictoryPoints(
                        report.strongholds,
                        report.expansions,
                        "Free",
                    ),
                },
                {
                    key: "fp-settlements",
                    content: <FreeCaptures report={report} />,
                },
                {
                    key: "fpvp",
                    content: countVictoryPoints(
                        report.strongholds,
                        report.expansions,
                        "Shadow",
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
        }),
    );
}

interface FixedWidthTextProps {
    children: ReactNode;
    width: number;
}

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

interface CommentTextProps {
    children: ReactNode;
}

function CommentText({ children }: CommentTextProps) {
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

interface GameAccentProps {
    side: Side;
}

function GameAccent({ side }: GameAccentProps) {
    const [color, src, alt] =
        side === "Free"
            ? [colors.freeAccent, freeIconPath, "FP Icon"]
            : [colors.shadowPrimary, shadowIconPath, "SP Icon"];

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
