import { GameFormData, ProcessedGameReport } from "../../types";
import { initializeToDefaults } from "../../hooks/useFormData";
import { ErrorMessage } from "../../constants";

export default function getInitialFormData(
    report?: ProcessedGameReport,
    allowedPlayerNames?: string[]
): GameFormData {
    return {
        rid: initializeToDefaults(fromMaybeReport(null, report, "rid")),
        timestamp: initializeToDefaults(
            fromMaybeReport(null, report, "timestamp")
        ),
        winner: {
            value: fromMaybeReport(null, report, "winner"),
            error: null,
            validate: function _() {
                return allowedPlayerNames &&
                    this.value &&
                    !allowedPlayerNames.includes(this.value)
                    ? ErrorMessage.ExistingPlayerRequired
                    : null;
            },
        },
        loser: {
            value: fromMaybeReport(null, report, "loser"),
            error: null,
            validate: function _() {
                return allowedPlayerNames &&
                    this.value &&
                    !allowedPlayerNames.includes(this.value)
                    ? ErrorMessage.ExistingPlayerRequired
                    : null;
            },
        },
        side: initializeToDefaults(fromMaybeReport(null, report, "side")),
        victory: initializeToDefaults(fromMaybeReport(null, report, "victory")),
        match: initializeToDefaults(fromMaybeReport(null, report, "match")),
        competition: initializeToDefaults(
            fromMaybeReport([], report, "competition")
        ),
        league: initializeToDefaults(fromMaybeReport(null, report, "league")),
        usedExpansions: initializeToDefaults(
            report ? !!report.expansions.length : null
        ),
        expansions: initializeToDefaults(
            fromMaybeReport([], report, "expansions")
        ),
        treebeard: initializeToDefaults(
            fromMaybeReport(null, report, "treebeard")
        ),
        usedHandicap: initializeToDefaults(
            report
                ? (typeof report.actionTokens === "number" &&
                      report.actionTokens > 0) ||
                      (typeof report.dwarvenRings === "number" &&
                          report.dwarvenRings > 0)
                : null
        ),
        actionTokens: initializeToDefaults(
            fromMaybeReport(0, report, "actionTokens")
        ),
        dwarvenRings: initializeToDefaults(
            fromMaybeReport(0, report, "dwarvenRings")
        ),
        turns: initializeToDefaults(fromMaybeReport(null, report, "turns")),
        corruption: initializeToDefaults(
            fromMaybeReport(null, report, "corruption")
        ),
        didFellowshipReachMordor: initializeToDefaults(
            report ? typeof report.mordor === "number" : null
        ),
        mordor: initializeToDefaults(fromMaybeReport(null, report, "mordor")),
        initialEyes: initializeToDefaults(
            fromMaybeReport(null, report, "initialEyes")
        ),
        wasAragornCrowned: initializeToDefaults(
            report ? report && !!report.aragornTurn : null
        ),
        aragornTurn: initializeToDefaults(
            fromMaybeReport(null, report, "aragornTurn")
        ),
        strongholds: initializeToDefaults(
            fromMaybeReport([], report, "strongholds")
        ),
        interestRating: initializeToDefaults(
            fromMaybeReport(null, report, "interestRating")
        ),
        comment: initializeToDefaults(fromMaybeReport(null, report, "comment")),

        /**
         * Warning: Modifying an existing report's log file is not implemented.
         * Will always initialize to null.
         */
        logFile: initializeToDefaults(null),
    };
}

function fromMaybeReport<T, K extends keyof ProcessedGameReport>(
    defaultVal: Exclude<T, undefined>,
    report: ProcessedGameReport | undefined,
    key: K
): T | ProcessedGameReport[K] {
    return report ? report[key] : defaultVal;
}
