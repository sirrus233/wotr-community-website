import { GameFormData, ProcessedGameReport } from "../types";
import { initializeToDefaults } from "../hooks/useFormData";
import { ErrorMessage } from "../constants";

export default function getInitialFormData(
    report?: ProcessedGameReport,
    allowedPlayerNames?: string[]
): GameFormData {
    return {
        rid: initializeToDefaults(fromMaybe(null, report?.rid)),
        timestamp: initializeToDefaults(fromMaybe(null, report?.timestamp)),
        winner: {
            value: fromMaybe(null, report?.winner),
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
            value: fromMaybe(null, report?.loser),
            error: null,
            validate: function _() {
                return allowedPlayerNames &&
                    this.value &&
                    !allowedPlayerNames.includes(this.value)
                    ? ErrorMessage.ExistingPlayerRequired
                    : null;
            },
        },
        side: initializeToDefaults(fromMaybe(null, report?.side)),
        victory: initializeToDefaults(fromMaybe(null, report?.victory)),
        match: initializeToDefaults(fromMaybe(null, report?.match)),
        competition: initializeToDefaults(fromMaybe([], report?.competition)),
        league: initializeToDefaults(fromMaybe(null, report?.league)),
        usedExpansions: initializeToDefaults(
            fromMaybe(null, report && !!report.expansions.length)
        ),
        expansions: initializeToDefaults(fromMaybe([], report?.expansions)),
        treebeard: initializeToDefaults(fromMaybe(null, report?.treebeard)),
        usedHandicap: initializeToDefaults(
            fromMaybe(
                null,
                report &&
                    ((typeof report.actionTokens === "number" &&
                        report.actionTokens > 0) ||
                        (typeof report.dwarvenRings === "number" &&
                            report.dwarvenRings > 0))
            )
        ),
        actionTokens: initializeToDefaults(fromMaybe(0, report?.actionTokens)),
        dwarvenRings: initializeToDefaults(fromMaybe(0, report?.dwarvenRings)),
        turns: initializeToDefaults(fromMaybe(null, report?.turns)),
        corruption: initializeToDefaults(fromMaybe(null, report?.corruption)),
        didFellowshipReachMordor: initializeToDefaults(
            fromMaybe(null, report && typeof report.mordor === "number")
        ),
        mordor: initializeToDefaults(fromMaybe(null, report?.mordor)),
        initialEyes: initializeToDefaults(fromMaybe(null, report?.initialEyes)),
        wasAragornCrowned: initializeToDefaults(
            fromMaybe(null, report && !!report.aragornTurn)
        ),
        aragornTurn: initializeToDefaults(fromMaybe(null, report?.aragornTurn)),
        strongholds: initializeToDefaults(fromMaybe([], report?.strongholds)),
        interestRating: initializeToDefaults(
            fromMaybe(null, report?.interestRating)
        ),
        comment: initializeToDefaults(fromMaybe(null, report?.comment)),

        /**
         * Warning: Modifying an existing report's log file is not implemented.
         * Will always initialize to null.
         */
        logFile: initializeToDefaults(null),
    };
}

function fromMaybe<D, V>(defaultVal: Exclude<D, undefined>, val: V) {
    return val === undefined ? defaultVal : val;
}
