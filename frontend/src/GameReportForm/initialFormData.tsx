import { GameFormData } from "../types";
import { initializeToDefaults } from "../hooks/useFormData";

const initialFormData: GameFormData = {
    winner: initializeToDefaults(null),
    loser: initializeToDefaults(null),
    side: initializeToDefaults(null),
    victory: initializeToDefaults(null),
    match: initializeToDefaults(null),
    competition: initializeToDefaults([]),
    league: initializeToDefaults(null),
    usedExpansions: initializeToDefaults(null),
    expansions: initializeToDefaults([]),
    treebeard: initializeToDefaults(null),
    usedHandicap: initializeToDefaults(null),
    actionTokens: initializeToDefaults(0),
    dwarvenRings: initializeToDefaults(0),
    turns: initializeToDefaults(null),
    corruption: initializeToDefaults(null),
    didFellowshipReachMordor: initializeToDefaults(null),
    mordor: initializeToDefaults(null),
    initialEyes: initializeToDefaults(null),
    wasAragornCrowned: initializeToDefaults(null),
    aragornTurn: initializeToDefaults(null),
    strongholds: initializeToDefaults([]),
    interestRating: initializeToDefaults(null),
    comment: initializeToDefaults(null),
    logFile: initializeToDefaults(null),
};

export default initialFormData;
