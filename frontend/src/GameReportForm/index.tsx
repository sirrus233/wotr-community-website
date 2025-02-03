import "@fontsource/inter";
import axios from "axios";
import React from "react";
import CircularProgress from "@mui/joy/CircularProgress";
import Modal from "@mui/joy/Modal";
import ModalClose from "@mui/joy/ModalClose";
import ModalDialog from "@mui/joy/ModalDialog";
import Typography from "@mui/joy/Typography";
import Sheet from "@mui/joy/Sheet";
import Box from "@mui/joy/Box";
import Button from "@mui/joy/Button";
import mordorStepsPath from "../assets/mordor-steps.png";
import {
    competitionTypes,
    ErrorMessage,
    expansions,
    leagues,
    matchTypes,
    MAX_GAME_LOG_SIZE_BYTES,
    MAX_GAME_LOG_SIZE_MB,
    optionalFields,
    serverValidationErrors,
    sides,
    strongholds,
    victoryTypes,
} from "../constants";
import { GAME_FORM_BOLD } from "../styles/colors";
import { toErrorMessage } from "../networkErrorHandlers";
import {
    GameFormData,
    GameReportPayload,
    LeaderboardEntry,
    ProcessedGameReport,
    ServerErrorBody,
    ServerValidationError,
    Stronghold,
    ValidGameFormData,
} from "../types";
import {
    getExpansionLabel,
    getLeagueLabel,
    getStrongholdLabel,
    isStrongholdInPlay,
    strongholdSide,
} from "../utils";
import useFormData from "../hooks/useFormData";
import Autocomplete from "../Autocomplete";
import ErrorDisplay from "../ErrorDisplay";
import FileUpload from "../FileUpload";
import FormElement from "../FormElement";
import MultiOptionInput from "../MultiOptionInput";
import SelectNumericOptionInput from "../SelectNumericOptionInput";
import SingleOptionInput from "../SingleOptionInput";
import TextInput from "../TextInput";
import VictoryPoints from "../VictoryPoints";
import getInitialFormData from "./getInitialFormData";
import useConditionalActionEffect from "../hooks/useConditionalActionEffect";
import useGameReportClearEffects from "../hooks/useGameReportFormEffects";

interface Props {
    report?: ProcessedGameReport;
    leaderboard: LeaderboardEntry[];
    loadingLeaderboard: boolean;
    refreshGameReports?: () => void;
    exit?: () => void;
}

function GameReportForm({
    report,
    leaderboard,
    loadingLeaderboard,
    refreshGameReports = () => {},
    exit,
}: Props) {
    const playerNames = leaderboard.map((entry) => entry.name);
    const initialFormData = getInitialFormData(report, report && playerNames);
    const emptyFormData = getInitialFormData();

    const [
        formData,
        { errorOnSubmit, successMessage, submitting },
        {
            setFormData,
            handleInputChange,
            validateField,
            handleSubmit,
            setSuccessMessage,
        },
    ] = useFormData<GameFormData, ValidGameFormData>({
        initialFormData,
        optionalFields: optionalFields.slice(),
        successMessageText: `${
            report ? "Modifications" : "Report"
        } submitted. Thank you!`,
        submit,
        toErrorMessage: toGameFormErrorMessage,
    });

    useGameReportClearEffects({
        formData,
        initialFormData: emptyFormData,
        setFormData,
        successMessage,
    });

    useConditionalActionEffect(!!successMessage, refreshGameReports);

    const { value: selectedExpansions } = formData.expansions;

    const freeStrongholdOptions: Stronghold[] = strongholds.filter(
        (stronghold) =>
            strongholdSide(selectedExpansions, stronghold) === "Free" &&
            isStrongholdInPlay(selectedExpansions, stronghold)
    );

    const shadowStrongholdOptions: Stronghold[] = strongholds.filter(
        (stronghold) =>
            strongholdSide(selectedExpansions, stronghold) === "Shadow" &&
            isStrongholdInPlay(selectedExpansions, stronghold)
    );

    const layoutTheme = report ? "minimal" : "default";

    const containerStyle = {
        display: "flex",
        flexDirection: "column",
        px: 8,
        py: 3,
    };

    return (
        <Sheet
            sx={
                report
                    ? {
                          ...containerStyle,
                          gap: 3,
                          fontSize: "sm",
                      }
                    : {
                          ...containerStyle,
                          gap: 2,
                          mx: 10,
                          my: 4,
                          borderRadius: "lg",
                          boxShadow: "lg",
                          backgroundColor: "lavender",
                      }
            }
        >
            <h1>
                {report ? "Edit Game Report" : "War of the Ring Game Report"}
            </h1>

            <Modal
                open={!!successMessage}
                onClose={() => {
                    setSuccessMessage(null);
                    if (exit) exit();
                }}
            >
                <ModalDialog>
                    <ModalClose />
                    <Typography mt={2}>{successMessage}</Typography>
                </ModalDialog>
            </Modal>

            <FormElement
                label={"Who won?"}
                error={formData.winner.error}
                layoutTheme={layoutTheme}
            >
                <Autocomplete
                    options={playerNames}
                    current={formData.winner.value || ""}
                    loading={loadingLeaderboard}
                    alertText={
                        report
                            ? undefined
                            : !!formData.winner.value &&
                              !playerNames.includes(formData.winner.value)
                            ? ErrorMessage.MissingPlayerName
                            : ""
                    }
                    placeholder={`Player Name${
                        report ? "" : " - Please check spelling!"
                    }`}
                    onInputValueChange={handleInputChange("winner")}
                    validate={validateField("winner")}
                />
            </FormElement>
            <FormElement
                label={"Who lost?"}
                error={formData.loser.error}
                layoutTheme={layoutTheme}
            >
                <Autocomplete
                    options={playerNames}
                    current={formData.loser.value || ""}
                    loading={loadingLeaderboard}
                    alertText={
                        report
                            ? undefined
                            : !!formData.loser.value &&
                              !playerNames.includes(formData.loser.value)
                            ? ErrorMessage.MissingPlayerName
                            : ""
                    }
                    placeholder={`Player Name${
                        report ? "" : " - Please check spelling!"
                    }`}
                    onInputValueChange={handleInputChange("loser")}
                    validate={validateField("loser")}
                />
            </FormElement>
            <FormElement
                label={"What side did the winner play?"}
                error={formData.side.error}
                layoutTheme={layoutTheme}
            >
                <SingleOptionInput
                    values={sides.slice()}
                    current={formData.side.value}
                    onChange={handleInputChange("side")}
                    validate={validateField("side")}
                />
            </FormElement>
            <FormElement
                label={"How did you win?"}
                error={formData.victory.error}
                layoutTheme={layoutTheme}
            >
                <SingleOptionInput
                    values={victoryTypes.slice()}
                    current={formData.victory.value}
                    onChange={handleInputChange("victory")}
                    validate={validateField("victory")}
                />
            </FormElement>
            <FormElement
                label={"What type of game did you play?"}
                error={formData.match.error}
                layoutTheme={layoutTheme}
            >
                <SingleOptionInput
                    values={matchTypes.slice()}
                    current={formData.match.value}
                    onChange={handleInputChange("match")}
                    validate={validateField("match")}
                />
            </FormElement>
            {formData.match.value === "Rated" && (
                <FormElement
                    label={"Was this for a specific competition?"}
                    error={formData.competition.error}
                    hasSingleControl={false}
                    layoutTheme={layoutTheme}
                >
                    <MultiOptionInput
                        values={competitionTypes.slice()}
                        current={formData.competition.value}
                        onChange={handleInputChange("competition")}
                        validate={validateField("competition")}
                    />
                </FormElement>
            )}
            {formData.match.value === "Rated" &&
                formData.competition.value.includes("League") && (
                    <FormElement
                        label={"Which League?"}
                        error={formData.league.error}
                        layoutTheme={layoutTheme}
                    >
                        <SingleOptionInput
                            values={leagues.slice()}
                            current={formData.league.value}
                            onChange={handleInputChange("league")}
                            validate={validateField("league")}
                            getLabel={getLeagueLabel}
                        />
                    </FormElement>
                )}
            <FormElement
                label={"Did you use any expansions?"}
                error={formData.usedExpansions.error}
                layoutTheme={layoutTheme}
            >
                <SingleOptionInput
                    values={[true, false]}
                    current={formData.usedExpansions.value}
                    getLabel={(v) => (v ? "Yes" : "No")}
                    onChange={handleInputChange("usedExpansions")}
                    validate={validateField("usedExpansions")}
                />
            </FormElement>
            {formData.usedExpansions.value && (
                <FormElement
                    label={"What expansions did you use?"}
                    error={formData.expansions.error}
                    hasSingleControl={false}
                    layoutTheme={layoutTheme}
                >
                    <MultiOptionInput
                        values={expansions.slice()}
                        current={formData.expansions.value}
                        onChange={handleInputChange("expansions")}
                        validate={validateField("expansions")}
                        getLabel={getExpansionLabel}
                    />
                </FormElement>
            )}
            {formData.usedExpansions.value &&
                formData.expansions.value.includes("Treebeard") && (
                    <FormElement
                        label={"Was Treebeard mustered?"}
                        error={formData.treebeard.error}
                        layoutTheme={layoutTheme}
                    >
                        <SingleOptionInput
                            values={[true, false]}
                            current={formData.treebeard.value}
                            getLabel={(v) => (v ? "Yes" : "No")}
                            onChange={handleInputChange("treebeard")}
                            validate={validateField("treebeard")}
                        />
                    </FormElement>
                )}
            <FormElement
                label={"Did you use a handicap mechanism (e.g. Action Tokens)?"}
                error={formData.usedHandicap.error}
                layoutTheme={layoutTheme}
            >
                <SingleOptionInput
                    values={[true, false]}
                    current={formData.usedHandicap.value}
                    getLabel={(v) => (v ? "Yes" : "No")}
                    onChange={handleInputChange("usedHandicap")}
                    validate={validateField("usedHandicap")}
                />
            </FormElement>
            {formData.usedHandicap.value && (
                <>
                    <FormElement
                        label="How many Action Tokens?"
                        error={formData.actionTokens.error}
                        layoutTheme={layoutTheme}
                    >
                        <SelectNumericOptionInput
                            start={0}
                            end={8}
                            current={formData.actionTokens.value}
                            initializeEmpty={false}
                            allowInfinite={true}
                            onChange={handleInputChange("actionTokens")}
                            validate={validateField("actionTokens")}
                        />
                    </FormElement>
                    <FormElement
                        label="How many Dwarven Rings?"
                        error={formData.dwarvenRings.error}
                        layoutTheme={layoutTheme}
                    >
                        <SelectNumericOptionInput
                            start={0}
                            end={8}
                            current={formData.dwarvenRings.value}
                            initializeEmpty={false}
                            allowInfinite={true}
                            onChange={handleInputChange("dwarvenRings")}
                            validate={validateField("dwarvenRings")}
                        />
                    </FormElement>
                </>
            )}
            <FormElement
                label="On what turn did the game end?"
                error={formData.turns.error}
                layoutTheme={layoutTheme}
            >
                <SelectNumericOptionInput
                    start={1}
                    end={25}
                    current={formData.turns.value}
                    onChange={handleInputChange("turns")}
                    validate={validateField("turns")}
                />
            </FormElement>
            <FormElement
                label="How much corruption did the Fellowship have?"
                error={formData.corruption.error}
                layoutTheme={layoutTheme}
            >
                <SelectNumericOptionInput
                    start={0}
                    end={18}
                    current={formData.corruption.value}
                    onChange={handleInputChange("corruption")}
                    validate={validateField("corruption")}
                />
            </FormElement>
            <FormElement
                label={"Did the Fellowship reach Mordor?"}
                error={formData.didFellowshipReachMordor.error}
                layoutTheme={layoutTheme}
            >
                <SingleOptionInput
                    values={[true, false]}
                    current={formData.didFellowshipReachMordor.value}
                    getLabel={(v) => (v ? "Yes" : "No")}
                    onChange={handleInputChange("didFellowshipReachMordor")}
                    validate={validateField("didFellowshipReachMordor")}
                />
            </FormElement>
            {formData.didFellowshipReachMordor.value && (
                <FormElement
                    label="Where did the Fellowship reach on the Mordor track?"
                    helpProps={{
                        content: <img src={mordorStepsPath} />,
                        iconStyle: {
                            paddingLeft: "5px",
                            fontSize: "23px",
                            color: GAME_FORM_BOLD,
                        },
                    }}
                    error={formData.mordor.error}
                    layoutTheme={layoutTheme}
                >
                    <SelectNumericOptionInput
                        start={0}
                        end={5}
                        current={formData.mordor.value}
                        onChange={handleInputChange("mordor")}
                        validate={validateField("mordor")}
                    />
                </FormElement>
            )}
            <FormElement
                label="How many eyes did Shadow allocate on turn 1? (Before action dice were rolled)"
                error={formData.initialEyes.error}
                layoutTheme={layoutTheme}
            >
                <SelectNumericOptionInput
                    start={0}
                    end={7}
                    current={formData.initialEyes.value}
                    onChange={handleInputChange("initialEyes")}
                    validate={validateField("initialEyes")}
                />
            </FormElement>
            <FormElement
                label={"Was Aragorn crowned King?"}
                error={formData.wasAragornCrowned.error}
                layoutTheme={layoutTheme}
            >
                <SingleOptionInput
                    values={[true, false]}
                    current={formData.wasAragornCrowned.value}
                    getLabel={(v) => (v ? "Yes" : "No")}
                    onChange={handleInputChange("wasAragornCrowned")}
                    validate={validateField("wasAragornCrowned")}
                />
            </FormElement>
            {formData.wasAragornCrowned.value && (
                <FormElement
                    label="On what turn was Aragorn crowned King?"
                    error={formData.aragornTurn.error}
                    layoutTheme={layoutTheme}
                >
                    <SelectNumericOptionInput
                        start={1}
                        end={18}
                        current={formData.aragornTurn.value}
                        onChange={handleInputChange("aragornTurn")}
                        validate={validateField("aragornTurn")}
                    />
                </FormElement>
            )}
            <FormElement
                label={"What strongholds were captured by Shadow?"}
                error={formData.strongholds.error}
                hasSingleControl={false}
                layoutTheme={layoutTheme}
            >
                <VictoryPoints
                    strongholds={formData.strongholds.value}
                    strongholdOptions={freeStrongholdOptions}
                />
                <MultiOptionInput
                    values={freeStrongholdOptions}
                    current={formData.strongholds.value}
                    onChange={handleInputChange("strongholds")}
                    validate={validateField("strongholds")}
                    getLabel={getStrongholdLabel}
                />
            </FormElement>
            <FormElement
                label={"What strongholds were captured by Free?"}
                error={formData.strongholds.error}
                hasSingleControl={false}
                layoutTheme={layoutTheme}
            >
                <VictoryPoints
                    strongholds={formData.strongholds.value}
                    strongholdOptions={shadowStrongholdOptions}
                />
                <MultiOptionInput
                    values={shadowStrongholdOptions}
                    current={formData.strongholds.value}
                    onChange={handleInputChange("strongholds")}
                    validate={validateField("strongholds")}
                    getLabel={getStrongholdLabel}
                />
            </FormElement>
            <FormElement
                label={
                    "On a scale from 1-10, how interesting did you find this game?"
                }
                error={formData.interestRating.error}
                layoutTheme={layoutTheme}
            >
                <SelectNumericOptionInput
                    start={1}
                    end={10}
                    current={formData.interestRating.value}
                    onChange={handleInputChange("interestRating")}
                    validate={validateField("interestRating")}
                />
            </FormElement>
            <FormElement
                label={"Do you have any comments or questions?"}
                error={formData.comment.error}
                layoutTheme={layoutTheme}
            >
                <TextInput
                    value={formData.comment.value || ""}
                    placeholder=""
                    onChange={handleInputChange("comment")}
                    validate={validateField("comment")}
                />
            </FormElement>
            {!report && (
                <FormElement
                    error={formData.logFile.error}
                    label={
                        "Game log (mandatory for rank 30 and higher, or for tournament and league games):"
                    }
                >
                    <FileUpload
                        value={formData.logFile.value}
                        id="game-log-upload"
                        constraintText={`Max ${MAX_GAME_LOG_SIZE_MB} MB`}
                        validate={validateField("logFile")}
                        onChange={handleInputChange(
                            "logFile",
                            (file: File | null) => {
                                return file
                                    ? file.size <= MAX_GAME_LOG_SIZE_BYTES
                                        ? null
                                        : "File too large"
                                    : null;
                            }
                        )}
                    />
                </FormElement>
            )}
            {errorOnSubmit && <ErrorDisplay message={errorOnSubmit} />}
            <Box
                sx={
                    report
                        ? {
                              display: "flex",
                              alignItems: "center",
                              justifyContent: "end",
                              width: "100%",
                              gap: 2,
                          }
                        : {
                              display: "flex",
                              flexDirection: "column",
                          }
                }
            >
                {exit && <Button onClick={exit}>Cancel</Button>}
                <Button
                    onClick={handleSubmit}
                    disabled={submitting}
                    startDecorator={
                        submitting ? <CircularProgress /> : undefined
                    }
                >
                    {submitting ? "Submitting..." : "Submit"}
                </Button>
            </Box>
        </Sheet>
    );
}

export default GameReportForm;

async function submit(validatedResult: ValidGameFormData) {
    return await axios.post(
        // `http://localhost:8081/${
        `https://api.waroftheringcommunity.net:8080/${
            typeof validatedResult.rid.value === "number"
                ? "modifyReport"
                : "submitReport"
        }`,
        toPayload(validatedResult),
        {
            headers: {
                "Content-Type": validatedResult.rid.value
                    ? "application/json"
                    : "multipart/form-data",
            },
        }
    );
}

function toPayload(formData: ValidGameFormData): GameReportPayload | FormData {
    const unencodedPayload: GameReportPayload = {
        rid: formData.rid.value,
        logFile: formData.logFile.value,
        timestamp: formData.timestamp.value,
        report: {
            winner: formData.winner.value,
            loser: formData.loser.value,
            side: formData.side.value,
            victory: formData.victory.value,
            match: formData.match.value,
            competition: formData.competition.value,
            league: formData.league.value,
            expansions: formData.expansions.value,
            treebeard: formData.treebeard.value,
            actionTokens: formData.actionTokens.value,
            dwarvenRings: formData.dwarvenRings.value,
            turns: formData.turns.value,
            corruption: formData.corruption.value,
            mordor: formData.mordor.value,
            initialEyes: formData.initialEyes.value,
            aragornTurn: formData.aragornTurn.value,
            strongholds: formData.strongholds.value,
            interestRating: formData.interestRating.value,
            comment: formData.comment.value,
        },
    };
    return typeof formData.rid.value === "number"
        ? unencodedPayload
        : toFormData(unencodedPayload);
}

function toFormData(unencodedPayload: GameReportPayload): FormData {
    const { logFile, report } = unencodedPayload;
    const formData = new FormData();

    if (logFile) formData.append("logFile", logFile);
    formData.append("report", JSON.stringify(report));

    return formData;
}

function toGameFormErrorMessage(error: ServerErrorBody): string {
    if (error.status === 422) {
        const parsedResult = parseValidationResult(error.response.data);
        if (parsedResult.length) {
            const errorMessage = parsedResult
                .map(validationErrorToMessage)
                .join(", ");
            return (
                errorMessage.slice(0, 1).toUpperCase() + errorMessage.slice(1)
            );
        }
    }
    return toErrorMessage(error);
}

function parseValidationResult(
    serverValidationResult: string
): ServerValidationError[] {
    const parsedResult = serverValidationResult
        .slice(1, serverValidationResult.length - 1)
        .split(",");

    return parsedResult.every((error) =>
        serverValidationErrors.includes(error as ServerValidationError)
    )
        ? (parsedResult as ServerValidationError[])
        : [];
}

function validationErrorToMessage(
    validationError: ServerValidationError
): string {
    switch (validationError) {
        case "VictoryConditionConflictSPRV":
            return "conditions met for Shadow ring victory instead of selected victory type";
        case "VictoryConditionConflictFPRV":
            return "conditions met for Free Peoples ring victory instead of selected victory type";
        case "VictoryConditionConflictSPMV":
            return "conditions met for Shadow military victory instead of selected victory type";
        case "VictoryConditionConflictFPMV":
            return "conditions met for Free Peoples military victory instead of selected victory type";
        case "VictoryConditionConflictConcession":
            return "conditions met for Concession victory type instead of selected victory type";
        case "NoVictoryConditionMet":
            return "no victory conditions met";
        case "InvalidSPMV":
            return "invalid Shadow military victory";
        case "InvalidFPMV":
            return "invalid Free Peoples military victory";
        case "InvalidSPRV":
            return "invalid Shadow ring victory";
        case "InvalidFPRV":
            return "invalid Free Peoples ring victory";
        case "CompetitionMismatch":
            return "reported competition type does not match reported league";
        case "LeagueExpansionMismatch":
            return "reported league does not match reported expansions";
        case "TreebeardExpansionMismatch":
            return "reported Treebeard muster does not match reported expansions";
        case "TurnsOutOfRange":
            return "invalid ending game turn selection";
        case "CorruptionOutOfRange":
            return "invalid fellowship corruption selection";
        case "MordorOutOfRange":
            return "invalid Mordor track selection";
        case "InitialEyesOutOfRange":
            return "invalid number of eyes allocated by Shadow on turn 1";
        case "InterestRatingOutOfRange":
            return "invalid interest rating selection";
        case "InvalidStronghold":
            return "invalid stronghold selections for the indicated expansions";
    }
}
