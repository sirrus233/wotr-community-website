import "@fontsource/inter";
import React from "react";
import CircularProgress from "@mui/joy/CircularProgress";
import Modal from "@mui/joy/Modal";
import ModalClose from "@mui/joy/ModalClose";
import ModalDialog from "@mui/joy/ModalDialog";
import Typography from "@mui/joy/Typography";
import Sheet from "@mui/joy/Sheet";
import Button from "@mui/joy/Button";
import {
    competitionTypes,
    ErrorMessage,
    expansions,
    leagues,
    matchTypes,
    sides,
    strongholds,
    victoryTypes,
} from "./constants";
import { Stronghold } from "./types";
import {
    getExpansionLabel,
    getLeagueLabel,
    getStrongholdLabel,
    strongholdSide,
} from "./utils";
import useFormData from "./hooks/useFormData";
import Autocomplete from "./Autocomplete";
import GameReportFormElement from "./GameReportFormElement";
import MultiOptionInput from "./MultiOptionInput";
import SelectNumericOptionInput from "./SelectNumericOptionInput";
import SingleOptionInput from "./SingleOptionInput";
import TextInput from "./TextInput";
import VictoryPoints from "./VictoryPoints";

function GameReportForm() {
    const [
        formData,
        { errorOnSubmit, successMessage, loading, loadingPlayers, playerNames },
        {
            handleInputChange,
            validateField,
            handleSubmit,
            isStrongholdInPlay,
            setSuccessMessage,
        },
    ] = useFormData();

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

    return (
        <Sheet
            sx={{
                mx: 10,
                my: 4,
                px: 8,
                py: 3,
                display: "flex",
                flexDirection: "column",
                gap: 2,
                borderRadius: "lg",
                boxShadow: "lg",
                backgroundColor: "lavender",
            }}
        >
            <h1>War of the Ring Game Report</h1>

            <Modal
                open={!!successMessage}
                onClose={() => setSuccessMessage(null)}
            >
                <ModalDialog>
                    <ModalClose />
                    <Typography>{successMessage}</Typography>
                </ModalDialog>
            </Modal>

            <GameReportFormElement
                label={"Who won?"}
                error={formData.winner.error}
            >
                <Autocomplete
                    options={playerNames}
                    current={formData.winner.value || ""}
                    loading={loadingPlayers}
                    alertText={
                        !!formData.winner.value &&
                        !playerNames.includes(formData.winner.value)
                            ? ErrorMessage.MissingPlayerName
                            : ""
                    }
                    placeholder="Player Name - Please check spelling!"
                    onChange={handleInputChange("winner")}
                    validate={validateField("winner")}
                />
            </GameReportFormElement>
            <GameReportFormElement
                label={"Who lost?"}
                error={formData.loser.error}
            >
                <Autocomplete
                    options={playerNames}
                    current={formData.loser.value || ""}
                    loading={loadingPlayers}
                    alertText={
                        !!formData.loser.value &&
                        !playerNames.includes(formData.loser.value)
                            ? ErrorMessage.MissingPlayerName
                            : ""
                    }
                    placeholder="Player Name - Please check spelling!"
                    onChange={handleInputChange("loser")}
                    validate={validateField("loser")}
                />
            </GameReportFormElement>
            <GameReportFormElement
                label={"What side did the winner play?"}
                error={formData.side.error}
            >
                <SingleOptionInput
                    values={sides.slice()}
                    current={formData.side.value}
                    onChange={handleInputChange("side")}
                    validate={validateField("side")}
                />
            </GameReportFormElement>
            <GameReportFormElement
                label={"How did you win?"}
                error={formData.victory.error}
            >
                <SingleOptionInput
                    values={victoryTypes.slice()}
                    current={formData.victory.value}
                    onChange={handleInputChange("victory")}
                    validate={validateField("victory")}
                />
            </GameReportFormElement>
            <GameReportFormElement
                label={"What type of game did you play?"}
                error={formData.match.error}
            >
                <SingleOptionInput
                    values={matchTypes.slice()}
                    current={formData.match.value}
                    onChange={handleInputChange("match")}
                    validate={validateField("match")}
                />
            </GameReportFormElement>
            {formData.match.value === "Ranked" && (
                <GameReportFormElement
                    label={"Was this for a specific competition?"}
                    error={formData.competition.error}
                    hasSingleControl={false}
                >
                    <MultiOptionInput
                        values={competitionTypes.slice()}
                        current={formData.competition.value}
                        onChange={handleInputChange("competition")}
                        validate={validateField("competition")}
                    />
                </GameReportFormElement>
            )}
            {formData.match.value === "Ranked" &&
                formData.competition.value.includes("League") && (
                    <GameReportFormElement
                        label={"Which League?"}
                        error={formData.league.error}
                    >
                        <SingleOptionInput
                            values={leagues.slice()}
                            current={formData.league.value}
                            onChange={handleInputChange("league")}
                            validate={validateField("league")}
                            getLabel={getLeagueLabel}
                        />
                    </GameReportFormElement>
                )}
            <GameReportFormElement
                label={"Did you use any expansions?"}
                error={formData.usedExpansions.error}
            >
                <SingleOptionInput
                    values={[true, false]}
                    current={formData.usedExpansions.value}
                    getLabel={(v) => (v ? "Yes" : "No")}
                    onChange={handleInputChange("usedExpansions")}
                    validate={validateField("usedExpansions")}
                />
            </GameReportFormElement>
            {formData.usedExpansions.value && (
                <GameReportFormElement
                    label={"What expansions did you use?"}
                    error={formData.expansions.error}
                    hasSingleControl={false}
                >
                    <MultiOptionInput
                        values={expansions.slice()}
                        current={formData.expansions.value}
                        onChange={handleInputChange("expansions")}
                        validate={validateField("expansions")}
                        getLabel={getExpansionLabel}
                    />
                </GameReportFormElement>
            )}
            {formData.usedExpansions.value &&
                formData.expansions.value.includes("Treebeard") && (
                    <GameReportFormElement
                        label={"Was Treebeard mustered?"}
                        error={formData.treebeard.error}
                    >
                        <SingleOptionInput
                            values={[true, false]}
                            current={formData.treebeard.value}
                            getLabel={(v) => (v ? "Yes" : "No")}
                            onChange={handleInputChange("treebeard")}
                            validate={validateField("treebeard")}
                        />
                    </GameReportFormElement>
                )}
            <GameReportFormElement
                label={"Did you use a handicap mechanism (e.g. Action Tokens)?"}
                error={formData.usedHandicap.error}
            >
                <SingleOptionInput
                    values={[true, false]}
                    current={formData.usedHandicap.value}
                    getLabel={(v) => (v ? "Yes" : "No")}
                    onChange={handleInputChange("usedHandicap")}
                    validate={validateField("usedHandicap")}
                />
            </GameReportFormElement>
            {formData.usedHandicap.value && (
                <>
                    <GameReportFormElement
                        label="How many Action Tokens?"
                        error={formData.actionTokens.error}
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
                    </GameReportFormElement>
                    <GameReportFormElement
                        label="How many Dwarven Rings?"
                        error={formData.dwarvenRings.error}
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
                    </GameReportFormElement>
                </>
            )}
            <GameReportFormElement
                label="On what turn did the game end?"
                error={formData.turns.error}
            >
                <SelectNumericOptionInput
                    start={1}
                    end={25}
                    current={formData.turns.value}
                    onChange={handleInputChange("turns")}
                    validate={validateField("turns")}
                />
            </GameReportFormElement>
            <GameReportFormElement
                label="How much corruption did the Fellowship have?"
                error={formData.corruption.error}
            >
                <SelectNumericOptionInput
                    start={0}
                    end={18}
                    current={formData.corruption.value}
                    onChange={handleInputChange("corruption")}
                    validate={validateField("corruption")}
                />
            </GameReportFormElement>
            <GameReportFormElement
                label={"Did the Fellowship reach Mordor?"}
                error={formData.didFellowshipReachMordor.error}
            >
                <SingleOptionInput
                    values={[true, false]}
                    current={formData.didFellowshipReachMordor.value}
                    getLabel={(v) => (v ? "Yes" : "No")}
                    onChange={handleInputChange("didFellowshipReachMordor")}
                    validate={validateField("didFellowshipReachMordor")}
                />
            </GameReportFormElement>
            {formData.didFellowshipReachMordor.value && (
                <GameReportFormElement
                    label="Where did the Fellowship reach on the Mordor track?"
                    error={formData.mordor.error}
                >
                    <SelectNumericOptionInput
                        start={0}
                        end={5}
                        current={formData.mordor.value}
                        onChange={handleInputChange("mordor")}
                        validate={validateField("mordor")}
                    />
                </GameReportFormElement>
            )}
            <GameReportFormElement
                label="How many eyes did Shadow allocate on turn 1? (Before action dice were rolled)"
                error={formData.initialEyes.error}
            >
                <SelectNumericOptionInput
                    start={0}
                    end={7}
                    current={formData.initialEyes.value}
                    onChange={handleInputChange("initialEyes")}
                    validate={validateField("initialEyes")}
                />
            </GameReportFormElement>
            <GameReportFormElement
                label={"Was Aragorn crowned King?"}
                error={formData.wasAragornCrowned.error}
            >
                <SingleOptionInput
                    values={[true, false]}
                    current={formData.wasAragornCrowned.value}
                    getLabel={(v) => (v ? "Yes" : "No")}
                    onChange={handleInputChange("wasAragornCrowned")}
                    validate={validateField("wasAragornCrowned")}
                />
            </GameReportFormElement>
            {formData.wasAragornCrowned.value && (
                <GameReportFormElement
                    label="On what turn was Aragorn crowned King?"
                    error={formData.aragornTurn.error}
                >
                    <SelectNumericOptionInput
                        start={1}
                        end={18}
                        current={formData.aragornTurn.value}
                        onChange={handleInputChange("aragornTurn")}
                        validate={validateField("aragornTurn")}
                    />
                </GameReportFormElement>
            )}
            <GameReportFormElement
                label={"What strongholds were captured by Shadow?"}
                error={formData.strongholds.error}
                hasSingleControl={false}
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
            </GameReportFormElement>
            <GameReportFormElement
                label={"What strongholds were captured by Free?"}
                error={formData.strongholds.error}
                hasSingleControl={false}
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
            </GameReportFormElement>
            <GameReportFormElement
                label={
                    "On a scale from 1-10, how interesting did you find this game?"
                }
                error={formData.interestRating.error}
            >
                <SelectNumericOptionInput
                    start={1}
                    end={10}
                    current={formData.interestRating.value}
                    onChange={handleInputChange("interestRating")}
                    validate={validateField("interestRating")}
                />
            </GameReportFormElement>
            <GameReportFormElement
                label={"Do you have any comments or questions?"}
                error={formData.comment.error}
            >
                <TextInput
                    value={formData.comment.value || ""}
                    placeholder=""
                    onChange={handleInputChange("comment")}
                    validate={validateField("comment")}
                />
            </GameReportFormElement>
            <Button
                onClick={handleSubmit}
                disabled={loading}
                startDecorator={loading ? <CircularProgress /> : undefined}
            >
                {loading ? "Submitting..." : "Submit"}
            </Button>
            {errorOnSubmit && (
                <Typography color="danger">{errorOnSubmit}</Typography>
            )}
        </Sheet>
    );
}

export default GameReportForm;
