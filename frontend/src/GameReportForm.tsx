import "@fontsource/inter";
import React from "react";
import { FormControl, FormHelperText, List, Typography } from "@mui/joy";
import Sheet from "@mui/joy/Sheet";
import FormLabel from "@mui/joy/FormLabel";
import Radio from "@mui/joy/Radio";
import RadioGroup from "@mui/joy/RadioGroup";
import Checkbox from "@mui/joy/Checkbox";
import Input from "@mui/joy/Input";
import ListItem from "@mui/joy/ListItem";
import Select from "@mui/joy/Select";
import Option from "@mui/joy/Option";
import Button from "@mui/joy/Button";
import axios from "axios";
import { FieldError, FormData, Stronghold, ValueOf } from "./types";
import {
    cities,
    competitionTypes,
    expansions,
    INFINITE,
    leagues,
    matchTypes,
    sides,
    strongholds,
    victoryTypes,
} from "./constants";
import useFormData from "./hooks/useFormData";

function GameReportForm() {
    const [
        formData,
        { errorOnSubmit },
        { handleInputChange, validateField, handleSubmit },
    ] = useFormData();

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
            <GameReportFormElement
                label={"Who won?"}
                error={formData.winner.error}
            >
                <TextInput
                    value={formData.winner.value || ""}
                    placeholder="Player Name - Please check spelling!"
                    onChange={handleInputChange("winner")}
                    validate={validateField("winner")}
                />
            </GameReportFormElement>
            <GameReportFormElement
                label={"Who lost?"}
                error={formData.loser.error}
            >
                <TextInput
                    value={formData.loser.value || ""}
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
                >
                    <MultiOptionInput
                        values={expansions.slice()}
                        current={formData.expansions.value}
                        onChange={handleInputChange("expansions")}
                        validate={validateField("expansions")}
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
                        onChange={handleInputChange("aragornTurn")}
                        validate={validateField("aragornTurn")}
                    />
                </GameReportFormElement>
            )}
            <GameReportFormElement
                label={"What strongholds were captured by Shadow?"}
                error={formData.strongholds.error}
            >
                <VictoryPoints
                    strongholds={formData.strongholds.value}
                ></VictoryPoints>
                <MultiOptionInput
                    values={strongholds.slice()}
                    current={formData.strongholds.value}
                    onChange={handleInputChange("strongholds")}
                    validate={validateField("strongholds")}
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
            <Button onClick={handleSubmit}>Submit</Button>
            {errorOnSubmit && (
                <Typography color="danger">{errorOnSubmit}</Typography>
            )}
        </Sheet>
    );
}

interface GameReportElementProps {
    children: React.ReactNode;
    label: string;
    error?: FieldError;
}

function GameReportFormElement({
    children,
    label,
    error,
}: GameReportElementProps) {
    return (
        <Sheet variant="outlined" sx={{ p: 2, borderRadius: "lg" }}>
            <FormControl error={!!error}>
                <FormLabel sx={{ fontSize: 16, pb: 2 }}>{label}</FormLabel>
                {children}
                {error && <FormHelperText>{error}</FormHelperText>}
            </FormControl>
        </Sheet>
    );
}

interface TextInputProps {
    value: string;
    placeholder: string;
    onChange: (value: string) => void;
    validate: () => void;
}

function TextInput({ value, placeholder, onChange, validate }: TextInputProps) {
    return (
        <Input
            value={value}
            placeholder={placeholder}
            onChange={(event) => onChange(event.target.value)}
            onBlur={validate}
        />
    );
}

interface SelectOptionInputProps<T> {
    values: T[];
    current: T;
    getLabel?: (value: T) => string;
    onChange: (value: T) => void;
    validate: () => void;
}

function SelectOptionInput<T>({
    values,
    current,
    getLabel = String,
    onChange,
    validate,
}: SelectOptionInputProps<T>) {
    return (
        <Select
            defaultValue={values[0]}
            onChange={(_, value) => onChange(value as T)}
            onClose={validate}
        >
            {values.map((value, i) => (
                <Option key={i} value={value}>
                    {getLabel(value)}
                </Option>
            ))}
        </Select>
    );
}

interface SelectNumericOptionInputProps {
    start: number;
    end: number;
    initializeEmpty?: boolean;
    allowInfinite?: boolean;
    onChange: (value: number | null) => void;
    validate: () => void;
}

function SelectNumericOptionInput({
    start,
    end,
    initializeEmpty = true,
    allowInfinite = false,
    onChange,
    validate,
}: SelectNumericOptionInputProps) {
    const values: (number | null)[] = initializeEmpty ? [null] : [];
    for (let i = start; i <= end; i++) {
        values.push(i);
    }
    if (allowInfinite) {
        values.push(INFINITE);
    }

    return (
        <SelectOptionInput
            values={values}
            current={start}
            getLabel={(value) => {
                if (value === null) {
                    return "";
                } else if (value === INFINITE) {
                    return "Unlimited!";
                } else {
                    return String(value);
                }
            }}
            onChange={onChange}
            validate={validate}
        />
    );
}

interface SingleOptionInputProps<T> {
    values: T[];
    current: T;
    getLabel?: (value: T) => string;
    onChange: (value: T) => void;
    validate: () => void;
}

function SingleOptionInput<T>({
    values,
    current,
    getLabel = String,
    onChange,
    validate,
}: SingleOptionInputProps<T>) {
    const handleChange = (event: React.ChangeEvent<HTMLInputElement>) => {
        const selectedValue = values.find(
            (value) => String(value) === event.target.value
        );
        if (selectedValue !== undefined) {
            onChange(selectedValue);
            validate();
        }
    };

    return (
        <RadioGroup
            value={current}
            orientation="horizontal"
            onChange={handleChange}
        >
            {values.map((value, i) => (
                <Radio key={i} value={value} label={getLabel(value)} />
            ))}
        </RadioGroup>
    );
}

interface MultiOptionInputProps<T> {
    values: T[];
    current: T[];
    getLabel?: (value: T) => string;
    onChange: (value: T[]) => void;
    validate: () => void;
}

function MultiOptionInput<T>({
    values,
    current,
    getLabel = String,
    onChange,
    validate,
}: MultiOptionInputProps<T>) {
    const handleChange = (event: React.ChangeEvent<HTMLInputElement>) => {
        const selectedValue = values.find(
            (value) => String(value) === event.target.value
        );
        if (selectedValue !== undefined) {
            if (event.target.checked) {
                onChange(current.concat(selectedValue));
            } else {
                onChange(current.filter((value) => value !== selectedValue));
            }
            validate();
        }
    };

    return (
        <List
            orientation="horizontal"
            wrap
            sx={{
                "--List-gap": "10px",
                "--ListItem-radius": "30px",
                "--ListItem-minHeight": "32px",
            }}
        >
            {values.map((value, i) => (
                <ListItem key={i}>
                    <Checkbox
                        checked={current.includes(value)}
                        size="sm"
                        disableIcon
                        overlay
                        value={getLabel(value)}
                        label={getLabel(value)}
                        onChange={handleChange}
                    />
                </ListItem>
            ))}
        </List>
    );
}

interface VictoryPointsProps {
    strongholds: Stronghold[];
}

function VictoryPoints({ strongholds }: VictoryPointsProps) {
    const points = strongholds
        .map((stronghold) => (cities.includes(stronghold) ? 1 : 2))
        .reduce((sum, current) => sum + current, 0);
    return (
        <Typography sx={{ fontWeight: "bold", pb: 2 }}>VP: {points}</Typography>
    );
}

export default GameReportForm;
