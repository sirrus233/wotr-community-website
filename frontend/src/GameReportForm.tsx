import "@fontsource/inter";
import React, { useState, useEffect } from "react";
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

const sides = ["Free", "Shadow"] as const;
type Side = (typeof sides)[number];

const victoryTypes = ["Ring", "Military", "Concession"] as const;
type Victory = (typeof victoryTypes)[number];

const expansions = [
    "LoME",
    "WoME",
    "KoME",
    "Cities",
    "Fate of Erebor",
    "Treebeard",
] as const;
type Expansion = (typeof expansions)[number];

const matchType = ["Ranked", "Unranked"] as const;
type Match = (typeof matchType)[number];

const competitiveType = ["League", "Tournament"] as const;
type Competitive = (typeof competitiveType)[number];

const leagues = ["General", "LoME", "WoME", "Super", "TTS"] as const;
type League = (typeof leagues)[number];

const strongholds = [
    "Rivendell",
    "Grey Havens",
    "Helm's Deep",
    "Lorien",
    "Woodland Realm",
    "Erebor",
    "Minas Tirith",
    "Dol Amroth",
    "Shire",
    "Edoras",
    "Dale",
    "Pelargir",
    "Ered Luin (Cities expansion only)",
    "Iron Hills (Fate of Erebor expansion only)",
] as const;
type Stronghold = (typeof strongholds)[number];

const cities: Stronghold[] = [
    "Shire",
    "Edoras",
    "Dale",
    "Pelargir",
    "Ered Luin (Cities expansion only)",
    "Iron Hills (Fate of Erebor expansion only)",
];

enum ErrorMessage {
    Required = "Required",
    OnSubmit = "Could not submit, please resolve errors",
}

export type FieldError = string | null;

interface FieldData<T> {
    value: T;
    error: FieldError;
    validate: () => FieldError;
}

export interface FormData {
    winner: FieldData<string | null>;
    loser: FieldData<string | null>;
    side: FieldData<Side | null>;
    victoryType: FieldData<Victory | null>;
    matchType: FieldData<Match | null>;
    competitionTypes: FieldData<Competitive[]>;
    league: FieldData<League | null>;
    usedExpansions: FieldData<boolean | null>;
    expansions: FieldData<Expansion[]>;
    wasTreebeardMustered: FieldData<boolean | null>;
    usedHandicap: FieldData<boolean | null>;
    actionTokens: FieldData<number>;
    dwarvenRings: FieldData<number>;
    gameTurns: FieldData<number>;
    corruption: FieldData<number>;
    didFellowshipReachMordor: FieldData<boolean | null>;
    mordorTrack: FieldData<number>;
    initialEyes: FieldData<number>;
    wasAragornCrowned: FieldData<boolean | null>;
    aragornCrownedTurn: FieldData<number>;
    capturedStrongholds: FieldData<Stronghold[]>;
    interestRating: FieldData<number>;
    comment: FieldData<string | null>;
}

const UNINITIALIZED = -1;
const INFINITE = 100;

const initialFormData: FormData = {
    winner: {
        value: null,
        error: "",
        validate: function _() {
            return detectMissingInput(this.value);
        },
    },
    loser: {
        value: null,
        error: "",
        validate: function _() {
            return detectMissingInput(this.value);
        },
    },
    side: {
        value: null,
        error: "",
        validate: function _() {
            return detectMissingInput(this.value);
        },
    },
    victoryType: {
        value: null,
        error: "",
        validate: function _() {
            return detectMissingInput(this.value);
        },
    },
    matchType: {
        value: null,
        error: "",
        validate: function _() {
            return detectMissingInput(this.value);
        },
    },
    competitionTypes: {
        value: [],
        error: "",
        validate: () => null,
    },
    league: {
        value: null,
        error: "",
        validate: () => null,
    },
    usedExpansions: {
        value: null,
        error: "",
        validate: function _() {
            return detectMissingInput(this.value);
        },
    },
    expansions: {
        value: [],
        error: "",
        validate: () => null,
    },
    wasTreebeardMustered: {
        value: null,
        error: "",
        validate: () => null,
    },
    usedHandicap: {
        value: null,
        error: "",
        validate: function _() {
            return detectMissingInput(this.value);
        },
    },
    actionTokens: { value: 0, error: "", validate: () => null },
    dwarvenRings: { value: 0, error: "", validate: () => null },
    gameTurns: {
        value: UNINITIALIZED,
        error: "",
        validate: function _() {
            return detectMissingInput(this.value);
        },
    },
    corruption: {
        value: UNINITIALIZED,
        error: "",
        validate: function _() {
            return detectMissingInput(this.value);
        },
    },
    didFellowshipReachMordor: {
        value: null,
        error: "",
        validate: function _() {
            return detectMissingInput(this.value);
        },
    },
    mordorTrack: {
        value: UNINITIALIZED,
        error: "",
        validate: () => null,
    },
    initialEyes: {
        value: UNINITIALIZED,
        error: "",
        validate: function _() {
            return detectMissingInput(this.value);
        },
    },
    wasAragornCrowned: {
        value: null,
        error: "",
        validate: function _() {
            return detectMissingInput(this.value);
        },
    },
    aragornCrownedTurn: {
        value: UNINITIALIZED,
        error: "",
        validate: () => null,
    },
    capturedStrongholds: {
        value: [],
        error: "",
        validate: () => null,
    },
    interestRating: {
        value: -1,
        error: "",
        validate: function _() {
            return detectMissingInput(this.value);
        },
    },
    comment: { value: "", error: "", validate: () => null },
};

type ValueOf<T> = T[keyof T];

function GameReportForm() {
    const [formData, setFormData] = useState(initialFormData);
    const [errorOnSubmit, setErrorOnSubmit] = useState<FieldError>(null);

    const handleInputChange = <K extends keyof FormData>(field: K) => {
        return (value: FormData[K]["value"]) =>
            setFormData((prevData) => ({
                ...prevData,
                [field]: { ...prevData[field], value },
            }));
    };

    const validateField = <K extends keyof FormData>(field: K) => {
        return () => {
            setFormData((prevData) => {
                const error = prevData[field].validate();
                return error || prevData[field].error
                    ? {
                          ...prevData,
                          [field]: { ...prevData[field], error },
                      }
                    : prevData;
            });
        };
    };

    const validateForm = () => {
        const stateUpdates = objectKeys(formData).reduce<(() => void)[]>(
            (updates, field) => {
                const fieldError = formData[field].validate();
                if (fieldError) {
                    updates.push(() =>
                        setFormData((prevData) => ({
                            ...prevData,
                            [field]: { ...prevData[field], error: fieldError },
                        }))
                    );
                }
                return updates;
            },
            []
        );

        if (stateUpdates.length) {
            stateUpdates.forEach((update) => update());
            return ErrorMessage.OnSubmit;
        } else {
            return null;
        }
    };

    const toPayload = (formData: FormData) =>
        Object.fromEntries(
            Object.entries(formData).map(([field, fieldData]) => [
                field,
                fieldData.value,
            ])
        );

    const handleSubmit = async () => {
        try {
            const formError = validateForm();

            if (formError) {
                setErrorOnSubmit(formError);
            } else {
                setErrorOnSubmit(null);

                const response = await axios.post(
                    "http://localhost:3001/submit-report",
                    toPayload(formData),
                    {
                        headers: {
                            "Content-Type": "application/json",
                        },
                    }
                );

                console.log("Form submitted successfully:", response);
                // Handle the response data as needed
            }
        } catch (error) {
            console.error("Error submitting form:", error);
        }
    };

    const controlledClearEffect = <
        K extends keyof FormData,
        V extends ValueOf<FormData>["value"]
    >(
        controlField: V,
        clearField: K,
        controlCondition?: (value: V) => boolean
    ) => {
        const condition = controlCondition
            ? controlCondition(controlField)
            : controlField;
        useEffect(() => {
            if (!condition) {
                setFormData((formData) => ({
                    ...formData,
                    [clearField]: initialFormData[clearField],
                }));
            }
        }, [controlField]);
    };

    controlledClearEffect(
        formData.matchType.value,
        "competitionTypes",
        (matchType) => matchType === "Ranked"
    );
    controlledClearEffect(
        formData.matchType.value,
        "league",
        (matchType) => matchType === "Ranked"
    );
    controlledClearEffect(
        formData.competitionTypes.value,
        "league",
        (competitionTypes) => competitionTypes.includes("League")
    );
    controlledClearEffect(
        formData.expansions.value,
        "wasTreebeardMustered",
        (expansions) => expansions.includes("Treebeard")
    );
    controlledClearEffect(formData.usedExpansions.value, "expansions");
    controlledClearEffect(
        formData.usedExpansions.value,
        "wasTreebeardMustered"
    );
    controlledClearEffect(
        formData.expansions.value,
        "wasTreebeardMustered",
        (expansions) => expansions.includes("Treebeard")
    );
    controlledClearEffect(formData.usedHandicap.value, "actionTokens");
    controlledClearEffect(formData.usedHandicap.value, "dwarvenRings");
    controlledClearEffect(
        formData.didFellowshipReachMordor.value,
        "mordorTrack"
    );
    controlledClearEffect(
        formData.wasAragornCrowned.value,
        "aragornCrownedTurn"
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
                error={formData.victoryType.error}
            >
                <SingleOptionInput
                    values={victoryTypes.slice()}
                    current={formData.victoryType.value}
                    onChange={handleInputChange("victoryType")}
                    validate={validateField("victoryType")}
                />
            </GameReportFormElement>
            <GameReportFormElement
                label={"What type of game did you play?"}
                error={formData.matchType.error}
            >
                <SingleOptionInput
                    values={matchType.slice()}
                    current={formData.matchType.value}
                    onChange={handleInputChange("matchType")}
                    validate={validateField("matchType")}
                />
            </GameReportFormElement>
            {formData.matchType.value === "Ranked" && (
                <GameReportFormElement
                    label={"Was this for a specific competition?"}
                    error={formData.competitionTypes.error}
                >
                    <MultiOptionInput
                        values={competitiveType.slice()}
                        current={formData.competitionTypes.value}
                        onChange={handleInputChange("competitionTypes")}
                        validate={validateField("competitionTypes")}
                    />
                </GameReportFormElement>
            )}
            {formData.matchType.value === "Ranked" &&
                formData.competitionTypes.value.includes("League") && (
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
                        error={formData.wasTreebeardMustered.error}
                    >
                        <SingleOptionInput
                            values={[true, false]}
                            current={formData.wasTreebeardMustered.value}
                            getLabel={(v) => (v ? "Yes" : "No")}
                            onChange={handleInputChange("wasTreebeardMustered")}
                            validate={validateField("wasTreebeardMustered")}
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
                error={formData.gameTurns.error}
            >
                <SelectNumericOptionInput
                    start={1}
                    end={25}
                    onChange={handleInputChange("gameTurns")}
                    validate={validateField("gameTurns")}
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
                    error={formData.mordorTrack.error}
                >
                    <SelectNumericOptionInput
                        start={0}
                        end={5}
                        onChange={handleInputChange("mordorTrack")}
                        validate={validateField("mordorTrack")}
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
                    error={formData.aragornCrownedTurn.error}
                >
                    <SelectNumericOptionInput
                        start={1}
                        end={18}
                        onChange={handleInputChange("aragornCrownedTurn")}
                        validate={validateField("aragornCrownedTurn")}
                    />
                </GameReportFormElement>
            )}
            <GameReportFormElement
                label={"What strongholds were captured by Shadow?"}
                error={formData.capturedStrongholds.error}
            >
                <VictoryPoints
                    strongholds={formData.capturedStrongholds.value}
                ></VictoryPoints>
                <MultiOptionInput
                    values={strongholds.slice()}
                    current={formData.capturedStrongholds.value}
                    onChange={handleInputChange("capturedStrongholds")}
                    validate={validateField("capturedStrongholds")}
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
    onChange: (value: number) => void;
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
    let values = initializeEmpty ? [UNINITIALIZED] : [];
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
                if (value === UNINITIALIZED) {
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

function detectMissingInput(value: unknown): FieldError {
    return value !== null &&
        value !== undefined &&
        value !== "" &&
        (typeof value === "number" ? value >= 0 : true)
        ? null
        : ErrorMessage.Required;
}

function objectKeys<T extends object>(obj: T): Array<keyof T> {
    return Object.keys(obj) as Array<keyof T>;
}

export default GameReportForm;
