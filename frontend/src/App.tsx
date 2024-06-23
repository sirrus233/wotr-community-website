import "@fontsource/inter";
import React, { useState, useEffect } from "react";
import { CssVarsProvider, List, Typography } from "@mui/joy";
import { CssBaseline } from "@mui/joy";
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

export default function App() {
    return (
        <CssVarsProvider>
            <CssBaseline />
            <GameReportForm />
        </CssVarsProvider>
    );
}

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

const matchType = ["Ladder", "League", "Tournament", "Friendly"] as const;
type Match = (typeof matchType)[number];

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

interface FormData {
    winner: string | null;
    loser: string | null;
    side: Side | null;
    victoryType: Victory | null;
    usedExpansions: boolean | null;
    expansions: Expansion[];
    wasTreebeardMustered: boolean | null;
    usedHandicap: boolean | null;
    actionTokens: number;
    dwarvenRings: number;
    matchType: Match | null;
    gameTurns: number;
    corruption: number;
    didFellowshipReachMordor: boolean | null;
    mordorTrack: number;
    initialEyes: number;
    wasAragornCrowned: boolean | null;
    aragornCrownedTurn: number;
    capturedStrongholds: Stronghold[];
}

const UNINITIALIZED = -1;
const INFINITE = 100;

const initialFormData: FormData = {
    winner: null,
    loser: null,
    side: null,
    victoryType: null,
    usedExpansions: null,
    expansions: [],
    wasTreebeardMustered: null,
    usedHandicap: null,
    actionTokens: 0,
    dwarvenRings: 0,
    matchType: null,
    gameTurns: UNINITIALIZED,
    corruption: UNINITIALIZED,
    didFellowshipReachMordor: null,
    mordorTrack: UNINITIALIZED,
    initialEyes: UNINITIALIZED,
    wasAragornCrowned: null,
    aragornCrownedTurn: UNINITIALIZED,
    capturedStrongholds: [],
};

type ValueOf<T> = T[keyof T];

function GameReportForm() {
    const [formData, setFormData] = useState(initialFormData);

    const handleInputChange = <K extends keyof FormData>(field: K) => {
        return (value: FormData[K]) =>
            setFormData({ ...formData, [field]: value });
    };

    const controlledClearEffect = <
        K extends keyof FormData,
        V extends ValueOf<FormData>
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

    controlledClearEffect(formData.usedExpansions, "expansions");
    controlledClearEffect(formData.usedExpansions, "wasTreebeardMustered");
    controlledClearEffect(
        formData.expansions,
        "wasTreebeardMustered",
        (expansions) => expansions.includes("Treebeard")
    );
    controlledClearEffect(formData.usedHandicap, "actionTokens");
    controlledClearEffect(formData.usedHandicap, "dwarvenRings");
    controlledClearEffect(formData.didFellowshipReachMordor, "mordorTrack");
    controlledClearEffect(formData.wasAragornCrowned, "aragornCrownedTurn");

    // wasTreebeardMustered: boolean | null;
    // matchType: Match | null;

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
            <GameReportFormElement label={"Who won?"}>
                <TextInput
                    value={formData.winner || ""}
                    placeholder="Player Name - Please check spelling!"
                    onChange={handleInputChange("winner")}
                />
            </GameReportFormElement>
            <GameReportFormElement label={"Who lost?"}>
                <TextInput
                    value={formData.loser || ""}
                    placeholder="Player Name - Please check spelling!"
                    onChange={handleInputChange("loser")}
                />
            </GameReportFormElement>
            <GameReportFormElement label={"What side did you play?"}>
                <SingleOptionInput
                    values={sides.slice()}
                    current={formData.side}
                    onChange={handleInputChange("side")}
                />
            </GameReportFormElement>
            <GameReportFormElement label={"How did you win?"}>
                <SingleOptionInput
                    values={victoryTypes.slice()}
                    current={formData.victoryType}
                    onChange={handleInputChange("victoryType")}
                />
            </GameReportFormElement>
            <GameReportFormElement label={"Did you use any expansions?"}>
                <SingleOptionInput
                    values={[true, false]}
                    current={formData.usedExpansions}
                    getLabel={(v) => (v ? "Yes" : "No")}
                    onChange={handleInputChange("usedExpansions")}
                />
            </GameReportFormElement>
            {formData.usedExpansions && (
                <GameReportFormElement label={"What expansions did you use?"}>
                    <MultiOptionInput
                        values={expansions.slice()}
                        current={formData.expansions}
                        onChange={handleInputChange("expansions")}
                    />
                </GameReportFormElement>
            )}
            {formData.usedExpansions &&
                formData.expansions.includes("Treebeard") && (
                    <GameReportFormElement label={"Was Treebeard mustered?"}>
                        <SingleOptionInput
                            values={[true, false]}
                            current={formData.wasTreebeardMustered}
                            getLabel={(v) => (v ? "Yes" : "No")}
                            onChange={handleInputChange("wasTreebeardMustered")}
                        />
                    </GameReportFormElement>
                )}
            <GameReportFormElement
                label={"Did you use a handicap mechanism (e.g. Action Tokens)?"}
            >
                <SingleOptionInput
                    values={[true, false]}
                    current={formData.usedHandicap}
                    getLabel={(v) => (v ? "Yes" : "No")}
                    onChange={handleInputChange("usedHandicap")}
                />
            </GameReportFormElement>
            {formData.usedHandicap && (
                <>
                    <GameReportFormElement label="How many Action Tokens?">
                        <SelectNumericOptionInput
                            start={0}
                            end={8}
                            initializeEmpty={false}
                            allowInfinite={true}
                            onChange={handleInputChange("actionTokens")}
                        />
                    </GameReportFormElement>
                    <GameReportFormElement label="How many Dwarven Rings?">
                        <SelectNumericOptionInput
                            start={0}
                            end={8}
                            initializeEmpty={false}
                            allowInfinite={true}
                            onChange={handleInputChange("dwarvenRings")}
                        />
                    </GameReportFormElement>
                </>
            )}
            <GameReportFormElement label={"What type of game did you play?"}>
                <SingleOptionInput
                    values={matchType.slice()}
                    current={formData.matchType}
                    onChange={handleInputChange("matchType")}
                />
            </GameReportFormElement>
            <GameReportFormElement label="On what turn did the game end?">
                <SelectNumericOptionInput
                    start={1}
                    end={25}
                    onChange={handleInputChange("gameTurns")}
                />
            </GameReportFormElement>
            <GameReportFormElement label="How much corruption did the Fellowship have?">
                <SelectNumericOptionInput
                    start={0}
                    end={18}
                    onChange={handleInputChange("corruption")}
                />
            </GameReportFormElement>
            <GameReportFormElement label={"Did the Fellowship reach Mordor?"}>
                <SingleOptionInput
                    values={[true, false]}
                    current={formData.didFellowshipReachMordor}
                    getLabel={(v) => (v ? "Yes" : "No")}
                    onChange={handleInputChange("didFellowshipReachMordor")}
                />
            </GameReportFormElement>
            {formData.didFellowshipReachMordor && (
                <GameReportFormElement label="Where did the Fellowship reach on the Mordor track?">
                    <SelectNumericOptionInput
                        start={0}
                        end={5}
                        onChange={handleInputChange("mordorTrack")}
                    />
                </GameReportFormElement>
            )}
            <GameReportFormElement label="How many eyes did Shadow allocate on turn 1? (Before action dice were rolled)">
                <SelectNumericOptionInput
                    start={0}
                    end={7}
                    onChange={handleInputChange("initialEyes")}
                />
            </GameReportFormElement>
            <GameReportFormElement label={"Was Aragorn crowned King?"}>
                <SingleOptionInput
                    values={[true, false]}
                    current={formData.wasAragornCrowned}
                    getLabel={(v) => (v ? "Yes" : "No")}
                    onChange={handleInputChange("wasAragornCrowned")}
                />
            </GameReportFormElement>
            {formData.wasAragornCrowned && (
                <GameReportFormElement label="On what turn was Aragorn crowned King?">
                    <SelectNumericOptionInput
                        start={1}
                        end={18}
                        onChange={handleInputChange("aragornCrownedTurn")}
                    />
                </GameReportFormElement>
            )}
            <GameReportFormElement
                label={"What strongholds were captured by Shadow?"}
            >
                <VictoryPoints
                    strongholds={formData.capturedStrongholds}
                ></VictoryPoints>
                <MultiOptionInput
                    values={strongholds.slice()}
                    current={formData.capturedStrongholds}
                    onChange={handleInputChange("capturedStrongholds")}
                />
            </GameReportFormElement>
            <Button onClick={(e) => alert(JSON.stringify(formData, null, 4))}>
                Print Form
            </Button>
        </Sheet>
    );
}

interface GameReportElementProps {
    children: React.ReactNode;
    label: string;
}

function GameReportFormElement({ children, label }: GameReportElementProps) {
    return (
        <Sheet variant="outlined" sx={{ p: 2, borderRadius: "lg" }}>
            <FormLabel sx={{ fontSize: 16, pb: 2 }}>{label}</FormLabel>
            {children}
        </Sheet>
    );
}

interface TextInputProps {
    value: string;
    placeholder: string;
    onChange: (value: string) => void;
}

function TextInput({ value, placeholder, onChange }: TextInputProps) {
    return (
        <Input
            value={value}
            placeholder={placeholder}
            onChange={(event) => onChange(event.target.value)}
        />
    );
}

interface SelectOptionInputProps<T> {
    values: T[];
    current: T;
    getLabel?: (value: T) => string;
    onChange: (value: T) => void;
}

function SelectOptionInput<T>({
    values,
    current,
    getLabel = String,
    onChange,
}: SelectOptionInputProps<T>) {
    return (
        <Select
            defaultValue={values[0]}
            onChange={(_, value) => onChange(value as T)}
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
}

function SelectNumericOptionInput({
    start,
    end,
    initializeEmpty = true,
    allowInfinite = false,
    onChange,
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
        />
    );
}

interface SingleOptionInputProps<T> {
    values: T[];
    current: T;
    getLabel?: (value: T) => string;
    onChange: (value: T) => void;
}

function SingleOptionInput<T>({
    values,
    current,
    getLabel = String,
    onChange,
}: SingleOptionInputProps<T>) {
    const handleChange = (event: React.ChangeEvent<HTMLInputElement>) => {
        const selectedValue = values.find(
            (value) => String(value) === event.target.value
        );
        if (selectedValue !== undefined) {
            onChange(selectedValue);
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
}

function MultiOptionInput<T>({
    values,
    current,
    getLabel = String,
    onChange,
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
