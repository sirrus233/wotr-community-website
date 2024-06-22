import "@fontsource/inter";
import React, { useState } from "react";
import { CssVarsProvider, List, Typography } from "@mui/joy";
import { CssBaseline } from "@mui/joy";
import Sheet from "@mui/joy/Sheet";
import FormControl from "@mui/joy/FormControl";
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
    wasTreebeardMustered: boolean | null;
    capturedStrongholds: Stronghold[];
}

const initialFormData: FormData = {
    winner: null,
    loser: null,
    side: null,
    victoryType: null,
    usedExpansions: null,
    expansions: [],
    usedHandicap: null,
    actionTokens: 0,
    dwarvenRings: 0,
    matchType: null,
    gameTurns: 1,
    corruption: 0,
    didFellowshipReachMordor: null,
    mordorTrack: 0,
    initialEyes: 0,
    wasAragornCrowned: null,
    aragornCrownedTurn: 0,
    wasTreebeardMustered: null,
    capturedStrongholds: [],
};

function GameReportForm() {
    const [formData, setFormData] = useState(initialFormData);

    const handleInputChange = <K extends keyof FormData>(field: K) => {
        return (value: FormData[K]) =>
            setFormData({ ...formData, [field]: value });
    };

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
                    placeholder="Player Name - Please check spelling!"
                    onChange={handleInputChange("winner")}
                />
            </GameReportFormElement>
            <GameReportFormElement label={"Who lost?"}>
                <TextInput
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
            <GameReportFormElement
                label={"What expansions did you use?"}
                show={formData.usedExpansions === true}
            >
                <MultiOptionInput
                    values={expansions.slice()}
                    current={formData.expansions}
                    onChange={handleInputChange("expansions")}
                />
            </GameReportFormElement>
            <GameReportFormElement
                label={"Was Treebeard mustered?"}
                show={formData.expansions.includes("Treebeard")}
            >
                <SingleOptionInput
                    values={[true, false]}
                    current={formData.wasTreebeardMustered}
                    getLabel={(v) => (v ? "Yes" : "No")}
                    onChange={handleInputChange("wasTreebeardMustered")}
                />
            </GameReportFormElement>
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
            <GameReportFormElement
                label="Did you use any Action Tokens?"
                show={formData.usedHandicap === true}
            >
                <SelectNumericOptionInput
                    start={0}
                    end={6}
                    onChange={handleInputChange("actionTokens")}
                />
            </GameReportFormElement>
            <GameReportFormElement
                label="Did you use any Dwarven Rings?"
                show={formData.usedHandicap === true}
            >
                <SelectNumericOptionInput
                    start={0}
                    end={6}
                    onChange={handleInputChange("dwarvenRings")}
                />
            </GameReportFormElement>
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
            <GameReportFormElement
                label="Where did the Fellowship reach on the Mordor track?"
                show={formData.didFellowshipReachMordor === true}
            >
                <SelectNumericOptionInput
                    start={0}
                    end={5}
                    onChange={handleInputChange("mordorTrack")}
                />
            </GameReportFormElement>
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
            <GameReportFormElement
                label="On what turn was Aragorn crowned King?"
                show={formData.wasAragornCrowned === true}
            >
                <SelectNumericOptionInput
                    start={1}
                    end={18}
                    onChange={handleInputChange("aragornCrownedTurn")}
                />
            </GameReportFormElement>
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
    show?: boolean;
}

function GameReportFormElement({
    children,
    label,
    show = true,
}: GameReportElementProps) {
    return (
        show && (
            <Sheet variant="outlined" sx={{ p: 2, borderRadius: "lg" }}>
                <FormLabel sx={{ fontSize: 16, pb: 2 }}>{label}</FormLabel>
                {children}
            </Sheet>
        )
    );
}

interface TextInputProps {
    placeholder: string;
    onChange: (value: string) => void;
}

function TextInput({ placeholder, onChange }: TextInputProps) {
    return (
        <Input
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
            defaultValue={current}
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
    onChange: (value: number) => void;
}

function SelectNumericOptionInput({
    start,
    end,
    onChange,
}: SelectNumericOptionInputProps) {
    let values = [];
    for (let i = start; i <= end; i++) {
        values.push(i);
    }

    return SelectOptionInput({
        values: values,
        current: start,
        onChange: onChange,
    });
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
            defaultValue={current}
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
