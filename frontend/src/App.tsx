import "@fontsource/inter";
import React, { useState } from "react";
import { CssVarsProvider, List } from "@mui/joy";
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

const sides = ["Free", "Shadow"];
const victoryTypes = ["Ring", "Military", "Concession"];
const expansions = ["LoME", "WoME", "KoME"];
const miniExpansions = ["Cities", "Fate of Erebor", "Treebeard"];
const gameType = ["Ladder", "League", "Tournament", "Friendly"];
const strongholds = [
    "Rivendell",
    "Grey Havens",
    "Helm's Deep",
    "Lorien",
    "Woodland Realm",
    "Erebor",
    "Minas Tirith",
    "Dol Amroth",
];
const cities = [
    "Shire",
    "Edoras",
    "Dale",
    "Pelargir",
    "Ered Luin (Cities expansion only)",
    "Iron Hills (Fate of Erebor expansion only)",
];

interface FormData {
    winner: string;
    loser: string;
    side: string;
    victoryType: string;
    expansionsUsed: string;
    expansions: string[];
    handicapUsed: string;
    actionTokens: string;
    dwarvenRings: string;
    gameType: string;
    gameTurns: string;
    corruption: string;
    fellowshipReachMordor: string;
    mordorTrack: string;
    initialEyes: string;
    aragornCrowned: string;
    aragornCrownedTurn: string;
    treebeardMustered: string;
    capturedStrongholds: string[];
}

const initialFormData: FormData = {
    winner: "",
    loser: "",
    side: "",
    victoryType: "",
    expansionsUsed: "false",
    expansions: [],
    handicapUsed: "false",
    actionTokens: "",
    dwarvenRings: "",
    gameType: "",
    gameTurns: "",
    corruption: "",
    fellowshipReachMordor: "",
    mordorTrack: "",
    initialEyes: "",
    aragornCrowned: "",
    aragornCrownedTurn: "",
    treebeardMustered: "",
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
            <TextInput
                label={"Who won?"}
                placeholder="Player Name - Please check spelling!"
                onChange={handleInputChange("winner")}
            />
            <TextInput
                label={"Who lost?"}
                placeholder="Player Name - Please check spelling!"
                onChange={handleInputChange("loser")}
            />
            <SingleOptionInput
                label={"What side did you play?"}
                options={sides}
                onChange={handleInputChange("side")}
            />
            <SingleOptionInput
                label={"How did you win?"}
                options={victoryTypes}
                onChange={handleInputChange("victoryType")}
            />
            <SingleOptionInput
                label={"Did you use any expansions?"}
                options={["Yes", "No"]}
                onChange={handleInputChange("expansionsUsed")}
            />
            <MultiOptionInput
                label={"What (if any) expansions did you use?"}
                options={expansions.concat(miniExpansions)}
                value={formData["expansions"]}
                onChange={handleInputChange("expansions")}
                show={formData["expansionsUsed"] === "Yes"}
            />
            <SingleOptionInput
                label={"Was Treebeard mustered?"}
                options={["Yes", "No"]}
                onChange={handleInputChange("treebeardMustered")}
                show={formData["expansions"].includes("Treebeard")}
            />
            <SingleOptionInput
                label={"Did you use a handicap mechanism (e.g. Action Tokens)?"}
                options={["Yes", "No"]}
                onChange={handleInputChange("handicapUsed")}
            />
            <SelectNumericOptionInput
                label="Did you use any Action Tokens?"
                start={0}
                end={6}
                additional={["Unlimited!"]}
                onChange={handleInputChange("actionTokens")}
                show={formData["handicapUsed"] === "Yes"}
            />
            <SelectNumericOptionInput
                label="Did you use any Dwarven Rings?"
                start={0}
                end={6}
                additional={["Unlimited!"]}
                onChange={handleInputChange("dwarvenRings")}
                show={formData["handicapUsed"] === "Yes"}
            />
            <SingleOptionInput
                label={"What type of game did you play?"}
                options={gameType}
                onChange={handleInputChange("gameType")}
            />
            <SelectNumericOptionInput
                label="On what turn did the game end?"
                start={1}
                end={25}
                onChange={handleInputChange("gameTurns")}
            />
            <SelectNumericOptionInput
                label="How much corruption did the Fellowship have?"
                start={0}
                end={18}
                onChange={handleInputChange("corruption")}
            />
            <SingleOptionInput
                label={"Did the Fellowship reach Mordor?"}
                options={["Yes", "No"]}
                onChange={handleInputChange("fellowshipReachMordor")}
            />
            <SelectNumericOptionInput
                label="Where did the Fellowship reach on the Mordor track?"
                start={0}
                end={5}
                onChange={handleInputChange("mordorTrack")}
                show={formData["fellowshipReachMordor"] === "Yes"}
            />
            <SelectNumericOptionInput
                label="How many eyes did Shadow allocate on turn 1? (Before action dice were rolled)"
                start={0}
                end={7}
                onChange={handleInputChange("initialEyes")}
            />
            <SingleOptionInput
                label={"Was Aragorn crowned King?"}
                options={["Yes", "No"]}
                onChange={handleInputChange("aragornCrowned")}
            />
            <SelectNumericOptionInput
                label="On what turn was Aragorn crowned King?"
                start={1}
                end={18}
                onChange={handleInputChange("aragornCrownedTurn")}
                show={formData["aragornCrowned"] === "Yes"}
            />
            <MultiOptionInput
                label={"What strongholds were captured by Shadow?"}
                options={strongholds.concat(cities)}
                value={formData["capturedStrongholds"]}
                onChange={handleInputChange("capturedStrongholds")}
            />
            <Button onClick={(e) => console.log(formData)}>Print Form</Button>
        </Sheet>
    );
}

function GameReportFormElement({ children }: { children: React.ReactNode }) {
    return (
        <Sheet variant="outlined" sx={{ p: 2, borderRadius: "lg" }}>
            {children}
        </Sheet>
    );
}

interface TextInputProps {
    label: string;
    placeholder: string;
    onChange: (value: string) => void;
}

function TextInput({ label, placeholder, onChange }: TextInputProps) {
    return (
        <GameReportFormElement>
            <FormLabel>{label}</FormLabel>
            <Input
                placeholder={placeholder}
                onChange={(event) => onChange(event.target.value)}
            />
        </GameReportFormElement>
    );
}

interface SelectOptionInputProps {
    label: string;
    options: string[];
    onChange: (value: string) => void;
    show?: boolean;
}

function SelectOptionInput({
    label,
    options,
    onChange,
    show = true,
}: SelectOptionInputProps) {
    return (
        show && (
            <GameReportFormElement>
                <FormLabel>{label}</FormLabel>
                <Select
                    defaultValue={options[0]}
                    onChange={(_, value) => onChange(value || "")}
                >
                    {options.map((option) => (
                        <Option key={option} value={option}>
                            {option}
                        </Option>
                    ))}
                </Select>
            </GameReportFormElement>
        )
    );
}

interface SelectNumericOptionInputProps {
    label: string;
    start: number;
    end: number;
    additional?: string[];
    onChange: (value: string) => void;
    show?: boolean;
}

function SelectNumericOptionInput({
    label,
    start,
    end,
    additional = [],
    onChange,
    show = true,
}: SelectNumericOptionInputProps) {
    let options = [];
    for (let i = start; i <= end; i++) {
        options.push(i.toString());
    }
    return SelectOptionInput({
        label: label,
        options: options.concat(additional),
        onChange: onChange,
        show: show,
    });
}

interface SingleOptionInputProps {
    label: string;
    options: string[];
    onChange: (value: string) => void;
    show?: boolean;
}

function SingleOptionInput({
    label,
    options,
    onChange,
    show = true,
}: SingleOptionInputProps) {
    return (
        show && (
            <GameReportFormElement>
                <FormLabel>{label}</FormLabel>
                <RadioGroup
                    orientation="horizontal"
                    onChange={(event) => onChange(event.target.value)}
                >
                    {options.map((option) => (
                        <Radio key={option} value={option} label={option} />
                    ))}
                </RadioGroup>
            </GameReportFormElement>
        )
    );
}

interface MultiOptionInputProps {
    label: string;
    options: string[];
    value: string[];
    onChange: (value: string[]) => void;
    show?: boolean;
}

function MultiOptionInput({
    label,
    options,
    value,
    onChange,
    show = true,
}: MultiOptionInputProps) {
    return (
        show && (
            <GameReportFormElement>
                <FormLabel sx={{ py: 1 }}>{label}</FormLabel>
                <List
                    orientation="horizontal"
                    wrap
                    sx={{
                        "--List-gap": "10px",
                        "--ListItem-radius": "30px",
                        "--ListItem-minHeight": "32px",
                    }}
                >
                    {options.map((option) => (
                        <ListItem key={option}>
                            <Checkbox
                                size="sm"
                                disableIcon
                                overlay
                                label={option}
                                onChange={(event) =>
                                    event.target.checked
                                        ? onChange(value.concat(option))
                                        : onChange(
                                              value.filter(
                                                  (item) => item !== option
                                              )
                                          )
                                }
                            />
                        </ListItem>
                    ))}
                </List>
            </GameReportFormElement>
        )
    );
}
