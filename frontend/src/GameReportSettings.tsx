import React from "react";
import Box from "@mui/joy/Box";
import Typography from "@mui/joy/Typography";
import { settlementLayouts } from "./constants";
import SingleOptionInput from "./SingleOptionInput";
import { SettlementLayout } from "./types";

export interface Settings {
    settlementLayout: SettlementLayout;
    areSettlementsAbbreviated: boolean;
}

export default function GameReportSettings(props: {
    settings: Settings;
    setSettings: React.Dispatch<React.SetStateAction<Settings>>;
}) {
    const { settings, setSettings } = props;

    return (
        <Box p="10px">
            <Typography level="h3" mb="10px">
                Settings
            </Typography>

            <Box border="1px solid #ccc" borderRadius={"5px"} p="10px">
                <Typography level="h4" mb="10px">
                    SP Settlement Captures
                </Typography>

                <Typography level="body-lg" mb="10px">
                    Layout
                </Typography>

                <SingleOptionInput
                    values={settlementLayouts.slice()}
                    orientation="vertical"
                    current={settings.settlementLayout}
                    validate={() => {}}
                    onChange={(v) =>
                        setSettings((prev) => ({
                            ...prev,
                            settlementLayout: v,
                        }))
                    }
                />

                <Typography level="body-lg" my="10px">
                    Names
                </Typography>

                <SingleOptionInput
                    values={[false, true]}
                    getLabel={(v) => (v ? "Abbreviated" : "Full")}
                    orientation="vertical"
                    current={settings.areSettlementsAbbreviated}
                    validate={() => {}}
                    onChange={(v) =>
                        setSettings((prev) => ({
                            ...prev,
                            areSettlementsAbbreviated: v,
                        }))
                    }
                />
            </Box>
        </Box>
    );
}
