import React, { useEffect } from "react";
import Box from "@mui/joy/Box";
import Typography from "@mui/joy/Typography";
import { settlementLayouts } from "../../constants";
import SingleOptionInput from "../SingleOptionInput";
import { SettlementLayout } from "../../types";
import { isDefined } from "../../utils";

const CACHED_SETTINGS_KEY = "gameReportSettings";

export const defaultSettings: GameReportSettings = {
    settlementLayout: "Standard",
    areSettlementsAbbreviated: false,
};

export interface GameReportSettings {
    settlementLayout: SettlementLayout;
    areSettlementsAbbreviated: boolean;
}

interface Props {
    settings: GameReportSettings;
    setSettings: React.Dispatch<React.SetStateAction<GameReportSettings>>;
}

export default function Settings({ settings, setSettings }: Props) {
    useEffect(function applyCachedSettings() {
        setSettings(
            parseCachedSettings(localStorage.getItem(CACHED_SETTINGS_KEY))
        );
    }, []);

    useEffect(
        function cacheSettings() {
            localStorage.setItem(CACHED_SETTINGS_KEY, JSON.stringify(settings));
        },
        [settings]
    );

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

function parseCachedSettings(
    cachedSettings: string | null
): GameReportSettings {
    try {
        if (cachedSettings) {
            const parsedSettings: Partial<GameReportSettings> | null =
                JSON.parse(cachedSettings);

            if (isDefined(parsedSettings)) {
                return {
                    settlementLayout:
                        settlementLayouts.find(
                            (s) => s === parsedSettings.settlementLayout
                        ) ?? defaultSettings.settlementLayout,
                    areSettlementsAbbreviated:
                        [true, false].find(
                            (a) =>
                                a === parsedSettings.areSettlementsAbbreviated
                        ) ?? defaultSettings.areSettlementsAbbreviated,
                };
            }
        }
        return defaultSettings;
    } catch {
        return defaultSettings;
    }
}
