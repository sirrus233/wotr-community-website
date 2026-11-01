import React, { useEffect } from "react";
import Box from "@mui/joy/Box";
import Typography from "@mui/joy/Typography";
import { settlementLayouts } from "../../constants";
import { Column } from "../../styles/styledComponents";
import { SettlementLayout } from "../../types";
import { isDefined } from "../../utils";
import SingleOptionInput from "../SingleOptionInput";

const CACHED_SETTINGS_KEY = "gameReportSettings";

const settingConfigs: SettingConfig[] = [
    { key: "settlementLayout", opts: settlementLayouts.slice() },
    { key: "areSettlementsAbbreviated", opts: [true, false] },
    { key: "hasSovereignStatusLabel", opts: [true, false] },
    { key: "areSovereignsAbbreviated", opts: [true, false] },
];

export const defaultSettings: GameReportSettings = {
    settlementLayout: "Standard",
    areSettlementsAbbreviated: false,
    hasSovereignStatusLabel: false,
    areSovereignsAbbreviated: false,
};

export interface GameReportSettings {
    settlementLayout: SettlementLayout;
    areSettlementsAbbreviated: boolean;
    hasSovereignStatusLabel: boolean;
    areSovereignsAbbreviated: boolean;
}

type SettingConfig = {
    [K in keyof GameReportSettings]: {
        key: K;
        opts: GameReportSettings[K][];
    };
}[keyof GameReportSettings];

interface Props {
    settings: GameReportSettings;
    setSettings: React.Dispatch<React.SetStateAction<GameReportSettings>>;
}

export default function Settings({ settings, setSettings }: Props) {
    useEffect(function applyCachedSettings() {
        setSettings(
            parseCachedSettings(localStorage.getItem(CACHED_SETTINGS_KEY)),
        );
    }, []);

    useEffect(
        function cacheSettings() {
            localStorage.setItem(CACHED_SETTINGS_KEY, JSON.stringify(settings));
        },
        [settings],
    );

    const inputProps = { orientation: "vertical", validate: () => {} } as const;

    return (
        <Column p="10px" gap="5px">
            <Typography level="h3" mb="10px">
                Settings
            </Typography>

            {[
                {
                    sectionLabel: "SP Settlement Captures",
                    fields: [
                        {
                            label: "Layout",
                            input: (
                                <SingleOptionInput
                                    {...inputProps}
                                    values={settlementLayouts.slice()}
                                    current={settings.settlementLayout}
                                    onChange={(v) =>
                                        setSettings((prev) => ({
                                            ...prev,
                                            settlementLayout: v,
                                        }))
                                    }
                                />
                            ),
                        },
                        {
                            label: "Names",
                            input: (
                                <SingleOptionInput
                                    {...inputProps}
                                    values={[false, true]}
                                    getLabel={(v) =>
                                        v ? "Abbreviated" : "Full"
                                    }
                                    current={settings.areSettlementsAbbreviated}
                                    onChange={(v) =>
                                        setSettings((prev) => ({
                                            ...prev,
                                            areSettlementsAbbreviated: v,
                                        }))
                                    }
                                />
                            ),
                        },
                    ],
                },
                {
                    sectionLabel: "Sovereigns",
                    fields: [
                        {
                            label: "Status label",
                            input: (
                                <SingleOptionInput
                                    {...inputProps}
                                    values={[true, false]}
                                    getLabel={(v) => (v ? "Yes" : "No")}
                                    current={settings.hasSovereignStatusLabel}
                                    onChange={(v) =>
                                        setSettings((prev) => ({
                                            ...prev,
                                            hasSovereignStatusLabel: v,
                                        }))
                                    }
                                />
                            ),
                        },
                        {
                            label: "Names",
                            input: (
                                <SingleOptionInput
                                    {...inputProps}
                                    values={[false, true]}
                                    getLabel={(v) =>
                                        v ? "Abbreviated" : "Full"
                                    }
                                    current={settings.areSovereignsAbbreviated}
                                    onChange={(v) =>
                                        setSettings((prev) => ({
                                            ...prev,
                                            areSovereignsAbbreviated: v,
                                        }))
                                    }
                                />
                            ),
                        },
                    ],
                },
            ].map(({ sectionLabel, fields }) => (
                <Box
                    key={sectionLabel}
                    border="1px solid #ccc"
                    borderRadius={"5px"}
                    p="10px"
                >
                    <Typography level="h4" mb="10px">
                        {sectionLabel}
                    </Typography>

                    {fields.map(({ label, input }) => (
                        <React.Fragment key={label}>
                            <Typography level="body-lg" my="10px">
                                {label}
                            </Typography>

                            {input}
                        </React.Fragment>
                    ))}
                </Box>
            ))}
        </Column>
    );
}

function parseCachedSettings(
    cachedSettings: string | null,
): GameReportSettings {
    try {
        if (cachedSettings) {
            const parsedSettings: Partial<GameReportSettings> | null =
                JSON.parse(cachedSettings);

            if (isDefined(parsedSettings)) {
                return settingConfigs.reduce<GameReportSettings>(
                    (accum, { key, opts }) => ({
                        ...accum,
                        [key]: normalizeSetting(key, opts, parsedSettings),
                    }),
                    defaultSettings,
                );
            }
        }
        return defaultSettings;
    } catch {
        return defaultSettings;
    }
}

function normalizeSetting<T extends keyof GameReportSettings>(
    key: T,
    opts: GameReportSettings[T][],
    parsedSettings: Partial<GameReportSettings>,
): GameReportSettings[T] {
    return opts.find((a) => a === parsedSettings[key]) ?? defaultSettings[key];
}
