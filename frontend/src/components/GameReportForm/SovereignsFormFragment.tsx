import React, { useEffect, useMemo, useState } from "react";
import Button from "@mui/joy/Button";
import {
    defaultSovereignStates,
    SOVEREIGN_COLLECTION_START_DATE_MS,
    sovereigns,
    sovereignStatuses,
} from "../../constants";
import colors from "../../styles/colors";
import { Column, FlexBox } from "../../styles/styledComponents";
import { Sovereign, Sovereigns } from "../../types";
import { displayTime, toTitleCase } from "../../utils";
import BooleanInput from "../BooleanInput";
import FormElement from "../FormElement";
import SingleOptionInput from "../SingleOptionInput";

interface Props {
    current: Sovereigns | null;
    reportTimestamp: string | null;
    isNewReport: boolean;
    onChange: (value: Sovereigns | null) => void;
    validate: () => void;
}

export default function SovereignsFormFragment({
    current,
    reportTimestamp,
    isNewReport,
    onChange,
    validate,
}: Props) {
    const currentMasked = current || defaultSovereignStates;

    const collectionStartIso = new Date(
        SOVEREIGN_COLLECTION_START_DATE_MS,
    ).toISOString();

    const [isElected, setIsElected] = useState(!!current);

    const isRequired = useMemo(
        () =>
            isNewReport ||
            new Date(reportTimestamp || Date.now()).getTime() >=
                SOVEREIGN_COLLECTION_START_DATE_MS,
        [reportTimestamp, isNewReport],
    );

    useEffect(
        function initializeFormData() {
            onChange(isRequired || isElected ? currentMasked : null);
        },
        [isRequired, isElected],
    );

    return isRequired || isElected ? (
        <FlexBox sx={{ flexWrap: "wrap", gap: "10px" }}>
            {sovereigns.map((sovereign) => {
                const sovereignLabel = toTitleCase(sovereign);

                return (
                    <FormElement
                        key={sovereign}
                        label={sovereignLabel}
                        hasSingleControl={false}
                        layoutTheme="card"
                        sx={{ label: { fontWeight: "bold" } }}
                        containerSx={{
                            boxShadow: `${toSovereignColor(sovereign)} 1px 1px 2px 1px`,
                            bgcolor: "transparent",
                        }}
                    >
                        <Column gap={2} sx={{ label: { fontWeight: "unset" } }}>
                            <FormElement
                                label={`${sovereignLabel} Status`}
                                labelHidden
                                layoutTheme="minimal"
                            >
                                <SingleOptionInput
                                    orientation="vertical"
                                    values={sovereignStatuses.slice()}
                                    current={currentMasked[sovereign].status}
                                    validate={validate}
                                    onChange={(status) =>
                                        onChange({
                                            ...currentMasked,
                                            [sovereign]: {
                                                ...currentMasked[sovereign],
                                                status,
                                            },
                                        })
                                    }
                                />
                            </FormElement>
                            <FormElement
                                label={`${sovereignLabel} Died`}
                                labelHidden
                                layoutTheme="minimal"
                            >
                                <BooleanInput
                                    label="Died"
                                    labelReversed
                                    current={currentMasked[sovereign].died}
                                    validate={validate}
                                    onChange={(died) =>
                                        onChange({
                                            ...currentMasked,
                                            [sovereign]: {
                                                ...currentMasked[sovereign],
                                                died,
                                            },
                                        })
                                    }
                                />
                            </FormElement>
                        </Column>
                    </FormElement>
                );
            })}
        </FlexBox>
    ) : (
        <FlexBox sx={{ gap: 2, alignItems: "center", flexWrap: "wrap" }}>
            <em>
                {`N/A. Reporting not required for games prior to ${displayTime(
                    collectionStartIso,
                    { tz: "UTC", withTzDisplayed: true },
                )} (${displayTime(collectionStartIso, { withTzDisplayed: true })})`}
            </em>

            <Button onClick={() => setIsElected(true)}>Report anyway?</Button>
        </FlexBox>
    );
}

function toSovereignColor(sovereign: Sovereign): string {
    switch (sovereign) {
        case "thranduil":
            return colors.elves;
        case "brand":
            return colors.north;
        case "dain":
            return colors.dwarves;
        case "denethor":
            return colors.gondor;
        case "theoden":
            return colors.rohan;
    }
}
