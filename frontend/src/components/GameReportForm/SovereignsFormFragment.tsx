import React, { useEffect, useMemo } from "react";
import {
    defaultSovereignStates,
    SOVEREIGN_COLLECTION_START_DATE_MS,
    sovereigns,
    sovereignStatuses,
} from "../../constants";
import { Column, FlexBox } from "../../styles/styledComponents";
import { Sovereigns } from "../../types";
import { displayTime, toTitleCase } from "../../utils";
import BooleanInput from "../BooleanInput";
import FormElement from "../FormElement";
import SingleOptionInput from "../SingleOptionInput";

interface Props {
    current: Sovereigns | null;
    reportTimestamp: string | null;
    onChange: (value: Sovereigns) => void;
    validate: () => void;
}

export default function SovereignsFormFragment({
    current: _current,
    reportTimestamp,
    onChange,
    validate,
}: Props) {
    const current = _current || defaultSovereignStates;

    const collectionStartIso = new Date(
        SOVEREIGN_COLLECTION_START_DATE_MS,
    ).toISOString();

    const hasCollectionStarted = useMemo(
        () =>
            !!(
                new Date(reportTimestamp || Date.now()).getTime() >=
                SOVEREIGN_COLLECTION_START_DATE_MS
            ),
        [reportTimestamp],
    );

    useEffect(
        function initializeFormData() {
            if (hasCollectionStarted) onChange(current);
        },
        [hasCollectionStarted],
    );

    return hasCollectionStarted ? (
        <FlexBox sx={{ flexWrap: "wrap", gap: 1 }}>
            {sovereigns.map((sovereign) => {
                const sovereignLabel = toTitleCase(sovereign);

                return (
                    <FormElement
                        key={sovereign}
                        label={sovereignLabel}
                        hasSingleControl={false}
                        layoutTheme="card"
                        sx={{ label: { fontWeight: "bold" } }}
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
                                    current={current[sovereign].status}
                                    validate={validate}
                                    onChange={(status) =>
                                        onChange({
                                            ...current,
                                            [sovereign]: {
                                                ...current[sovereign],
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
                                    current={current[sovereign].died}
                                    validate={validate}
                                    onChange={(died) =>
                                        onChange({
                                            ...current,
                                            [sovereign]: {
                                                ...current[sovereign],
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
        <em>
            {`N/A. Not reported for games prior to ${displayTime(
                collectionStartIso,
                { tz: "UTC", withTzDisplayed: true },
            )} (${displayTime(collectionStartIso, { withTzDisplayed: true })})`}
        </em>
    );
}
