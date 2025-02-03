import axios from "axios";
import React from "react";
import AdminFormLayout from "./AdminFormLayout";
import useConditionalActionEffect from "./hooks/useConditionalActionEffect";
import useFormData, { initializeToDefaults } from "./hooks/useFormData";
import { toErrorMessage } from "./networkErrorHandlers";
import {
    ProcessedGameReport,
    ReportDeleteFormData,
    ValidReportDeleteFormData,
} from "./types";
import { displayTime } from "./utils";

interface Props {
    report: ProcessedGameReport;
    refresh: () => void;
}

export default function ReportDeleteForm({
    report: { rid, winner, loser, timestamp },
    refresh,
}: Props) {
    const [_, { errorOnSubmit, successMessage, submitting }, { handleSubmit }] =
        useFormData<ReportDeleteFormData, ValidReportDeleteFormData>({
            initialFormData: { rid: initializeToDefaults(rid) },
            optionalFields: [],
            successMessageText: "Report deleted",
            submit,
            toErrorMessage,
        });

    useConditionalActionEffect(!!successMessage, refresh);

    return (
        <AdminFormLayout
            header=""
            handleSubmit={handleSubmit}
            submitting={submitting}
            errorOnSubmit={errorOnSubmit}
            buttonText="Delete"
            successMessage={successMessage}
            shouldHideFormOnSuccess
            formElementsProps={[]}
        >
            Delete {displayTime(timestamp)} game between {winner} and {loser}?
        </AdminFormLayout>
    );
}

async function submit({ rid }: ValidReportDeleteFormData) {
    return await axios.post(
        "https://api.waroftheringcommunity.net:8080/deleteReport",
        // "http://localhost:8081/deleteReport",
        { rid: rid.value },
        { headers: { "Content-Type": "application/json" } }
    );
}
