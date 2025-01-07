import axios from "axios";
import React from "react";
import Box from "@mui/joy/Box";
import Button from "@mui/joy/Button";
import CircularProgress from "@mui/joy/CircularProgress";
import Sheet from "@mui/joy/Sheet";
import Typography from "@mui/joy/Typography";
import useConditionalActionEffect from "./hooks/useConditionalActionEffect";
import useFormData from "./hooks/useFormData";
import {
    ProcessedGameReport,
    ReportDeleteFormData,
    ValidReportDeleteFormData,
} from "./types";
import { displayTime, toErrorMessage } from "./utils";

interface Props {
    report: ProcessedGameReport;
    refresh: () => void;
}

export default function ReportDeleteForm({
    report: { rid, winner, loser, timestamp },
    refresh,
}: Props) {
    const [
        _,
        { errorOnSubmit, successMessage, loading: submitting },
        { handleSubmit },
    ] = useFormData<ReportDeleteFormData, ValidReportDeleteFormData>({
        initialFormData: {
            rid: { value: rid, error: null, validate: () => null },
        },
        optionalFields: [],
        successMessageText: "Report deleted",
        submit,
        toErrorMessage,
    });

    useConditionalActionEffect(!!successMessage, refresh);

    return successMessage ? (
        <Typography color="success">{successMessage}</Typography>
    ) : (
        <Sheet
            sx={{
                display: "flex",
                flexDirection: "column",
                alignItems: "center",
                justifyContent: "center",
                p: 2,
                gap: 2,
            }}
        >
            <Box>
                Delete {displayTime(timestamp)} game between {winner} and{" "}
                {loser}?
            </Box>
            <Button
                onClick={handleSubmit}
                disabled={submitting}
                startDecorator={submitting ? <CircularProgress /> : undefined}
            >
                {submitting ? "Submitting..." : "Delete"}
            </Button>
            {errorOnSubmit && (
                <Typography color="danger" mt={1}>
                    {errorOnSubmit}
                </Typography>
            )}
        </Sheet>
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
