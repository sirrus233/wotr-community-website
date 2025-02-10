import React, { ReactNode } from "react";
import Box from "@mui/joy/Box";
import Button from "@mui/joy/Button";
import CircularProgress from "@mui/joy/CircularProgress";
import Sheet from "@mui/joy/Sheet";
import Typography from "@mui/joy/Typography";
import { FieldError } from "./types";
import ErrorDisplay from "./ErrorDisplay";
import FormElement from "./FormElement";

interface FormElementProps {
    element: ReactNode;
    label: string;
    error: FieldError;
    hasSingleControl?: boolean;
}

interface Props {
    header: ReactNode;
    formElementsProps: FormElementProps[];
    handleSubmit: () => void;
    submitting: boolean;
    errorOnSubmit: FieldError;
    buttonText?: string;
    successMessage: string | null;
    shouldHideFormOnSuccess?: boolean;
    children?: ReactNode;
}

export default function AdminFormLayout({
    header,
    formElementsProps,
    handleSubmit,
    submitting,
    errorOnSubmit,
    buttonText = "Submit",
    successMessage,
    shouldHideFormOnSuccess = false,
    children,
}: Props) {
    return (
        <Sheet
            sx={{
                display: "flex",
                flexDirection: "column",
                alignItems: "center",
                justifyContent: "center",
            }}
        >
            {shouldHideFormOnSuccess && successMessage ? (
                <Typography color="success">{successMessage}</Typography>
            ) : (
                <>
                    <Typography level="title-lg">{header}</Typography>

                    <Box
                        display="flex"
                        flexDirection="column"
                        gap={2}
                        sx={{ my: 2, width: "100%" }}
                    >
                        {children}
                        {formElementsProps.map(
                            ({ element, label, ...props }) => (
                                <FormElement
                                    key={label}
                                    layoutTheme="minimal"
                                    label={label}
                                    {...props}
                                >
                                    {element}
                                </FormElement>
                            )
                        )}
                    </Box>

                    <Button
                        onClick={handleSubmit}
                        disabled={submitting}
                        startDecorator={
                            submitting ? <CircularProgress /> : undefined
                        }
                    >
                        {submitting ? "Submitting..." : buttonText}
                    </Button>

                    {errorOnSubmit && (
                        <ErrorDisplay message={errorOnSubmit} sx={{ mt: 1 }} />
                    )}
                </>
            )}
        </Sheet>
    );
}
