import React, { CSSProperties, ReactNode } from "react";
import Box from "@mui/joy/Box";
import FormControl from "@mui/joy/FormControl";
import FormHelperText from "@mui/joy/FormHelperText";
import FormLabel from "@mui/joy/FormLabel";
import Sheet from "@mui/joy/Sheet";
import { useTheme } from "@mui/joy/styles";
import { FieldError } from "../types";
import HelpIcon from "./HelpIcon";

type LayoutTheme = "minimal" | "default";

interface Props {
    children: React.ReactNode;
    label: string;
    error?: FieldError;
    helpProps?: {
        content: ReactNode;
        iconStyle?: CSSProperties;
    };
    hasSingleControl?: boolean;
    layoutTheme?: LayoutTheme;
}

export default function FormElement({
    children,
    label,
    error,
    helpProps,
    hasSingleControl = true,
    layoutTheme = "default",
}: Props) {
    const theme = useTheme();

    const errorTextStyle = {
        color: theme.palette.danger.plainColor,
        marginTop: theme.spacing(1),
    };

    const formComponents = (
        <>
            <FormLabel
                sx={
                    layoutTheme === "minimal"
                        ? { pb: 1 }
                        : { fontSize: 16, pb: 2 }
                }
            >
                {label}
                {helpProps && <HelpIcon {...helpProps} />}
            </FormLabel>
            {children}
            {error && (
                <FormHelperText sx={errorTextStyle}>{error}</FormHelperText>
            )}
        </>
    );

    return (
        <Container layoutTheme={layoutTheme}>
            {hasSingleControl ? (
                <FormControl error={!!error}>{formComponents}</FormControl>
            ) : (
                <Box>{formComponents}</Box>
            )}
        </Container>
    );
}

interface ContainerProps {
    children: ReactNode;
    layoutTheme?: LayoutTheme;
}

function Container({ children, layoutTheme = "default" }: ContainerProps) {
    return layoutTheme === "minimal" ? (
        <Box>{children}</Box>
    ) : (
        <Sheet variant="outlined" sx={{ p: 2, borderRadius: "lg" }}>
            {children}
        </Sheet>
    );
}
