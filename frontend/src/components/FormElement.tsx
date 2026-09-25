import React, { CSSProperties, ReactNode } from "react";
import Box from "@mui/joy/Box";
import Card from "@mui/joy/Card";
import FormControl from "@mui/joy/FormControl";
import FormHelperText from "@mui/joy/FormHelperText";
import FormLabel from "@mui/joy/FormLabel";
import Sheet from "@mui/joy/Sheet";
import { styled, useTheme } from "@mui/joy/styles";
import { SxProps } from "@mui/joy/styles/types";
import { FieldError } from "../types";
import HelpIcon from "./HelpIcon";

type LayoutTheme = "card" | "minimal" | "default";

const LabelArea = styled(Box)({
    display: "flex",
});

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
    labelHidden?: boolean;
    sx?: SxProps;
}

export default function FormElement({
    children,
    label,
    error,
    helpProps,
    hasSingleControl = true,
    layoutTheme = "default",
    labelHidden = false,
    sx,
}: Props) {
    const theme = useTheme();

    const errorTextStyle = {
        color: theme.palette.danger.plainColor,
        marginTop: theme.spacing(1),
    };

    const formComponents = (
        <>
            <LabelArea>
                <FormLabel
                    className={labelHidden ? "visually-hidden" : undefined}
                    sx={styleLabel(layoutTheme)}
                >
                    {label}
                </FormLabel>
                {helpProps && <HelpIcon {...helpProps} />}
            </LabelArea>
            {children}
            {error && (
                <FormHelperText sx={errorTextStyle}>{error}</FormHelperText>
            )}
        </>
    );

    return (
        <Container layoutTheme={layoutTheme}>
            {hasSingleControl ? (
                <FormControl error={!!error} sx={sx}>
                    {formComponents}
                </FormControl>
            ) : (
                <Box sx={sx}>{formComponents}</Box>
            )}
        </Container>
    );
}

interface ContainerProps {
    children: ReactNode;
    layoutTheme?: LayoutTheme;
}

function Container({
    children,
    layoutTheme = "default",
}: ContainerProps): JSX.Element {
    switch (layoutTheme) {
        case "minimal":
            return <Box>{children}</Box>;
        case "card":
            return (
                <Card variant="soft" size="sm">
                    {children}
                </Card>
            );
        case "default":
            return (
                <Sheet variant="outlined" sx={{ p: 2, borderRadius: "lg" }}>
                    {children}
                </Sheet>
            );
    }
}

function styleLabel(layoutTheme: LayoutTheme): SxProps {
    switch (layoutTheme) {
        case "minimal":
        case "card":
            return { fontSize: "inherit", pb: 1 };
        case "default":
            return { fontSize: 16, pb: 2 };
    }
}
