import Box from "@mui/joy/Box";
import Sheet from "@mui/joy/Sheet";
import { styled } from "@mui/joy/styles";

const BaseContainer = styled(Sheet)(({ theme }) => ({
    display: "flex",
    flexDirection: "column",
    padding: "24px 64px",
    [theme.breakpoints.down("md")]: {
        padding: "24px 16px",
    },
}));

export const CreateFormContainer = styled(BaseContainer)(({ theme }) => ({
    borderRadius: theme.radius.lg,
    boxShadow: theme.shadow.lg,
    gap: "16px",
    margin: "32px 80px",
    background: "lavender",
    [theme.breakpoints.down("md")]: {
        margin: "32px",
    },
}));

export const EditFormContainer = styled(BaseContainer)(({ theme }) => ({
    fontSize: theme.fontSize.sm,
    gap: "24px",
}));

export const FileInputContainer = styled(Box)(({ theme }) => ({
    display: "flex",
    alignItems: "center",
    gap: "8px",
    [theme.breakpoints.down("sm")]: {
        flexDirection: "column",
    },
}));
