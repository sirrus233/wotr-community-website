import { styled } from "@mui/joy/styles";

export const BelowSmall = styled("div")(({ theme }) => ({
    [theme.breakpoints.up("sm")]: {
        display: "none",
    },
}));

export const AboveSmall = styled("div")(({ theme }) => ({
    [theme.breakpoints.down("sm")]: {
        display: "none",
    },
}));

export const BelowLarge = styled("div")(({ theme }) => ({
    [theme.breakpoints.up("lg")]: {
        display: "none",
    },
}));

export const AboveMedium = styled("div")(({ theme }) => ({
    [theme.breakpoints.down("md")]: {
        display: "none",
    },
}));

export const AboveLarge = styled("div")(({ theme }) => ({
    [theme.breakpoints.down("lg")]: {
        display: "none",
    },
}));
