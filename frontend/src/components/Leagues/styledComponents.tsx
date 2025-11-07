import Box from "@mui/joy/Box";
import { styled } from "@mui/joy/styles";

export const LeagueSelector = styled(Box)(({ theme }) => ({
    display: "flex",
    alignItems: "center",
    justifyContent: "center",
    gap: "5px",
    overflow: "auto",
    [theme.breakpoints.down("md")]: {
        flexDirection: "column",
    },
}));

export const SubLeagueSelector = styled(Box)(({ theme }) => ({
    fontSize: theme.fontSize.sm,
    display: "flex",
    alignItems: "center",
    gap: "5px",
    [theme.breakpoints.down("md")]: {
        flexDirection: "column",
    },
}));
