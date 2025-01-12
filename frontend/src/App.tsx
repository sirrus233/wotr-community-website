import axios from "axios";
import { CssVarsProvider } from "@mui/joy/styles";
import CssBaseline from "@mui/joy/CssBaseline";
import React, { ReactNode, useEffect, useState } from "react";
import ExternalLink from "./ExternalLink";
import GameReportForm from "./GameReportForm";
import GameReports from "./GameReports";
import Rankings from "./Rankings";
import Box from "@mui/joy/Box";
import IconButton from "@mui/joy/IconButton";
import Drawer from "@mui/joy/Drawer";
import MuiLink from "@mui/joy/Link";
import List from "@mui/joy/List";
import ListItemButton from "@mui/joy/ListItemButton";
import MenuIcon from "@mui/icons-material/Menu";
import Typography from "@mui/joy/Typography";
import { BrowserRouter, Routes, Route, Link } from "react-router-dom";
import { ErrorMessage } from "./constants";
import { HEADER_HEIGHT_PX, HEADER_MARGIN_PX } from "./styles/sizes";
import { LeaderboardEntry } from "./types";

export default function App() {
    const [leaderboard, setLeaderboard] = useState<LeaderboardEntry[]>([]);
    const [leaderboardYear, setLeaderboardYear] = useState(
        new Date().getFullYear()
    );
    const [loadingLeaderboard, setLoadingLeaderboard] = useState(false);
    const [leaderboardError, setLeaderboardError] = useState<string | null>(
        null
    );

    const getLeaderboard = () => {
        setLoadingLeaderboard(true);

        axios
            // .get("http://localhost:8081/leaderboard", {
            .get("https://api.waroftheringcommunity.net:8080/leaderboard", {
                params: { year: leaderboardYear },
            })
            .then((response) => {
                setLeaderboard(response.data.entries as LeaderboardEntry[]);
            })
            .catch((error) => {
                setLeaderboardError(ErrorMessage.Default);
                console.error(error);
            })
            .finally(() => {
                setLoadingLeaderboard(false);
            });
    };

    useEffect(getLeaderboard, [leaderboardYear]);

    return (
        <CssVarsProvider>
            <CssBaseline />
            <BrowserRouter>
                <header
                    style={{
                        display: "flex",
                        alignItems: "center",
                        width: "100%",
                        height: `${HEADER_HEIGHT_PX}px`,
                        position: "sticky",
                        top: 0,
                        zIndex: 1000,
                        margin: `0 0 ${HEADER_MARGIN_PX}px 0`,
                        padding: "0 10px",
                        boxShadow: "0 0 2px 0 rgba(0, 0, 0, 0.5)",
                        color: "white",
                        backgroundColor: "var(--joy-palette-primary-solidBg)",
                    }}
                >
                    <DrawerNavigation />
                    <Typography
                        level="title-lg"
                        sx={{
                            color: "white",
                            width: "100%",
                            textAlign: "center",
                        }}
                    >
                        WotR Community Ladder
                    </Typography>
                </header>
                <Routes>
                    <Route path="/" element={<Home />} />
                    <Route
                        path="/game-report"
                        element={
                            <GameReportForm
                                leaderboard={leaderboard}
                                loadingLeaderboard={loadingLeaderboard}
                            />
                        }
                    />
                    <Route
                        path="/game-reports"
                        element={
                            <GameReports
                                leaderboard={leaderboard}
                                loadingLeaderboard={loadingLeaderboard}
                            />
                        }
                    />
                    <Route
                        path="/rankings"
                        element={
                            <Rankings
                                leaderboard={leaderboard}
                                year={leaderboardYear}
                                loading={loadingLeaderboard}
                                error={leaderboardError}
                                getLeaderboard={getLeaderboard}
                                setYear={setLeaderboardYear}
                                setError={setLeaderboardError}
                            />
                        }
                    />
                </Routes>
            </BrowserRouter>
        </CssVarsProvider>
    );
}

function Home() {
    return (
        <Box
            gap={4}
            sx={{
                width: "100%",
                display: "flex",
                flexDirection: "column",
                alignItems: "center",
            }}
        >
            <Section>
                <Typography level="title-lg">Welcome!</Typography>
            </Section>

            <Section>
                <Typography level="title-lg" mb={1}>
                    Community Resources*
                </Typography>

                <Typography level="title-sm" mb={1} textAlign="center">
                    *Not affiliated with the official War of the Ring board game
                    produced by Ares Games
                </Typography>

                {[
                    { to: "/game-report", label: "Game Report Form" },
                    { to: "/rankings", label: "Rankings" },
                    { to: "/game-reports", label: "Game Reports" },
                ].map(({ to, label }) => (
                    <MuiLink component={Link} to={to} key={label}>
                        {label}
                    </MuiLink>
                ))}

                {[
                    {
                        label: "Game Client",
                        href: "https://www.dropbox.com/s/vc0q5twum8iol2b/WOTR-GANDALF2.zip?dl=0",
                        isDownload: false,
                    },
                    {
                        label: "Discord",
                        href: "https://discord.gg/yZxQF4fK",
                        isDownload: false,
                    },
                    {
                        label: "WotR Almanac",
                        href: "https://tinyurl.com/WOTRAlmanac",
                        isDownload: false,
                    },
                    {
                        label: "WotR Almanac: Expansions",
                        href: "https://tinyurl.com/WOTRExpansions",
                        isDownload: false,
                    },
                    {
                        label: "WotR Almanac: KoME",
                        href: "https://tinyurl.com/KOMEAlmanac",
                        isDownload: false,
                    },
                    {
                        label: "Legacy Ladder (2024)",
                        href: "https://docs.google.com/spreadsheets/d/1okxuCGH1P9mDT-3Np4IvK-dUhmJHaoxP1-xG5U8CX4U/edit?gid=1355474132#gid=1355474132",
                        isDownload: false,
                    },
                    {
                        label: "Legacy Game Report Form",
                        href: "https://docs.google.com/forms/d/e/1FAIpQLSeHCvS71vBIZnEYErzdgmANWzyiAL57Dv0wJDitXtp6J11tFg/viewform",
                        isDownload: false,
                    },
                ].map(({ label, href, isDownload }) => (
                    <ExternalLink
                        href={href}
                        isDownload={isDownload}
                        key={label}
                    >
                        {label}
                    </ExternalLink>
                ))}
            </Section>

            <Section>
                <Typography level="title-lg" mb={1}>
                    Official Resources
                </Typography>

                {[
                    {
                        label: "Official WotR Game",
                        href: "https://www.aresgames.eu/games/war-of-the-ring-line/war-of-the-ring-second-edition",
                        isDownload: false,
                    },
                    {
                        label: "Official WotR Products",
                        href: "https://www.aresgames.eu/games/war-of-the-ring-line",
                        isDownload: false,
                    },
                    {
                        label: "Official Rules - Base",
                        href: "https://www.aresgames.eu/download/20980/?tmstv=1735779992",
                        isDownload: true,
                    },
                    {
                        label: "Official Rules - LoME",
                        href: "https://www.aresgames.eu/download/21011/?tmstv=1735780330",
                        isDownload: true,
                    },
                    {
                        label: "Official Rules - WoME",
                        href: "https://www.aresgames.eu/download/21189/?tmstv=1735780518",
                        isDownload: true,
                    },
                    {
                        label: "Official Rules - KoME",
                        href: "https://www.aresgames.eu/download/33646/?tmstv=1735780308",
                        isDownload: true,
                    },
                ].map(({ label, href, isDownload }) => (
                    <ExternalLink
                        href={href}
                        isDownload={isDownload}
                        key={label}
                    >
                        {label}
                    </ExternalLink>
                ))}
            </Section>

            <Section>
                <button
                    onClick={async () =>
                        await axios.post(
                            "https://api.waroftheringcommunity.net:8080/auth/google/login"
                        )
                    }
                >
                    Login
                </button>
            </Section>
        </Box>
    );
}

function DrawerNavigation() {
    const [open, setOpen] = React.useState(false);

    return (
        <>
            <IconButton
                variant="solid"
                color="primary"
                sx={{ position: "absolute" }}
                onClick={() => setOpen(true)}
            >
                <MenuIcon />
            </IconButton>
            <Drawer open={open} onClose={() => setOpen(false)}>
                <List
                    size="lg"
                    component="nav"
                    sx={{
                        flex: "none",
                        fontSize: "xl",
                    }}
                >
                    <ListItemButton
                        component={Link}
                        to="/"
                        onClick={() => setOpen(false)}
                    >
                        Home
                    </ListItemButton>
                    <ListItemButton
                        component={Link}
                        to="/game-report"
                        onClick={() => setOpen(false)}
                    >
                        Game Report Form
                    </ListItemButton>
                    <ListItemButton
                        component={Link}
                        to="/rankings"
                        onClick={() => setOpen(false)}
                    >
                        Rankings
                    </ListItemButton>
                    <ListItemButton
                        component={Link}
                        to="/game-reports"
                        onClick={() => setOpen(false)}
                    >
                        Game Reports
                    </ListItemButton>
                    <ListItemButton>Leagues (Coming Soon!)</ListItemButton>
                </List>
            </Drawer>
        </>
    );
}

interface SectionProps {
    children: ReactNode;
}

function Section({ children }: SectionProps) {
    return (
        <Box
            sx={{
                display: "flex",
                flexDirection: "column",
                alignItems: "center",
            }}
        >
            {children}
        </Box>
    );
}
