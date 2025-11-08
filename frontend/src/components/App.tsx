import axios from "axios";
import { CssVarsProvider, extendTheme } from "@mui/joy/styles";
import CssBaseline from "@mui/joy/CssBaseline";
import React, { ReactNode, useEffect } from "react";
import ExternalLink from "./ExternalLink";
import GameReportForm from "./GameReportForm";
import GameReports, { serializeReportsParams } from "./GameReports";
import Rankings from "./Rankings";
import CheckIcon from "@mui/icons-material/Check";
import CircularProgress from "@mui/joy/CircularProgress";
import Box from "@mui/joy/Box";
import Button from "@mui/joy/Button";
import IconButton from "@mui/joy/IconButton";
import Drawer from "@mui/joy/Drawer";
import List from "@mui/joy/List";
import ListItemButton from "@mui/joy/ListItemButton";
import MenuIcon from "@mui/icons-material/Menu";
import Typography from "@mui/joy/Typography";
import {
    BrowserRouter,
    Routes,
    Route,
    Link,
    useLocation,
} from "react-router-dom";
import ringImgPath from "../assets/ring.png";
import useRequestState from "../hooks/useRequestState";
import { logNetworkError } from "../networkErrorHandlers";
import {
    AboveLarge,
    AboveSmall,
    AboveMedium,
    BelowLarge,
    BelowSmall,
} from "../styles/breakpoints";
import "../styles/index.css";
import sizes from "../styles/sizes";
import {
    GameReportParams,
    LeaderboardEntry,
    LeaderboardParams,
    LeagueParams,
    LeagueStats,
    MenuOption,
    ProcessedGameReport,
    UserInfo,
} from "../types";
import { API_BASE_URL } from "../env";
import { GoogleOAuthProvider } from "@react-oauth/google";
import Leagues from "./Leagues";
import ToolsMenu, { ToolsMenuProps } from "./ToolsMenu";

const routesData = [
    { to: "/", label: "Home" },
    { to: "/game-report", label: "Game Report Form" },
    { to: "/rankings", label: "Rankings" },
    { to: "/game-reports", label: "Game Reports" },
    { to: "/leagues", label: "Leagues" },
];

const PAGE_LIMIT = 100;

export default function App() {
    const [
        refreshUserInfo,
        [, userInfo, loadingUserInfo, loginError],
        [, setUserInfo, setLoadingUserInfo, setLoginError],
    ] = useRequestState<UserInfo | null>({
        initialState: null,
        initialParams: undefined,
        sendRequest: () => axios.get(`${API_BASE_URL}/userInfo`),
    });

    const [
        refreshLeaderboard,
        [leaderboardParams, leaderboard, loadingLeaderboard, leaderboardError],
        [setLeaderboardParams],
    ] = useRequestState<{ entries: LeaderboardEntry[] }, LeaderboardParams>({
        initialState: { entries: [] },
        initialParams: { year: new Date().getFullYear() },
        sendRequest: (params) =>
            axios.get(`${API_BASE_URL}/leaderboard`, { params }),
    });

    const [
        refreshLeagueStats,
        [leagueParams, leagueStats, loadingLeague, leagueError],
        [setLeagueParams],
    ] = useRequestState<LeagueStats, LeagueParams>({
        initialState: {},
        initialParams: {
            league: "GeneralLeague",
            tier: "Tier1",
            year: new Date().getFullYear(),
        },
        sendRequest: (params) =>
            axios.get(`${API_BASE_URL}/leagueStats`, { params }),
    });

    const [
        refreshReports,
        [reportsParams, reportsData, loadingReports, reportsError],
        [setReportsParams],
    ] = useRequestState<
        { total: number; reports: ProcessedGameReport[] },
        GameReportParams
    >({
        initialState: { total: 0, reports: [] },
        initialParams: {
            limit: PAGE_LIMIT,
            currentPage: 1,
            filters: {
                pairing: [],
                players: [],
                winners: [],
                losers: [],
                leagues: [],
                victory: [],
                turns: null,
                tokens: null,
                dwarvenRings: null,
                corruption: null,
                mordor: null,
                aragorn: null,
                initialEyes: null,
                interestRating: null,
                hasLog: null,
                treebeard: null,
            },
        },
        sendRequest: (params) => {
            return axios.get(`${API_BASE_URL}/reports`, {
                params: serializeReportsParams(params),
            });
        },
    });

    const playerOptions = leaderboard.entries
        .map(
            (entry): MenuOption<number> => ({
                label: entry.name,
                id: entry.pid,
            })
        )
        .sort((a, b) => a.label.localeCompare(b.label));

    const playerNames = playerOptions.map(({ label }) => label);

    const clearUserInfo = () => setUserInfo(null);

    useEffect(refreshLeaderboard, [leaderboardParams]);
    useEffect(refreshLeagueStats, [leagueParams]);
    useEffect(refreshReports, [reportsParams]);
    useEffect(() => {
        const onError = (error: unknown) => {
            logNetworkError(error);
            clearUserInfo();
        };
        refreshUserInfo({ onError });
    }, []);

    const theme = extendTheme({
        breakpoints: {
            values: {
                xs: 0,
                sm: 450,
                md: 700,
                lg: 1100,
                xl: 1536,
            },
        },
    });

    return (
        // cspell:disable-next-line
        <GoogleOAuthProvider clientId="331114708951-rhdksfhejc8l5tif6qd3ofuj6uc2e4pg.apps.googleusercontent.com">
            <CssVarsProvider theme={theme}>
                <CssBaseline />
                <BrowserRouter>
                    <Header
                        toolsMenuProps={{
                            loadingUserInfo,
                            loginError,
                            userInfo,
                            refreshUserInfo,
                            clearUserInfo,
                            setLoginError,
                            setLoadingUserInfo,
                        }}
                    />

                    <Routes>
                        <Route path="/" element={<Home />} />
                        <Route
                            path="/game-report"
                            element={
                                <GameReportForm
                                    playerNames={playerNames}
                                    loadingPlayers={loadingLeaderboard}
                                    refreshGameReports={refreshReports}
                                    refreshLeaderboard={refreshLeaderboard}
                                />
                            }
                        />
                        <Route
                            path="/game-reports"
                            element={
                                <GameReports
                                    reports={reportsData.reports}
                                    totalReportCount={reportsData.total}
                                    playerOptions={playerOptions}
                                    loadingReports={loadingReports}
                                    loadingPlayers={loadingLeaderboard}
                                    isAdmin={userInfo?.isAdmin || false}
                                    error={reportsError}
                                    params={reportsParams}
                                    setParams={setReportsParams}
                                    refresh={refreshReports}
                                    refreshLeaderboard={refreshLeaderboard}
                                />
                            }
                        />
                        <Route
                            path="/rankings"
                            element={
                                <Rankings
                                    entries={leaderboard.entries}
                                    loading={loadingLeaderboard}
                                    error={leaderboardError}
                                    isAdmin={userInfo?.isAdmin || false}
                                    params={leaderboardParams}
                                    setParams={setLeaderboardParams}
                                    refresh={refreshLeaderboard}
                                />
                            }
                        />
                        <Route
                            path="/leagues"
                            element={
                                <Leagues
                                    stats={leagueStats}
                                    playerNames={playerNames}
                                    loading={loadingLeague}
                                    loadingPlayers={loadingLeaderboard}
                                    isAdmin={userInfo?.isAdmin || false}
                                    error={leagueError}
                                    params={leagueParams}
                                    setParams={setLeagueParams}
                                    refresh={refreshLeagueStats}
                                />
                            }
                        />
                    </Routes>
                </BrowserRouter>
            </CssVarsProvider>
        </GoogleOAuthProvider>
    );
}

interface HeaderProps {
    toolsMenuProps: ToolsMenuProps;
}

function Header({ toolsMenuProps }: HeaderProps) {
    const { loadingUserInfo, userInfo } = toolsMenuProps;
    const { pathname } = useLocation();

    return (
        <header
            style={{
                display: "flex",
                alignItems: "center",
                width: "100%",
                height: `${sizes.headerHeight}px`,
                position: "sticky",
                top: 0,
                zIndex: 1000,
                margin: `0 0 ${sizes.headerMargin}px 0`,
                padding: "0 10px",
                boxShadow: "0 0 2px 0 rgba(0, 0, 0, 0.5)",
                color: "white",
                backgroundColor: "var(--joy-palette-primary-solidBg)",
            }}
        >
            <Typography
                component={Link}
                to="/"
                level="title-lg"
                sx={{
                    display: "flex",
                    alignItems: "center",
                    color: "white",
                    textDecoration: "none",
                }}
            >
                <img
                    src={ringImgPath}
                    alt="The one ring"
                    style={{ paddingRight: "10px" }}
                    width="40px"
                />

                <Box whiteSpace="nowrap">
                    <AboveSmall>War of the Ring Community</AboveSmall>
                    <BelowSmall>WotR Community</BelowSmall>
                </Box>
            </Typography>

            <Box
                sx={{
                    display: "flex",
                    alignItems: "center",
                    position: "absolute",
                    px: 1,
                    right: 0,
                }}
            >
                <AboveLarge>
                    <Box sx={{ display: "flex" }}>
                        {routesData.map(({ to, label }) => (
                            <Typography
                                key={label}
                                component={Link}
                                to={to}
                                variant="solid"
                                color="primary"
                                sx={{
                                    textDecoration: "none",
                                    mr: 1,
                                    px: 1,
                                    py: "5px",
                                    borderRadius: "5px",
                                    ":hover": {
                                        background:
                                            "var(--joy-palette-primary-softBg)",
                                        color: "var(--joy-palette-primary-solidBg)",
                                    },
                                    ":last-child": { mr: 0 },
                                }}
                            >
                                {label}
                                {to === pathname && (
                                    <Box borderBottom="1px solid white" />
                                )}
                            </Typography>
                        ))}
                    </Box>
                </AboveLarge>

                <Box display="flex" alignItems="center">
                    {loadingUserInfo ? (
                        <CircularProgress size="sm" />
                    ) : (
                        userInfo?.isAdmin && (
                            <>
                                <AboveSmall>
                                    <CheckIcon
                                        sx={{
                                            mx: "5px",
                                            color: "inherit",
                                            display: "flex",
                                        }}
                                    />
                                </AboveSmall>

                                <AboveMedium>
                                    <Box mr="5px">Signed in</Box>
                                </AboveMedium>
                            </>
                        )
                    )}

                    <ToolsMenu {...toolsMenuProps} />
                </Box>

                <BelowLarge>
                    <DrawerNavigation pathname={pathname} />
                </BelowLarge>
            </Box>
        </header>
    );
}

function Home() {
    return (
        <Box
            gap={3}
            sx={{
                width: "100%",
                display: "flex",
                flexDirection: "column",
                alignItems: "center",
                boxSizing: "border-box",
                px: 3,
                pb: 3,
            }}
        >
            <Section>
                <Typography level="h4">Welcome!</Typography>
            </Section>

            <Section>
                <Button
                    component={Link}
                    to="/game-report"
                    sx={{ textAlign: "center" }}
                >
                    Submit a game report
                </Button>
            </Section>

            <Section>
                <Box display="flex" flexDirection="row" gap={1}>
                    {[
                        { to: "/rankings", label: "View rankings" },
                        { to: "/game-reports", label: "View game reports" },
                        { to: "/leagues", label: "View leagues" },
                    ].map(({ to, label }) => (
                        <Button
                            component={Link}
                            to={to}
                            key={label}
                            variant="outlined"
                            sx={{ textAlign: "center" }}
                        >
                            {label}
                        </Button>
                    ))}
                </Box>
            </Section>

            <Section>
                <Typography level="h4">Community Resources</Typography>

                <Subtitle>Find and Play Games</Subtitle>

                {[
                    {
                        label: "Game Client",
                        href: "https://www.dropbox.com/s/vc0q5twum8iol2b/WOTR-GANDALF2.zip?dl=0",
                        isDownload: false,
                    },
                    {
                        label: "Discord",
                        href: "https://discord.gg/DUdcpdyNMm",
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

                <Subtitle>Rules Clarifications and FAQ</Subtitle>

                {[
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
                        label: "League Rules",
                        href: "https://docs.google.com/document/d/1T2OIOfU9vlSJQd0xPoivFmHOUTvPOa1jjtaiN2vrHAQ",
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

                <Subtitle>Technical Support</Subtitle>

                {[
                    {
                        label: "Java Client & Hamachi Setup",
                        href: "https://www.youtube.com/watch?v=g6xt-xC9dXY",
                        isDownload: false,
                    },
                    {
                        label: "ZeroTier Setup",
                        href: "https://www.youtube.com/watch?v=mKkia_KGzzo",
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

                <Subtitle>Legacy</Subtitle>

                {[
                    {
                        label: "Legacy Ladder (2024)",
                        href: "https://docs.google.com/spreadsheets/d/1kPk4X-pkWe2-ORgGqRSwfVuaMlwAsKInOVcQpb7OGbA/edit?usp=drive_link",
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
                <Typography level="h4" mb={1}>
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
                <Typography level="title-sm" mt={1} textAlign="center">
                    This website is not affiliated with the official War of the
                    Ring board game produced by Ares Games.
                </Typography>
            </Section>
        </Box>
    );
}

interface DrawerNavigationProps {
    pathname: string;
}

function DrawerNavigation({ pathname }: DrawerNavigationProps) {
    const [open, setOpen] = React.useState(false);

    return (
        <>
            <IconButton
                variant="solid"
                color="primary"
                onClick={() => setOpen(true)}
            >
                <MenuIcon />
            </IconButton>
            <Drawer open={open} onClose={() => setOpen(false)} anchor="right">
                <List
                    size="lg"
                    component="nav"
                    sx={{
                        flex: "none",
                        fontSize: "xl",
                    }}
                >
                    {routesData.map(({ to, label }) => (
                        <ListItemButton
                            key={label}
                            component={Link}
                            to={to}
                            onClick={() => setOpen(false)}
                            variant={to === pathname ? "soft" : "plain"}
                            sx={{
                                fontWeight: to === pathname ? "bold" : "normal",
                            }}
                        >
                            {label}
                        </ListItemButton>
                    ))}
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

interface SubtitleProps {
    children: ReactNode;
}

function Subtitle({ children }: SubtitleProps) {
    return (
        <Typography fontWeight="bold" my={1} sx={{ color: "#666" }}>
            {children}
        </Typography>
    );
}
