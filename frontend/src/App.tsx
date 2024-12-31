import { CssVarsProvider } from "@mui/joy/styles";
import CssBaseline from "@mui/joy/CssBaseline";
import React from "react";
import GameReportForm from "./GameReportForm";
import GameReports from "./GameReports";
import Rankings from "./Rankings";
import IconButton from "@mui/joy/IconButton";
import Drawer from "@mui/joy/Drawer";
import List from "@mui/joy/List";
import ListItemButton from "@mui/joy/ListItemButton";
import { BrowserRouter, Routes, Route, Link } from "react-router-dom";

export default function App() {
    return (
        <CssVarsProvider>
            <CssBaseline />
            <BrowserRouter>
                <DrawerNavigation />
                <Routes>
                    <Route path="/" element={<Home />} />
                    <Route path="/game-report" element={<GameReportForm />} />
                    <Route path="/game-reports" element={<GameReports />} />
                    <Route path="/rankings" element={<Rankings />} />
                </Routes>
            </BrowserRouter>
        </CssVarsProvider>
    );
}

function Home() {
    return <div>Welcome to the Home Page</div>;
}

function DrawerNavigation() {
    const [open, setOpen] = React.useState(false);

    return (
        <>
            <IconButton
                variant="solid"
                color="primary"
                onClick={() => setOpen(true)}
            />
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
