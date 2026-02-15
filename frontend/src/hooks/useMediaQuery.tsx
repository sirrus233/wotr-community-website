import { useCallback, useEffect, useState } from "react";
import { useTheme } from "@mui/joy/styles";

type Breakpoint = "xs" | "sm" | "md" | "lg" | "xl";

export default function useMediaQuery(breakpoint: Breakpoint) {
    const [doesMatch, setDoesMatch] = useState(false);

    const theme = useTheme();

    const handleChange = useCallback(
        (e: MediaQueryListEvent) => setDoesMatch(e.matches),
        [],
    );

    useEffect(function listenForMediaChange() {
        const mediaQueryList = window.matchMedia(
            `${theme.breakpoints.down(breakpoint).slice("@media ".length)}`,
        );
        setDoesMatch(mediaQueryList.matches);
        mediaQueryList.addEventListener("change", handleChange);
        return () => mediaQueryList.removeEventListener("change", handleChange);
    }, []);

    return doesMatch;
}
