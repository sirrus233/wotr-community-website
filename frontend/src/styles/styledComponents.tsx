import Box from "@mui/joy/Box";
import { styled } from "@mui/joy/styles";
import sizes from "./sizes";

export const PageContainer = styled(Box)({
    boxSizing: "border-box",
    display: "flex",
    flexDirection: "column",
    height: `calc(100% - ${sizes.headerHeight}px - ${sizes.headerMargin}px)`,
    pb: `${sizes.tableElementsGap}px`,
});
