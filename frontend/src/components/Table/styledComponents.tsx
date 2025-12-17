import Box from "@mui/joy/Box";
import Input from "@mui/joy/Input";
import { styled } from "@mui/joy/styles";

export const FilterContainer = styled(Box)({
    boxSizing: "border-box",
    display: "flex",
    alignItems: "end",
    justifyContent: "center",
    height: "100%",
    padding: "0 5px",
    fontWeight: "normal",
    lineHeight: "1em",
});

export const FilterContainerVertical = styled(FilterContainer)({
    flexDirection: "column",
    alignItems: "center",
    justifyContent: "end",
});

export const StyledDateInput = styled(Input)({
    flex: 1,
    minHeight: 0,
    minWidth: 0,
    width: "100%",
    padding: "0 5px",
    fontSize: "inherit",
    lineHeight: "1em",
    background: "white",
    input: {
        cursor: "text",
        "&::-webkit-calendar-picker-indicator": {
            cursor: "pointer",
        },
    },
});
