import Box from "@mui/joy/Box";
import FormControl from "@mui/joy/FormControl";
import FormLabel from "@mui/joy/FormLabel";
import Input from "@mui/joy/Input";
import { styled } from "@mui/joy/styles";
import { TABLE_FILTER_HEIGHT } from "./constants";

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

export const FormControlGridRow = styled(FormControl)<{
    $columnWidths: string;
}>(({ $columnWidths }) => ({
    display: "grid",
    gap: "5px",
    gridTemplateColumns: $columnWidths,
}));

export const DateInput = styled(Input)({
    minHeight: 0,
    minWidth: 0,
    width: "100%",
    height: TABLE_FILTER_HEIGHT,
    padding: "0 5px",
    fontSize: "inherit",
    background: "white",
    input: {
        cursor: "text",
        "&::-webkit-calendar-picker-indicator": {
            cursor: "pointer",
        },
    },
});

export const InputLabel = styled(FormLabel)({
    display: "flex",
    alignItems: "center",
    justifyContent: "end",
    height: "100%",
    width: "100%",
    fontSize: "1em",
    margin: 0,
});
