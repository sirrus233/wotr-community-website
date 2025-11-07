import React from "react";
import Alert from "@mui/joy/Alert";
import Box from "@mui/joy/Box";
import Button from "@mui/joy/Button";
import ClearIcon from "@mui/icons-material/Close";
import IconButton from "@mui/joy/IconButton";
import Typography from "@mui/joy/Typography";

interface Props {
    value: File | null;
    id: string;
    constraintText: string;
    onChange: (file: File | null) => void;
    validate: () => void;
}

export default function FileUpload({
    value,
    id,
    constraintText,
    onChange,
    validate,
}: Props) {
    return (
        <Box>
            <Box display="flex" alignItems="center" gap={1}>
                <input
                    id={id}
                    style={{ display: "none" }}
                    type="file"
                    accept=".log,text/plain"
                    onChange={(event) =>
                        onChange(event.target.files?.[0] || null)
                    }
                    onBlur={validate}
                />

                <label htmlFor={id}>
                    <Button component="span" sx={{ whiteSpace: "nowrap" }}>
                        Choose file
                    </Button>
                </label>

                {value && (
                    <Alert color="success" sx={{ overflow: "hidden" }}>
                        <Box
                            sx={{
                                display: "block",
                                whiteSpace: "nowrap",
                                overflow: "hidden",
                                textOverflow: "ellipsis",
                            }}
                        >
                            {value.name}
                        </Box>
                        <IconButton>
                            <ClearIcon onClick={() => onChange(null)} />
                        </IconButton>
                    </Alert>
                )}
            </Box>

            <Typography level="body-xs" mt={1}>
                {constraintText}
            </Typography>
        </Box>
    );
}
