import React, { ReactNode } from "react";
import MaterialModal from "@mui/joy/Modal";
import ModalClose from "@mui/joy/ModalClose";
import Sheet from "@mui/joy/Sheet";
import Typography from "@mui//joy/Typography";

interface Props {
    isOpen: boolean;
    close: () => void;
    title: ReactNode;
    children: ReactNode;
}

export default function Modal({ isOpen, close, title, children }: Props) {
    return (
        <MaterialModal
            aria-labelledby="modal-title"
            aria-describedby="modal-desc"
            open={isOpen}
            onClose={close}
            sx={{
                display: "flex",
                justifyContent: "center",
                alignItems: "center",
            }}
        >
            <Sheet
                variant="outlined"
                sx={{
                    maxWidth: 500,
                    borderRadius: "md",
                    p: 3,
                    boxShadow: "lg",
                }}
            >
                <ModalClose variant="plain" sx={{ m: 1 }} />

                <Typography
                    id="modal-title"
                    level="h4"
                    textColor="inherit"
                    sx={{ fontWeight: "lg", mb: 1 }}
                >
                    {title}
                </Typography>

                <Typography id="modal-desc" textColor="text.tertiary">
                    {children}
                </Typography>
            </Sheet>
        </MaterialModal>
    );
}
