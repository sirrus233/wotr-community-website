import React from "react";
import Input from "@mui/joy/Input";

interface TextInputProps {
    value: string;
    placeholder: string;
    onChange: (value: string) => void;
    validate: () => void;
}

export default function TextInput({
    value,
    placeholder,
    onChange,
    validate,
}: TextInputProps) {
    return (
        <Input
            value={value}
            placeholder={placeholder}
            onChange={(event) => onChange(event.target.value)}
            onBlur={validate}
        />
    );
}
