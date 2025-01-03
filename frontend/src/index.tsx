import React from "react";
import { createRoot } from "react-dom/client";
import App from "./App";
import "./styles/index.css";

const container = document.getElementById("root");

if (container !== null) {
    const root = createRoot(container);
    root.render(<App />);
} else {
    console.error("Missing root element in document.");
}
