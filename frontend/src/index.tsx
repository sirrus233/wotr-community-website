import React from "react";
import { createRoot } from "react-dom/client";
import App from "./components/App";
import "./styles/index.css";
import axios from "axios";

axios.defaults.withCredentials = true;

const container = document.getElementById("root");

if (container !== null) {
    const root = createRoot(container);
    root.render(<App />);
} else {
    console.error("Missing root element in document.");
}
