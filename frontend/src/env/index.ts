import { MODE } from "env";

export const isProd = MODE === "prod";

export const API_BASE_URL = isProd
    ? "https://api.waroftheringcommunity.net:8080"
    : "http://localhost:8081";
