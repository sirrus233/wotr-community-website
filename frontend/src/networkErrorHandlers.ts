import { ErrorMessage } from "./constants";
import { ServerErrorBody } from "./types";

export function toErrorMessage(error: ServerErrorBody): string {
    switch (error.status) {
        case 401:
            return `${ErrorMessage.NotAuthorizedStatus} ${ErrorMessage.NotAuthorized}`;
        case 422:
            return error.response.data;
        default:
            return ErrorMessage.Default;
    }
}

export function toAuthStatus(error: unknown): ErrorMessage {
    if (isServerError(error) && error.status === 401) {
        return ErrorMessage.NotAuthorizedStatus;
    }
    return ErrorMessage.UnknownAuthStatus;
}

export function logNetworkError(error: unknown) {
    if (isServerError(error)) {
        console.error({
            responseError: error.response,
            requestConfig: error.config,
        });
    } else if (isRequestError(error)) {
        console.error({
            requestError: error.request,
            requestConfig: error.config,
        });
    } else {
        console.error(error);
    }
}

export function isServerError(error: unknown): error is ServerErrorBody {
    return (
        isRegularObject(error) &&
        "status" in error &&
        "config" in error &&
        typeof error.status === "number" &&
        "response" in error &&
        isRegularObject(error.response) &&
        "data" in error.response &&
        typeof error.response.data === "string" &&
        "headers" in error.response
    );
}

function isRequestError(
    error: unknown
): error is { request: unknown; config: unknown } {
    return isRegularObject(error) && "request" in error && "config" in error;
}

function isRegularObject(obj: unknown): obj is object {
    return typeof obj === "object" && obj !== null && !Array.isArray(obj);
}
