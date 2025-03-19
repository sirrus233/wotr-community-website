import { useState } from "react";
import { ErrorMessage } from "../constants";
import { logNetworkError } from "../networkErrorHandlers";

type RequestWithState = (onError?: (err: unknown) => void) => void;

type State<T, I> = [T | I, boolean, ErrorMessage | null];

type Setters<T, I> = [
    React.Dispatch<React.SetStateAction<T | I>>,
    React.Dispatch<React.SetStateAction<boolean>>,
    React.Dispatch<React.SetStateAction<ErrorMessage | null>>
];

interface Response {
    data: any;
}

export default function useRequestState<T, I>(
    initialData: I,
    sendRequest: () => Promise<Response>
): [RequestWithState, State<T, I>, Setters<T, I>] {
    const [data, setData] = useState<T | I>(initialData);
    const [loading, setLoading] = useState(false);
    const [error, setError] = useState<ErrorMessage | null>(null);

    const request: RequestWithState = (
        onError = (err) => {
            logNetworkError(err);
            setError(ErrorMessage.Default);
        }
    ) => {
        setLoading(true);
        setError(null);
        sendRequest()
            .then((res) => setData(res.data as T))
            .catch(onError)
            .finally(() => setLoading(false));
    };

    return [request, [data, loading, error], [setData, setLoading, setError]];
}
