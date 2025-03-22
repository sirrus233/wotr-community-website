import { useState } from "react";
import { ErrorMessage } from "../constants";
import { logNetworkError } from "../networkErrorHandlers";

type OnError = (err: unknown) => void;

interface RefreshRequestOptions {
    onError?: OnError;
}

export type RequestWithState<P = undefined> = (
    params: P,
    onError?: OnError
) => void;

export type RefreshRequest = (options?: RefreshRequestOptions) => void;

type State<T, P> = [P, T, boolean, ErrorMessage | null];

type Setters<T, P> = [
    React.Dispatch<React.SetStateAction<P>>,
    React.Dispatch<React.SetStateAction<T>>,
    React.Dispatch<React.SetStateAction<boolean>>,
    React.Dispatch<React.SetStateAction<ErrorMessage | null>>
];

interface Response {
    data: any;
}

interface Args<T, P> {
    initialState: T;
    initialParams: P;
    sendRequest: (params: P) => Promise<Response>;
}

export default function useRequestState<T, P = undefined>({
    initialState,
    initialParams,
    sendRequest,
}: Args<T, P>): [RefreshRequest, State<T, P>, Setters<T, P>] {
    const [params, setParams] = useState<P>(initialParams);
    const [data, setData] = useState<T>(initialState);
    const [loading, setLoading] = useState(false);
    const [error, setError] = useState<ErrorMessage | null>(null);

    const request: RequestWithState<P> = (
        params,
        onError = (err) => {
            logNetworkError(err);
            setError(ErrorMessage.Default);
        }
    ) => {
        setLoading(true);
        setError(null);
        sendRequest(params)
            .then((res) => setData(res.data as T))
            .catch(onError)
            .finally(() => setLoading(false));
    };

    const refresh: RefreshRequest = ({ onError } = {}) =>
        request(params, onError);

    return [
        refresh,
        [params, data, loading, error],
        [setParams, setData, setLoading, setError],
    ];
}
