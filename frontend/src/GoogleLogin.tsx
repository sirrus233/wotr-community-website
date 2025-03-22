import React from "react";
import { CredentialResponse, GoogleLogin } from "@react-oauth/google";
import axios from "axios";
import { ErrorMessage } from "./constants";
import { API_BASE_URL } from "./env";
import { RefreshRequest } from "./hooks/useRequestState";
import { logNetworkError, toAuthStatus } from "./networkErrorHandlers";

interface Props {
    refreshUserInfo: RefreshRequest;
    clearUserInfo: () => void;
    setLoginError: (error: ErrorMessage) => void;
    setLoadingUserInfo: (loading: boolean) => void;
}

export default function GoogleLoginButton({
    refreshUserInfo,
    clearUserInfo,
    setLoginError,
    setLoadingUserInfo,
}: Props) {
    const onError = (error: unknown) => {
        logNetworkError(error);
        setLoginError(toAuthStatus(error));
        clearUserInfo();
    };

    const handleLoginSuccess = async (response: CredentialResponse) => {
        setLoadingUserInfo(true);
        await axios
            .post(`${API_BASE_URL}/auth/google/login`, response.credential, {
                headers: { "Content-Type": "text/plain;charset=utf-8" },
            })
            .then(() => refreshUserInfo(onError))
            .catch((error) => {
                onError(error);
                setLoadingUserInfo(false);
            });
    };

    return (
        <GoogleLogin
            onSuccess={handleLoginSuccess}
            onError={() => setLoginError(ErrorMessage.NotAuthorizedStatus)}
        />
    );
}
