import React from "react";
import { CredentialResponse, GoogleLogin } from "@react-oauth/google";
import axios from "axios";
import { ErrorMessage } from "./constants";
import { API_BASE_URL } from "./env";
import { logNetworkError, toAuthStatus } from "./networkErrorHandlers";

interface Props {
    getUserInfo: (onError: (error: unknown) => void) => void;
    clearUserInfo: () => void;
    setLoginError: (error: ErrorMessage) => void;
    setLoadingUserInfo: (loading: boolean) => void;
}

export default function GoogleLoginButton({
    getUserInfo,
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
            .then(() => getUserInfo(onError))
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
