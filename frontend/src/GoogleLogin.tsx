import React from "react";
import { CredentialResponse, GoogleLogin } from "@react-oauth/google";
import axios from "axios";
import { logNetworkError } from "./networkErrorHandlers";

interface Props {
    getUserInfo: () => void;
}

export default function GoogleLoginButton({ getUserInfo }: Props) {
    const handleLoginSuccess = async (response: CredentialResponse) => {
        await axios
            .post(
                "https://api.waroftheringcommunity.net:8080/auth/google/login",
                response.credential,
                { headers: { "Content-Type": "text/plain;charset=utf-8" } }
            )
            .then(getUserInfo)
            .catch(logNetworkError);
    };

    return (
        <GoogleLogin
            onSuccess={handleLoginSuccess}
            onError={() => console.log("Error logging in.")}
        />
    );
}
