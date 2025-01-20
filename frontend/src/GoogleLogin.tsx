import React from "react";
import { CredentialResponse, GoogleLogin } from "@react-oauth/google";
import axios from "axios";

export default function GoogleLoginButton() {
    const handleLoginSuccess = async (response: CredentialResponse) => {
        await axios
            .post(
                "https://api.waroftheringcommunity.net:8080/auth/google/login",
                response.credential,
                { headers: { "Content-Type": "text/plain;charset=utf-8" } }
            )
            .catch((error) => {
                console.error(error);
            });
    };

    return (
        <GoogleLogin
            onSuccess={handleLoginSuccess}
            onError={() => console.log("Error logging in.")}
        />
    );
}
