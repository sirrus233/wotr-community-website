import React from "react";
import { CredentialResponse, GoogleLogin } from "@react-oauth/google";
import axios from "axios";

export default function GoogleLoginButton() {
    const handleLoginSuccess = async (response: CredentialResponse) => {
        await axios
            .post(
                "https://api.waroftheringcommunity.net:8080/login",
                response.credential,
                { headers: { "Content-Type": "text/plain" } }
            )
            .catch((error) => {
                console.error(error);
            });
    };

    return <GoogleLogin onSuccess={handleLoginSuccess} />;
}
