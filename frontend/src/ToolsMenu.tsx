import axios from "axios";
import React, { CSSProperties, ReactNode, useEffect, useState } from "react";
import Box from "@mui/joy/Box";
import CircularProgress from "@mui/joy/CircularProgress";
import Dropdown from "@mui/joy/Dropdown";
import ExportIcon from "@mui/icons-material/FileDownload";
import LoginIcon from "@mui/icons-material/Login";
import LogoutIcon from "@mui/icons-material/Logout";
import Menu from "@mui/joy/Menu";
import MenuButton from "@mui/joy/MenuButton";
import MenuItem from "@mui/joy/MenuItem";
import ToolboxIcon from "@mui/icons-material/HomeRepairService";
import GoogleLoginButton from "./GoogleLogin";
import Modal from "./Modal";
import SupportLink from "./SupportLink";
import { ErrorMessage } from "./constants";
import { RefreshRequest } from "./hooks/useRequestState";
import { UserInfo } from "./types";
import { API_BASE_URL } from "./env";
import { logNetworkError } from "./networkErrorHandlers";

export interface ToolsMenuProps {
    loadingUserInfo: boolean;
    loginError: string | null;
    userInfo: UserInfo | null;
    refreshUserInfo: RefreshRequest;
    clearUserInfo: () => void;
    setLoginError: (error: ErrorMessage | null) => void;
    setLoadingUserInfo: (loading: boolean) => void;
}

export default function ToolsMenu({
    loadingUserInfo,
    loginError,
    userInfo,
    refreshUserInfo,
    clearUserInfo,
    setLoginError,
    setLoadingUserInfo,
}: ToolsMenuProps) {
    const [isOpen, setIsOpen] = useState(false);
    const [isLoginModalOpen, setIsLoginModalOpen] = useState(false);
    const [exporting, setExporting] = useState(false);
    const [errorData, setErrorData] = useState<{
        title: ReactNode;
        message: ReactNode;
    } | null>(null);

    useEffect(() => {
        if (loginError) {
            setErrorData({
                title: loginError,
                message: ErrorMessage.LoginError,
            });
        }
    }, [loginError]);

    const handleExport = () => {
        setExporting(true);
        axios
            .get(`${API_BASE_URL}/export`, {
                responseType: "blob",
            })
            .then((response) => {
                // https://gist.github.com/javilobo8/097c30a233786be52070986d8cdb1743
                const blob = new Blob([response.data], {
                    type: response.data.type,
                });
                const url = window.URL.createObjectURL(blob);
                const link = document.createElement("a");
                link.href = url;
                const contentDisposition =
                    response.headers["content-disposition"];
                let fileName = "export.zip";
                if (contentDisposition) {
                    const fileNameMatch =
                        contentDisposition.match(/filename="(.+)"/);
                    if (fileNameMatch.length === 2) fileName = fileNameMatch[1];
                }
                link.setAttribute("download", fileName);
                document.body.appendChild(link);
                link.click();
                link.remove();
                window.URL.revokeObjectURL(url);
            })
            .catch((error) => {
                logNetworkError(error);
                setErrorData({
                    title: "There is some new devilry here...",
                    message: ErrorMessage.ExportError,
                });
            })
            .finally(() => setExporting(false));
    };

    const handleLogout = () => {
        const onLogoutEndpointError = (error: unknown) => {
            logNetworkError(error);
            setErrorData({
                title: "The way is shut.",
                message: ErrorMessage.LogoutError,
            });
        };

        const onUserInfoErrorAfterLogout = (error: unknown) => {
            // an error is expected after logout
            logNetworkError(error);
            clearUserInfo();
        };

        setLoadingUserInfo(true);
        axios
            .post(`${API_BASE_URL}/logout`)
            .catch(onLogoutEndpointError)
            .then(() =>
                refreshUserInfo({ onError: onUserInfoErrorAfterLogout })
            )
            .finally(() => setLoadingUserInfo(false));
    };

    return (
        <Dropdown open={isOpen} onOpenChange={(_, isOpen) => setIsOpen(isOpen)}>
            <Modal
                isOpen={!!errorData}
                close={() => {
                    setErrorData(null);
                    setLoginError(null);
                }}
                title={errorData?.title}
            >
                {errorData?.message}
                <SupportLink />
            </Modal>

            <Modal
                isOpen={isLoginModalOpen}
                close={() => setIsLoginModalOpen(false)}
                title={"Admin Sign-In"}
            >
                {loadingUserInfo || userInfo?.isAdmin ? (
                    <Box width={200}>
                        {loadingUserInfo ? (
                            <CircularProgress size="sm" />
                        ) : (
                            "Signed in"
                        )}
                    </Box>
                ) : (
                    <GoogleLoginButton
                        refreshUserInfo={refreshUserInfo}
                        clearUserInfo={clearUserInfo}
                        setLoginError={setLoginError}
                        setLoadingUserInfo={setLoadingUserInfo}
                    />
                )}
            </Modal>

            <ToolsButton loading={exporting || loadingUserInfo} />

            <Menu>
                <Item disabled={exporting} onClick={handleExport}>
                    <ExportIcon sx={{ mr: 1 }} />
                    Export Data
                </Item>

                {userInfo?.isAdmin ? (
                    <Item disabled={loadingUserInfo} onClick={handleLogout}>
                        <LogoutIcon sx={{ mr: 1, pl: "3px" }} />
                        Sign Out
                    </Item>
                ) : (
                    <Item
                        disabled={loadingUserInfo}
                        onClick={() => setIsLoginModalOpen(true)}
                    >
                        <LoginIcon sx={{ mr: 1 }} />
                        Admin Sign-In
                    </Item>
                )}
            </Menu>
        </Dropdown>
    );
}

interface ContainerProps {
    children: ReactNode;
}

interface ToolsButtonProps {
    loading: boolean;
}

function ToolsButton({ loading }: ToolsButtonProps) {
    return (
        <MenuButton
            variant="solid"
            color="primary"
            size="lg"
            sx={{
                p: "5px",
                m: "5px",
                mx: 0,
                minHeight: 0,
            }}
        >
            {loading ? (
                <CircularProgress size="sm" />
            ) : (
                <ToolboxIcon sx={{ color: "inherit", mx: "5px" }} />
            )}
        </MenuButton>
    );
}

type ItemProps = ContainerProps & {
    disabled?: boolean;
    containerStyle?: CSSProperties;
    onClick?: () => void;
};

function Item({ children, disabled, containerStyle = {}, onClick }: ItemProps) {
    return (
        <MenuItem disabled={disabled} sx={{ p: 0 }} onClick={onClick}>
            <Box
                sx={{
                    display: "flex",
                    alignItems: "center",
                    p: 1,
                    width: "100%",
                    ...containerStyle,
                }}
            >
                {children}
            </Box>
        </MenuItem>
    );
}
