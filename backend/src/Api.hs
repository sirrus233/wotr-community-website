module Api where

import Auth (SessionIdCookie)
import Servant
  ( AuthProtect,
    Get,
    JSON,
    PlainText,
    Post,
    PostNoContent,
    QueryParam,
    ReqBody,
    (:<|>),
    (:>),
  )
import Servant.Multipart (MultipartForm, Tmp)
import Types.Api
  ( DeleteReportRequest,
    GetLeaderboardResponse,
    GetReportsResponse,
    IdToken,
    ModifyReportRequest,
    RemapPlayerRequest,
    RemapPlayerResponse,
    RenamePlayerRequest,
    SubmitGameReportResponse,
    SubmitReportRequest,
  )

type AuthGoogleLoginAPI =
  "auth" :> "google" :> "login" :> ReqBody '[PlainText] IdToken :> PostNoContent

type SubmitReportAPI =
  "submitReport" :> MultipartForm Tmp SubmitReportRequest :> Post '[JSON] SubmitGameReportResponse

type GetReportsAPI =
  "reports" :> QueryParam "limit" Int64 :> QueryParam "offset" Int64 :> Get '[JSON] GetReportsResponse

type GetLeaderboardAPI =
  "leaderboard" :> QueryParam "year" Int :> Get '[JSON] GetLeaderboardResponse

type AdminRenamePlayerAPI =
  "renamePlayer" :> ReqBody '[JSON] RenamePlayerRequest :> PostNoContent

type AdminRemapPlayerAPI =
  "remapPlayer" :> ReqBody '[JSON] RemapPlayerRequest :> Post '[JSON] RemapPlayerResponse

type AdminModifyReportAPI =
  "modifyReport" :> ReqBody '[JSON] ModifyReportRequest :> PostNoContent

type AdminDeleteReportAPI =
  "deleteReport" :> ReqBody '[JSON] DeleteReportRequest :> PostNoContent

-- TODO Add logout and logged-in verification APIs

type Unprotected =
  AuthGoogleLoginAPI
    :<|> SubmitReportAPI
    :<|> GetReportsAPI
    :<|> GetLeaderboardAPI

type Protected =
  AdminRenamePlayerAPI
    :<|> AdminRemapPlayerAPI
    :<|> AdminModifyReportAPI
    :<|> AdminDeleteReportAPI

type API = (AuthProtect SessionIdCookie :> Protected) :<|> Unprotected
