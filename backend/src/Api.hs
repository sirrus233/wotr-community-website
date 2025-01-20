module Api where

import Servant
  ( AuthProtect,
    Get,
    Header,
    Header',
    JSON,
    NoContent,
    OctetStream,
    PlainText,
    Post,
    PostNoContent,
    QueryParam,
    QueryParam',
    ReqBody,
    Required,
    StdMethod (..),
    Strict,
    Verb,
    (:<|>),
    (:>),
  )
import Servant.Multipart (MultipartForm, Tmp)
import Servant.Server.Experimental.Auth (AuthServerData)
import Types.Api
  ( AdminUser,
    DeleteReportRequest,
    GetLeaderboardResponse,
    GetReportsResponse,
    ModifyReportRequest,
    RemapPlayerRequest,
    RemapPlayerResponse,
    RenamePlayerRequest,
    SubmitGameReportResponse,
    SubmitReportRequest,
  )

type AuthGoogleLoginAPI =
  "auth" :> "google" :> "login" :> ReqBody '[PlainText] Text :> PostNoContent

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

-- TODO Weird type tag?
data CookieAuth = CookieAuth

type instance AuthServerData (AuthProtect CookieAuth) = AdminUser

type API = (AuthProtect CookieAuth :> Protected) :<|> Unprotected
