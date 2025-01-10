module Api where

import Servant
  ( Get,
    JSON,
    Post,
    PostNoContent,
    QueryParam,
    ReqBody,
    (:<|>),
    (:>),
  )
import Servant.Auth (Auth, Cookie)
import Servant.Multipart (MultipartForm, Tmp)
import Types.Api
  ( AdminUser,
    DeleteReportRequest,
    GetLeaderboardResponse,
    GetReportsResponse,
    LoginRequest,
    LoginResponse,
    ModifyReportRequest,
    RemapPlayerRequest,
    RemapPlayerResponse,
    RenamePlayerRequest,
    SubmitGameReportResponse,
    SubmitReportRequest,
  )

type LoginAPI = "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] LoginResponse

type SubmitReportAPI = "submitReport" :> MultipartForm Tmp SubmitReportRequest :> Post '[JSON] SubmitGameReportResponse

type GetReportsAPI = "reports" :> QueryParam "limit" Int64 :> QueryParam "offset" Int64 :> Get '[JSON] GetReportsResponse

type GetLeaderboardAPI = "leaderboard" :> QueryParam "year" Int :> Get '[JSON] GetLeaderboardResponse

type AdminRenamePlayerAPI = "renamePlayer" :> ReqBody '[JSON] RenamePlayerRequest :> PostNoContent

type AdminRemapPlayerAPI = "remapPlayer" :> ReqBody '[JSON] RemapPlayerRequest :> Post '[JSON] RemapPlayerResponse

type AdminModifyReportAPI = "modifyReport" :> ReqBody '[JSON] ModifyReportRequest :> PostNoContent

type AdminDeleteReportAPI = "deleteReport" :> ReqBody '[JSON] DeleteReportRequest :> PostNoContent

type Unprotected = LoginAPI :<|> SubmitReportAPI :<|> GetReportsAPI :<|> GetLeaderboardAPI

type Protected = AdminRenamePlayerAPI :<|> AdminRemapPlayerAPI :<|> AdminModifyReportAPI :<|> AdminDeleteReportAPI

type API auths = (Auth auths AdminUser :> Protected) :<|> Unprotected

api :: Proxy (API '[Cookie])
api = Proxy
