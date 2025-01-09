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
import Servant.Auth (Auth)
import Servant.Multipart (MultipartForm, Tmp)
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

type SubmitReportAPI = "submitReport" :> MultipartForm Tmp SubmitReportRequest :> Post '[JSON] SubmitGameReportResponse

type GetReportsAPI = "reports" :> QueryParam "limit" Int64 :> QueryParam "offset" Int64 :> Get '[JSON] GetReportsResponse

type GetLeaderboardAPI = "leaderboard" :> QueryParam "year" Int :> Get '[JSON] GetLeaderboardResponse

type AdminRenamePlayerAPI = "renamePlayer" :> ReqBody '[JSON] RenamePlayerRequest :> PostNoContent

type AdminRemapPlayerAPI = "remapPlayer" :> ReqBody '[JSON] RemapPlayerRequest :> Post '[JSON] RemapPlayerResponse

type AdminModifyReportAPI = "modifyReport" :> ReqBody '[JSON] ModifyReportRequest :> PostNoContent

type AdminDeleteReportAPI = "deleteReport" :> ReqBody '[JSON] DeleteReportRequest :> PostNoContent

submitReportAPI :: Proxy SubmitReportAPI
submitReportAPI = Proxy

getReportsAPI :: Proxy GetReportsAPI
getReportsAPI = Proxy

getLeaderboardAPI :: Proxy GetLeaderboardAPI
getLeaderboardAPI = Proxy

adminRenamePlayerAPI :: Proxy AdminRenamePlayerAPI
adminRenamePlayerAPI = Proxy

adminRemapPlayerAPI :: Proxy AdminRemapPlayerAPI
adminRemapPlayerAPI = Proxy

adminModifyReportAPI :: Proxy AdminModifyReportAPI
adminModifyReportAPI = Proxy

adminDeleteReportAPI :: Proxy AdminDeleteReportAPI
adminDeleteReportAPI = Proxy

type Unprotected = SubmitReportAPI :<|> GetReportsAPI :<|> GetLeaderboardAPI

type Protected = AdminRenamePlayerAPI :<|> AdminRemapPlayerAPI :<|> AdminModifyReportAPI :<|> AdminDeleteReportAPI

type Api auths = (Auth auths AdminUser :> Protected) :<|> Unprotected

api :: Proxy (Api auths)
api = Proxy
