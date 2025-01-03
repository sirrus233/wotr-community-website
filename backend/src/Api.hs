module Api where

import Servant (Get, JSON, Post, PostNoContent, QueryParam', ReqBody, Required, (:<|>), (:>))
import Types.Api
  ( GetLeaderboardResponse,
    GetReportsResponse,
    ModifyReportRequest,
    RawGameReport,
    RenamePlayerRequest,
    SubmitGameReportResponse,
  )
import Types.Database (GameReportId)

type SubmitReportAPI = "submitReport" :> ReqBody '[JSON] RawGameReport :> Post '[JSON] SubmitGameReportResponse

type GetReportsAPI = "reports" :> Get '[JSON] GetReportsResponse

type GetLeaderboardAPI = "leaderboard" :> QueryParam' '[Required] "year" Int :> Get '[JSON] GetLeaderboardResponse

type AdminRenamePlayerAPI = "renamePlayer" :> ReqBody '[JSON] RenamePlayerRequest :> PostNoContent

type AdminModifyReportAPI = "modifyReport" :> ReqBody '[JSON] ModifyReportRequest :> PostNoContent

type AdminDeleteReportAPI = "deleteReport" :> ReqBody '[JSON] GameReportId :> PostNoContent

submitReportAPI :: Proxy SubmitReportAPI
submitReportAPI = Proxy

getReportsAPI :: Proxy GetReportsAPI
getReportsAPI = Proxy

getLeaderboardAPI :: Proxy GetLeaderboardAPI
getLeaderboardAPI = Proxy

adminRenamePlayerAPI :: Proxy AdminRenamePlayerAPI
adminRenamePlayerAPI = Proxy

adminModifyReportAPI :: Proxy AdminModifyReportAPI
adminModifyReportAPI = Proxy

adminDeleteReportAPI :: Proxy AdminDeleteReportAPI
adminDeleteReportAPI = Proxy

type Api =
  SubmitReportAPI
    :<|> GetReportsAPI
    :<|> GetLeaderboardAPI
    :<|> AdminRenamePlayerAPI
    :<|> AdminModifyReportAPI
    :<|> AdminDeleteReportAPI

api :: Proxy Api
api = Proxy
