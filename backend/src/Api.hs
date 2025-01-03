module Api where

import Servant (Get, JSON, Post, PostNoContent, QueryParam', ReqBody, Required, (:<|>), (:>))
import Types.Api
  ( GetLeaderboardResponse,
    GetReportsResponse,
    RawGameReport,
    RenamePlayerRequest,
    SubmitGameReportResponse,
  )

type SubmitReportAPI = "submitReport" :> ReqBody '[JSON] RawGameReport :> Post '[JSON] SubmitGameReportResponse

type GetReportsAPI = "reports" :> Get '[JSON] GetReportsResponse

type GetLeaderboardAPI = "leaderboard" :> QueryParam' '[Required] "year" Int :> Get '[JSON] GetLeaderboardResponse

type AdminRenamePlayerAPI = "renamePlayer" :> ReqBody '[JSON] RenamePlayerRequest :> PostNoContent

submitReportAPI :: Proxy SubmitReportAPI
submitReportAPI = Proxy

getReportsAPI :: Proxy GetReportsAPI
getReportsAPI = Proxy

getLeaderboardAPI :: Proxy GetLeaderboardAPI
getLeaderboardAPI = Proxy

adminRenamePlayerAPI :: Proxy AdminRenamePlayerAPI
adminRenamePlayerAPI = Proxy

type Api =
  SubmitReportAPI
    :<|> GetReportsAPI
    :<|> GetLeaderboardAPI
    :<|> AdminRenamePlayerAPI

api :: Proxy Api
api = Proxy
