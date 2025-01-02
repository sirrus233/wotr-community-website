module Api where

import Servant (Get, JSON, Post, QueryParam, ReqBody, (:<|>), (:>))
import Types.Api (GetLeaderboardResponse, GetReportsResponse, RawGameReport, SubmitGameReportResponse)

type SubmitReportAPI = "submitReport" :> ReqBody '[JSON] RawGameReport :> Post '[JSON] SubmitGameReportResponse

type GetReportsAPI = "reports" :> Get '[JSON] GetReportsResponse

type GetLeaderboardAPI = "leaderboard" :> QueryParam "year" Int :> Get '[JSON] GetLeaderboardResponse

submitReportAPI :: Proxy SubmitReportAPI
submitReportAPI = Proxy

getReportsAPI :: Proxy GetReportsAPI
getReportsAPI = Proxy

getLeaderboardAPI :: Proxy GetLeaderboardAPI
getLeaderboardAPI = Proxy

type Api = SubmitReportAPI :<|> GetReportsAPI :<|> GetLeaderboardAPI

api :: Proxy Api
api = Proxy
