module Api where

import Servant (Get, JSON, Post, ReqBody, (:<|>), (:>))
import Types.Api (GetReportsResponse, RawGameReport, SubmitGameReportResponse)

type SubmitReportAPI = "submitReport" :> ReqBody '[JSON] RawGameReport :> Post '[JSON] SubmitGameReportResponse

type GetReportsAPI = "reports" :> Get '[JSON] GetReportsResponse

submitReportAPI :: Proxy SubmitReportAPI
submitReportAPI = Proxy

getReportsAPI :: Proxy GetReportsAPI
getReportsAPI = Proxy

type Api = SubmitReportAPI :<|> GetReportsAPI

api :: Proxy Api
api = Proxy