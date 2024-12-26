module Api where

import Servant (JSON, Post, ReqBody, (:>))
import Types.Api (RawGameReport, SubmitGameReportResponse)

type SubmitReportAPI = "submitReport" :> ReqBody '[JSON] RawGameReport :> Post '[JSON] SubmitGameReportResponse

submitReportAPI :: Proxy SubmitReportAPI
submitReportAPI = Proxy

type Api = SubmitReportAPI

api :: Proxy Api
api = Proxy