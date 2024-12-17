module Api where

import Servant (JSON, Post, ReqBody, (:>))
import Types.Api (GameReport, SubmitGameReportResponse)

type SubmitReportAPI = "submitReport" :> ReqBody '[JSON] GameReport :> Post '[JSON] SubmitGameReportResponse

submitReportAPI :: Proxy SubmitReportAPI
submitReportAPI = Proxy

type Api = SubmitReportAPI

api :: Proxy Api
api = Proxy