module Api where

import Servant (JSON, Post, ReqBody, (:>))
import Types.Api (GameReport)
import Types.Database (ReadProcessedGameReport)

type SubmitReportAPI = "submitReport" :> ReqBody '[JSON] GameReport :> Post '[JSON] ReadProcessedGameReport

submitReportAPI :: Proxy SubmitReportAPI
submitReportAPI = Proxy

type Api = SubmitReportAPI

api :: Proxy Api
api = Proxy