module Api where

import Servant (JSON, Post, ReqBody, (:>))
import Types.Api (GameReport)

type SubmitReportAPI = "submitReport" :> ReqBody '[JSON] GameReport :> Post '[JSON] ()

submitReportAPI :: Proxy SubmitReportAPI
submitReportAPI = Proxy

type Api = SubmitReportAPI

api :: Proxy Api
api = Proxy