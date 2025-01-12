module Api where

import Network.OAuth.OAuth2 (ExchangeToken)
import Network.OAuth2.Experiment (AuthorizeState)
import Servant
  ( AuthProtect,
    Get,
    JSON,
    NoContent,
    PlainText,
    Post,
    QueryParam,
    ReqBody,
    StdMethod (..),
    Verb,
    (:<|>),
    (:>),
  )
import Servant.Multipart (MultipartForm, Tmp)
import Types.Api
  ( DeleteReportRequest,
    GetLeaderboardResponse,
    GetReportsResponse,
    ModifyReportRequest,
    RemapPlayerRequest,
    RemapPlayerResponse,
    RenamePlayerRequest,
    SubmitGameReportResponse,
    SubmitReportRequest,
  )

-- TODO What are the response status codes of the NoContent requests now? Still 204? Or 200?
type Get302 = Verb 'GET 302 '[PlainText] NoContent

type AuthGoogleLoginAPI = "auth" :> "google" :> "login" :> Get302

type AuthGoogleCallbackAPI = "auth" :> "google" :> "callback" :> QueryParam "code" ExchangeToken :> QueryParam "state" AuthorizeState :> Get302

type SubmitReportAPI = "submitReport" :> MultipartForm Tmp SubmitReportRequest :> Post '[JSON] SubmitGameReportResponse

type GetReportsAPI = "reports" :> QueryParam "limit" Int64 :> QueryParam "offset" Int64 :> Get '[JSON] GetReportsResponse

type GetLeaderboardAPI = "leaderboard" :> QueryParam "year" Int :> Get '[JSON] GetLeaderboardResponse

type AdminRenamePlayerAPI = "renamePlayer" :> ReqBody '[JSON] RenamePlayerRequest :> Post '[JSON] NoContent

type AdminRemapPlayerAPI = "remapPlayer" :> ReqBody '[JSON] RemapPlayerRequest :> Post '[JSON] RemapPlayerResponse

type AdminModifyReportAPI = "modifyReport" :> ReqBody '[JSON] ModifyReportRequest :> Post '[JSON] NoContent

type AdminDeleteReportAPI = "deleteReport" :> ReqBody '[JSON] DeleteReportRequest :> Post '[JSON] NoContent

type Unprotected = AuthGoogleLoginAPI :<|> AuthGoogleCallbackAPI :<|> SubmitReportAPI :<|> GetReportsAPI :<|> GetLeaderboardAPI

type Protected = AdminRenamePlayerAPI :<|> AdminRemapPlayerAPI :<|> AdminModifyReportAPI :<|> AdminDeleteReportAPI

type API = (AuthProtect "cookie-auth" :> Protected) :<|> Unprotected

api :: Proxy API
api = Proxy
