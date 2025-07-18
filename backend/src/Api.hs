module Api where

import Servant
  ( AuthProtect,
    Get,
    JSON,
    NoFraming,
    OctetStream,
    PlainText,
    Post,
    PostNoContent,
    QueryParam,
    QueryParam',
    ReqBody,
    Required,
    StdMethod (..),
    StreamGet,
    Verb,
    (:<|>),
    (:>),
  )
import Servant.Multipart (MultipartForm, Tmp)
import Types.Api
  ( DeleteReportRequest,
    EditPlayerRequest,
    ExportResponse,
    GameReportFilterSpec,
    GetLeaderboardResponse,
    GetReportsResponse,
    GoogleLoginResponse,
    IdToken,
    LeagueStatsResponse,
    ModifyReportRequest,
    RemapPlayerRequest,
    RemapPlayerResponse,
    SubmitGameReportResponse,
    SubmitReportRequest,
    UserInfoResponse,
  )
import Types.Auth (ServiceCaller, SessionIdCookie)
import Types.DataField (League, LeagueTier, PlayerName)

type RequiredQueryParam = QueryParam' '[Required]

type AuthGoogleLoginAPI = "auth" :> "google" :> "login" :> ReqBody '[PlainText] IdToken :> Verb 'POST 204 '[JSON] GoogleLoginResponse

type LogoutAPI = "logout" :> PostNoContent

type UserInfoAPI = "userInfo" :> Get '[JSON] UserInfoResponse

type SubmitReportAPI =
  "submitReport" :> MultipartForm Tmp SubmitReportRequest :> Post '[JSON] SubmitGameReportResponse

type GetReportsAPI =
  "reports"
    :> QueryParam "limit" Int64
    :> QueryParam "offset" Int64
    :> QueryParam "filter" GameReportFilterSpec
    :> Get '[JSON] GetReportsResponse

type GetLeaderboardAPI =
  "leaderboard" :> QueryParam "year" Int :> Get '[JSON] GetLeaderboardResponse

type GetLeagueStatsAPI =
  "leagueStats"
    :> RequiredQueryParam "league" League
    :> RequiredQueryParam "tier" LeagueTier
    :> RequiredQueryParam "year" Int
    :> Get '[JSON] LeagueStatsResponse

type ExportAPI = "export" :> StreamGet NoFraming OctetStream ExportResponse

type AdminEditPlayerAPI =
  "editPlayer" :> ReqBody '[JSON] EditPlayerRequest :> PostNoContent

type AdminRemapPlayerAPI =
  "remapPlayer" :> ReqBody '[JSON] RemapPlayerRequest :> Post '[JSON] RemapPlayerResponse

type AdminModifyReportAPI =
  "modifyReport" :> ReqBody '[JSON] ModifyReportRequest :> PostNoContent

type AdminDeleteReportAPI =
  "deleteReport" :> ReqBody '[JSON] DeleteReportRequest :> PostNoContent

type AdminAddLeaguePlayerAPI =
  "addLeaguePlayer"
    :> RequiredQueryParam "league" League
    :> RequiredQueryParam "tier" LeagueTier
    :> RequiredQueryParam "year" Int
    :> QueryParam "playerId" Int64
    :> QueryParam "playerName" PlayerName
    :> PostNoContent

type UpdateActiveStatusAPI = "updateActiveStatus" :> PostNoContent

type Unprotected =
  AuthGoogleLoginAPI
    :<|> SubmitReportAPI
    :<|> GetReportsAPI
    :<|> GetLeaderboardAPI
    :<|> GetLeagueStatsAPI
    :<|> ExportAPI

type Service =
  UpdateActiveStatusAPI

type Protected =
  LogoutAPI
    :<|> UserInfoAPI
    :<|> AdminEditPlayerAPI
    :<|> AdminRemapPlayerAPI
    :<|> AdminModifyReportAPI
    :<|> AdminDeleteReportAPI
    :<|> AdminAddLeaguePlayerAPI

type API = (AuthProtect SessionIdCookie :> Protected) :<|> (AuthProtect ServiceCaller :> Service) :<|> Unprotected
