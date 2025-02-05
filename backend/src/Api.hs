module Api where

import Servant
  ( AuthProtect,
    Get,
    JSON,
    PlainText,
    Post,
    PostNoContent,
    QueryParam,
    QueryParam',
    ReqBody,
    Required,
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
    GoogleLoginResponse,
    IdToken,
    LeagueStatsResponse,
    ModifyReportRequest,
    RemapPlayerRequest,
    RemapPlayerResponse,
    RenamePlayerRequest,
    SubmitGameReportResponse,
    SubmitReportRequest,
    UserInfoResponse,
  )
import Types.Auth (SessionIdCookie)
import Types.DataField (League, LeagueTier, PlayerName)

type RequiredQueryParam = QueryParam' '[Required]

type AuthGoogleLoginAPI = "auth" :> "google" :> "login" :> ReqBody '[PlainText] IdToken :> Verb 'POST 204 '[JSON] GoogleLoginResponse

type LogoutAPI = "logout" :> PostNoContent

type UserInfoAPI = "userInfo" :> Get '[JSON] UserInfoResponse

type SubmitReportAPI =
  "submitReport" :> MultipartForm Tmp SubmitReportRequest :> Post '[JSON] SubmitGameReportResponse

type GetReportsAPI =
  "reports" :> QueryParam "limit" Int64 :> QueryParam "offset" Int64 :> Get '[JSON] GetReportsResponse

type GetLeaderboardAPI =
  "leaderboard" :> QueryParam "year" Int :> Get '[JSON] GetLeaderboardResponse

type GetLeagueStatsAPI =
  "leagueStats"
    :> RequiredQueryParam "league" League
    :> RequiredQueryParam "tier" LeagueTier
    :> RequiredQueryParam "year" Int
    :> Get '[JSON] LeagueStatsResponse

type AdminRenamePlayerAPI =
  "renamePlayer" :> ReqBody '[JSON] RenamePlayerRequest :> PostNoContent

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

type Unprotected =
  AuthGoogleLoginAPI
    :<|> SubmitReportAPI
    :<|> GetReportsAPI
    :<|> GetLeaderboardAPI
    :<|> GetLeagueStatsAPI

type Protected =
  LogoutAPI
    :<|> UserInfoAPI
    :<|> AdminRenamePlayerAPI
    :<|> AdminRemapPlayerAPI
    :<|> AdminModifyReportAPI
    :<|> AdminDeleteReportAPI
    :<|> AdminAddLeaguePlayerAPI

type API = (AuthProtect SessionIdCookie :> Protected) :<|> Unprotected
