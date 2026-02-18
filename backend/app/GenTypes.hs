import Api qualified
import Servant ((:<|>))
import Servant.TypeScript (writeTypeScriptLibrary)

type TypeScriptableAPI =
  Api.UserInfoAPI
    :<|> Api.SubmitReportAPI
    :<|> Api.GetReportsAPI
    :<|> Api.GetLeaderboardAPI
    :<|> Api.GetLeagueStatsAPI
    :<|> Api.UpdateActiveStatusAPI
    :<|> Api.LogoutAPI
    :<|> Api.AdminEditPlayerAPI
    :<|> Api.AdminRemapPlayerAPI
    :<|> Api.AdminModifyReportAPI
    :<|> Api.AdminDeleteReportAPI
    :<|> Api.AdminAddLeaguePlayerAPI

main :: IO ()
main = writeTypeScriptLibrary (Proxy :: Proxy TypeScriptableAPI) "../frontend/src/generated/"
