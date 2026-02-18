import Api qualified
import Control.Lens ((^.))
import Data.Char (toUpper)
import Data.Text qualified as T
import Servant ((:<|>))
import Servant.Foreign (FunctionName (..), Req, reqFuncName)
import Servant.TypeScript
  ( ServantTypeScriptOptions (..),
    defaultServantTypeScriptOptions,
    writeTypeScriptLibrary',
  )

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
main = writeTypeScriptLibrary' tsOptions (Proxy :: Proxy TypeScriptableAPI) "../frontend/src/generated/"

tsOptions :: ServantTypeScriptOptions
tsOptions =
  defaultServantTypeScriptOptions
    { getFunctionName = prettyFunctionName
    }

prettyFunctionName :: Req T.Text -> T.Text
prettyFunctionName req =
  case unFunctionName (req ^. reqFuncName) of
    [] -> error "Unexpected empty function name components"
    method : segments ->
      T.concat (method : fmap (capitalize . camelFromSnake) segments)
  where
    camelFromSnake :: T.Text -> T.Text
    camelFromSnake =
      T.concat . fmap capitalize . T.splitOn "_"

    capitalize :: T.Text -> T.Text
    capitalize t =
      case T.uncons t of
        Nothing -> t
        Just (h, rest) -> T.cons (toUpper h) rest
