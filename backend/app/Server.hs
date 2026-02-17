module Main where

import Amazonka qualified as AWS
import Amazonka.Auth (AccessKey (..), SecretKey (..), fromKeys)
import Api (API)
import AppConfig (Env (..), authDatabaseFile, databaseFile, logFile, logFilter, maxGameLogSizeMB, nt, redisConfig, runAppLogger)
import AppServer (server)
import Auth (authServiceHandler, authUserHandlerDev, authUserHandlerProd)
import Control.Monad.Logger (LogLevel (..))
import Database.Esqueleto.Experimental (defaultConnectionPoolConfig)
import Database.Persist.Sqlite (createSqlitePoolWithConfig)
import Database.Redis (connect)
import Logging (fileLogger, log, logException, stdoutLogger, toAwsLogger)
import Network.Wai.Handler.Warp (Settings, defaultSettings, runSettings, setOnException, setPort)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors)
import Network.Wai.Middleware.Gzip (defaultGzipSettings, gzip)
import Network.Wai.Parse (defaultParseRequestBodyOptions, setMaxRequestFileSize)
import Servant (Application, Context (..), serveWithContextT)
import Servant.Multipart (MultipartOptions (..), Tmp, defaultMultipartOptions)
import System.Directory (createDirectoryIfMissing)
import System.Environment (setEnv)
import System.FilePath (takeDirectory)
import Types.Database (migrateSchema)

type Middleware = Application -> Application

data AppMode = Dev | Prod

corsMiddleware :: AppMode -> Middleware
corsMiddleware mode = cors $ const $ Just policy
  where
    origins = case mode of
      Dev -> ["http://localhost:3000"]
      Prod -> ["https://waroftheringcommunity.net"]
    maxAge = case mode of
      Dev -> Nothing
      Prod -> Nothing -- TODO Set to 3600 or 7200 prior to launch
    policy =
      CorsResourcePolicy
        { corsOrigins = Just (origins, True),
          corsMethods = [],
          corsRequestHeaders = ["Content-Type"],
          corsExposedHeaders = Just ["Content-Disposition"],
          corsMaxAge = maxAge,
          corsVaryOrigin = False,
          corsRequireOrigin = True,
          corsIgnoreFailures = False
        }

gzipMiddleware :: Middleware
gzipMiddleware = gzip defaultGzipSettings

multipartOpts :: MultipartOptions Tmp
multipartOpts =
  (defaultMultipartOptions (Proxy :: Proxy Tmp))
    { generalOptions = setMaxRequestFileSize maxSizeBytes defaultParseRequestBodyOptions
    }
  where
    maxSizeBytes = maxGameLogSizeMB * 1024 * 1024

app :: AppMode -> Env -> Application
app mode env = gzipMiddleware . corsMiddleware mode $ serveWithContextT (Proxy :: Proxy API) context (nt env) server
  where
    context =
      (\case Dev -> authUserHandlerDev; Prod -> authUserHandlerProd) mode env
        :. authServiceHandler env
        :. multipartOpts
        :. EmptyContext

runDev :: Env -> Settings -> IO ()
runDev env settings = runSettings settings . app Dev $ env

runProd :: Env -> Settings -> IO ()
runProd env settings = do
  let certFile = "/etc/letsencrypt/live/api.waroftheringcommunity.net/fullchain.pem"
  let keyFile = "/etc/letsencrypt/live/api.waroftheringcommunity.net/privkey.pem"
  let tlsSettings_ = tlsSettings certFile keyFile
  runTLS tlsSettings_ settings . app Prod $ env

main :: IO ()
main = do
  args <- getArgs
  let appMode = if "dev" `elem` args then Dev else Prod
  let doMigrate = "migrate" `elem` args

  setEnv "AWS_PROFILE" "wotrcommunity"
  createDirectoryIfMissing True . takeDirectory $ databaseFile
  createDirectoryIfMissing True . takeDirectory $ logFile

  logger <- (\case Dev -> stdoutLogger; Prod -> fileLogger logFile) appMode
  dbPool <- runAppLogger logger $ createSqlitePoolWithConfig (toText databaseFile) defaultConnectionPoolConfig
  authDbPool <- runAppLogger logger $ createSqlitePoolWithConfig (toText authDatabaseFile) defaultConnectionPoolConfig
  redisPool <- connect redisConfig
  aws <- case appMode of
    Dev -> fromKeys (AccessKey "ABC") (SecretKey "123") <$> AWS.newEnvNoAuth
    Prod -> AWS.newEnv AWS.discover >>= \awsEnv -> pure $ awsEnv {AWS.logger = toAwsLogger logFilter logger, AWS.region = AWS.Oregon}
  apiSecret <- fmap encodeUtf8 <$> lookupEnv "SERVICE_API_SECRET"

  when doMigrate $ migrateSchema dbPool logger

  let env = Env {dbPool, authDbPool, redisPool, logger, aws, apiSecret}
  let settings = setPort 8080 . setOnException (logException env.logger) $ defaultSettings
  let runServer = (\case Dev -> runDev; Prod -> runProd) appMode

  log logger LevelInfo "Starting server."
  runServer env settings
