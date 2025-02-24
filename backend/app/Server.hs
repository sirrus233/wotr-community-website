module Main where

import Amazonka qualified as AWS
import Api (API)
import AppConfig (Env (..), authDatabaseFile, databaseFile, logFile, maxGameLogSizeMB, nt, redisConfig, runAppLogger)
import AppServer (server)
import Auth (authHandler)
import Control.Monad.Logger (LogLevel (..))
import Database.Esqueleto.Experimental (defaultConnectionPoolConfig)
import Database.Persist.Sqlite (createSqlitePoolWithConfig)
import Database.Redis (connect)
import Logging (fileLogger, log, logException, stdoutLogger)
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

corsMiddleware :: Middleware
corsMiddleware = cors $ const $ Just policy
  where
    policy =
      CorsResourcePolicy
        { corsOrigins = Just (["https://waroftheringcommunity.net"], True),
          corsMethods = [],
          corsRequestHeaders = ["Content-Type"],
          corsExposedHeaders = Just ["Content-Disposition"],
          corsMaxAge = Nothing, -- TODO Set to 3600 or 7200 prior to launch
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

app :: Env -> Application
app env = gzipMiddleware . corsMiddleware $ serveWithContextT (Proxy :: Proxy API) context (nt env) server
  where
    context = authHandler env :. multipartOpts :. EmptyContext

runDev :: Env -> Settings -> IO ()
runDev env settings = runSettings settings . app $ env

runProd :: Env -> Settings -> IO ()
runProd env settings = do
  let certFile = "/etc/letsencrypt/live/api.waroftheringcommunity.net/fullchain.pem"
  let keyFile = "/etc/letsencrypt/live/api.waroftheringcommunity.net/privkey.pem"
  let tlsSettings_ = tlsSettings certFile keyFile
  runTLS tlsSettings_ settings . app $ env

main :: IO ()
main = do
  args <- getArgs
  let isDev = "dev" `elem` args
  let doMigrate = "migrate" `elem` args

  setEnv "AWS_PROFILE" "wotrcommunity"
  createDirectoryIfMissing True . takeDirectory $ databaseFile
  createDirectoryIfMissing True . takeDirectory $ logFile

  logger <- if isDev then stdoutLogger else fileLogger logFile
  awsLogger <- AWS.newLogger AWS.Debug stdout -- TODO Replace Amazonka's logger with our real one
  dbPool <- runAppLogger logger $ createSqlitePoolWithConfig (toText databaseFile) defaultConnectionPoolConfig
  authDbPool <- runAppLogger logger $ createSqlitePoolWithConfig (toText authDatabaseFile) defaultConnectionPoolConfig
  redisPool <- connect redisConfig
  aws <- AWS.newEnv AWS.discover >>= \awsEnv -> pure $ awsEnv {AWS.logger = awsLogger, AWS.region = AWS.Oregon}

  when doMigrate $ migrateSchema dbPool logger

  let env = Env {dbPool, authDbPool, redisPool, logger, aws}
  let settings = setPort 8080 . setOnException (logException env.logger) $ defaultSettings
  let runServer = if isDev then runDev else runProd

  log logger LevelInfo "Starting server."
  runServer env settings
