module Main where

import Amazonka qualified as AWS
import Api (api)
import AppConfig (Env (..), databaseFile, logFile, nt, redisConfig, runAppLogger)
import AppServer (server)
import Control.Monad.Logger (LogLevel (..))
import Database.Esqueleto.Experimental (defaultConnectionPoolConfig)
import Database.Persist.Sqlite (createSqlitePoolWithConfig)
import Database.Redis (connect)
import Logging (fileLogger, log)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors)
import Network.Wai.Middleware.Gzip (defaultGzipSettings, gzip)
import Servant (Application, hoistServer)
import Servant.Server (serve)
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
        { corsOrigins = Nothing,
          corsMethods = ["HEAD", "GET", "POST"],
          corsRequestHeaders = ["content-type"],
          corsExposedHeaders = Nothing,
          corsMaxAge = Nothing,
          corsVaryOrigin = True,
          corsRequireOrigin = False,
          corsIgnoreFailures = True
        }

gzipMiddleware :: Middleware
gzipMiddleware = gzip defaultGzipSettings

app :: Env -> Application
app env = gzipMiddleware . corsMiddleware . serve api . hoistServer api (nt env) $ server

main :: IO ()
main = do
  args <- getArgs
  setEnv "AWS_PROFILE" "wotrcommunity"
  createDirectoryIfMissing True . takeDirectory $ databaseFile
  createDirectoryIfMissing True . takeDirectory $ logFile

  logger <- fileLogger logFile
  awsLogger <- AWS.newLogger AWS.Debug stdout -- TODO Replace Amazonka's logger with our real one
  dbPool <- runAppLogger logger $ createSqlitePoolWithConfig (toText databaseFile) defaultConnectionPoolConfig
  redisPool <- connect redisConfig
  aws <- AWS.newEnv AWS.discover >>= \awsEnv -> pure $ awsEnv {AWS.logger = awsLogger, AWS.region = AWS.Oregon}

  let env = Env {dbPool, redisPool, logger, aws}

  when ("migrate" `elem` args) $ migrateSchema dbPool logger

  log logger LevelInfo "Starting server."

  let certFile = "/etc/letsencrypt/live/api.waroftheringcommunity.net/fullchain.pem"
  let keyFile = "/etc/letsencrypt/live/api.waroftheringcommunity.net/privkey.pem"
  let tlsSettings_ = tlsSettings certFile keyFile
  let settings = setPort 8080 defaultSettings
  runTLS tlsSettings_ settings $ app env
