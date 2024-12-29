module Main where

import Api (api)
import AppConfig (Env (..), databaseFile, logFile, nt, redisConfig, runAppLogger)
import AppServer (server)
import Control.Monad.Logger (LogLevel (..), ToLogStr (toLogStr))
import Database.Esqueleto.Experimental (defaultConnectionPoolConfig, runMigrationQuiet, runSqlPool)
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
import System.FilePath (takeDirectory)
import Types.Database (migrateAll)

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
  createDirectoryIfMissing True . takeDirectory $ databaseFile
  createDirectoryIfMissing True . takeDirectory $ logFile

  logger <- fileLogger logFile
  dbPool <- runAppLogger logger $ createSqlitePoolWithConfig (toText databaseFile) defaultConnectionPoolConfig
  redisPool <- connect redisConfig

  let env = Env {dbPool, redisPool, logger}

  -- TODO Disable/handle auto-migration
  migrations <- runSqlPool (runMigrationQuiet migrateAll) dbPool
  unless (null migrations) (log logger LevelWarn "Database schema changed. Running migrations.")
  mapM_ (log logger LevelDebug . toLogStr) migrations

  log logger LevelInfo "Starting server."

  let certFile = "/etc/letsencrypt/live/api.waroftheringcommunity.net/fullchain.pem"
  let keyFile = "/etc/letsencrypt/live/api.waroftheringcommunity.net/privkey.pem"
  let tlsSettings_ = tlsSettings certFile keyFile
  let settings = setPort 8080 defaultSettings
  runTLS tlsSettings_ settings $ app env
