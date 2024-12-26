module Main where

import Api (api)
import App (Env (..), nt, runAppLogger)
import AppServer (server)
import Control.Monad.Logger (LogLevel (..), ToLogStr (toLogStr))
import Database.Esqueleto.Experimental (defaultConnectionPoolConfig, runMigrationQuiet, runSqlPool)
import Database.Persist.Sqlite (createSqlitePoolWithConfig)
import Database.Redis (ConnectInfo, connect, defaultConnectInfo)
import Logging (filterInfo, log, stdoutLogger)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors)
import Servant (Application, hoistServer)
import Servant.Server (serve)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Types.Database (migrateAll)

databaseFile :: FilePath
databaseFile = "data/db.sqlite"

redisConfig :: ConnectInfo
redisConfig = defaultConnectInfo

corsMiddleware :: Application -> Application
corsMiddleware = cors $ const $ Just policy
  where
    policy =
      CorsResourcePolicy
        { corsOrigins = Just (["http://127.0.0.1:3000"], True),
          corsMethods = ["POST"],
          corsRequestHeaders = ["content-type"],
          corsExposedHeaders = Nothing,
          corsMaxAge = Nothing,
          corsVaryOrigin = True,
          corsRequireOrigin = False,
          corsIgnoreFailures = True
        }

app :: Env -> Application
app env = corsMiddleware . serve api . hoistServer api (nt env) $ server

main :: IO ()
main = do
  createDirectoryIfMissing True . takeDirectory $ databaseFile

  let logFilter = filterInfo
  logger <- stdoutLogger
  dbPool <- runAppLogger logFilter logger $ createSqlitePoolWithConfig (toText databaseFile) defaultConnectionPoolConfig
  redisPool <- connect redisConfig

  let env = Env {dbPool, redisPool, logger}

  -- TODO Disable/handle auto-migration
  migrations <- runSqlPool (runMigrationQuiet migrateAll) dbPool
  unless (null migrations) (log logger LevelWarn "Database schema changed. Running migrations.")
  mapM_ (log logger LevelInfo . toLogStr) migrations

  log logger LevelInfo "Starting server"
  run 8081 $ app env
