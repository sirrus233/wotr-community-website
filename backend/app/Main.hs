module Main where

import Api (api)
import AppServer (server)
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Esqueleto.Experimental (defaultConnectionPoolConfig, runMigration, runSqlPool)
import Database.Persist.Sqlite (createSqlitePoolWithConfig)
import Database.Redis (ConnectInfo, connect, defaultConnectInfo)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors)
import Servant (Application, hoistServer)
import Servant.Server (serve)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.Log.FastLogger (LogType, LogType' (..), defaultBufSize, newTimeCache, newTimedFastLogger, simpleTimeFormat)
import Types.App (Env (..), log, nt)
import Types.Database (migrateAll)

databaseFile :: FilePath
databaseFile = "data/db.sqlite"

redisConfig :: ConnectInfo
redisConfig = defaultConnectInfo

logType :: LogType
logType = LogStdout defaultBufSize

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

  dbPool <- runStdoutLoggingT $ createSqlitePoolWithConfig (toText databaseFile) defaultConnectionPoolConfig
  redisPool <- connect redisConfig
  timeCache <- newTimeCache simpleTimeFormat
  (logger, _) <- newTimedFastLogger timeCache logType

  let env = Env {dbPool, redisPool, logger}

  runSqlPool (runMigration migrateAll) dbPool

  log logger "Starting server"
  run 8081 . app $ env
