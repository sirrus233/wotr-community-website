module Main where

import Api (api)
import AppServer (server)
import Data.Pool (defaultPoolConfig, destroyAllResources, newPool)
import Database (initializeDatabase)
import Database.Redis (ConnectInfo, connect, defaultConnectInfo, disconnect)
import Database.SQLite.Simple (close, open)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors)
import Servant (Application, hoistServer)
import Servant.Server (serve)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.Log.FastLogger (LogType, LogType' (..), defaultBufSize, newTimeCache, newTimedFastLogger, simpleTimeFormat)
import Types.App (Env (..), log, nt)

databaseFile :: FilePath
databaseFile = "data/db.sqlite"

redisConfig :: ConnectInfo
redisConfig = defaultConnectInfo

logType :: LogType
logType = LogStdout defaultBufSize

corsMiddleware :: Application -> Application
corsMiddleware = cors . const $ Just policy
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

  dbPool <- newPool $ defaultPoolConfig (open databaseFile) close 30 10
  redisPool <- connect redisConfig
  timeCache <- newTimeCache simpleTimeFormat
  (logger, rmLogger) <- newTimedFastLogger timeCache logType

  let env = Env {dbPool, redisPool, logger}

  runReaderT initializeDatabase env

  log logger "Starting server"
  run 8081 . app $ env

  log logger "Shutting down server"
  destroyAllResources dbPool
  disconnect redisPool
  rmLogger
