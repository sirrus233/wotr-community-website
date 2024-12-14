module Main where

import Api (api)
import AppServer (server)
import Data.Pool (defaultPoolConfig, destroyAllResources, newPool)
import Database (initializeDatabase)
import Database.Redis (ConnectInfo, connect, defaultConnectInfo, disconnect)
import Database.SQLite.Simple (close, open)
import Network.Wai.Handler.Warp (run)
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

app :: Env -> Application
app env = serve api . hoistServer api (nt env) $ server

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
