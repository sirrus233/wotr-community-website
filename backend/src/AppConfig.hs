module AppConfig where

import Control.Monad.Logger (LoggingT (runLoggingT), filterLogger)
import Database.Esqueleto.Experimental qualified as SQL
import Database.Redis (ConnectInfo, defaultConnectInfo)
import Database.Redis qualified as Redis
import Logging (LogLevelFilter, Logger, filterInfo)
import Servant (Handler)

data Env = Env {dbPool :: SQL.ConnectionPool, redisPool :: Redis.Connection, logger :: Logger}

type AppM = ReaderT Env (LoggingT Handler)

databaseFile :: FilePath
databaseFile = "data/db.sqlite"

logFile :: FilePath
logFile = "logs/app.log"

redisConfig :: ConnectInfo
redisConfig = defaultConnectInfo

logFilter :: LogLevelFilter
logFilter = filterInfo

runAppLogger :: (MonadIO m) => Logger -> LoggingT m a -> m a
runAppLogger logger loggingAction = runLoggingT (filterLogger logFilter loggingAction) logger

nt :: Env -> AppM a -> Handler a
nt env server = runAppLogger env.logger $ runReaderT server env
