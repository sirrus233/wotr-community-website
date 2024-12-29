module App where

import Control.Monad.Logger (LoggingT (runLoggingT), filterLogger)
import Database.Esqueleto.Experimental (SqlPersistT)
import Database.Esqueleto.Experimental qualified as SQL
import Database.Redis qualified as Redis
import Logging (LogLevelFilter, Logger, filterInfo)
import Servant (Handler)

data Env = Env {dbPool :: SQL.ConnectionPool, redisPool :: Redis.Connection, logger :: Logger}

type AppM = ReaderT Env (LoggingT Handler)

runAppLogger :: (MonadIO m) => LogLevelFilter -> Logger -> LoggingT m a -> m a
runAppLogger logFilter logger loggingAction = runLoggingT (filterLogger logFilter loggingAction) logger

runDb :: SqlPersistT (LoggingT IO) a -> AppM a
runDb dbAction = ask >>= \env -> liftIO . runAppLogger filterInfo env.logger . SQL.runSqlPool dbAction $ env.dbPool

nt :: Env -> AppM a -> Handler a
nt env server = runAppLogger filterInfo env.logger $ runReaderT server env
