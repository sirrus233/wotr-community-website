module Types.App where

import Control.Monad.Logger (Loc, LogLevel (..), LogSource, LoggingT (runLoggingT), defaultLoc)
import Database.Esqueleto.Experimental (SqlPersistT)
import Database.Esqueleto.Experimental qualified as SQL
import Database.Redis qualified as Redis
import Servant (Handler)
import System.Log.FastLogger (LogStr, TimedFastLogger, ToLogStr (..))

data Env = Env {dbPool :: SQL.ConnectionPool, redisPool :: Redis.Connection, logger :: TimedFastLogger}

type AppM = ReaderT Env (LoggingT Handler)

log :: TimedFastLogger -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
log logger _ _ level msg = logger (\time -> "[" <> levelStr level <> "] " <> toLogStr time <> ": " <> msg <> "\n")
  where
    levelStr :: LogLevel -> LogStr
    levelStr LevelDebug = "DEBUG"
    levelStr LevelInfo = "INFO"
    levelStr LevelWarn = "WARN"
    levelStr LevelError = "ERROR"
    levelStr (LevelOther l) = toLogStr l

log' :: TimedFastLogger -> LogLevel -> LogStr -> IO ()
log' logger = log logger defaultLoc ""

runAppLogger :: (MonadIO m) => TimedFastLogger -> LoggingT m a -> m a
runAppLogger logger loggingAction = runLoggingT loggingAction (log logger)

nt :: Env -> TimedFastLogger -> AppM a -> Handler a
nt env logger server = runAppLogger logger $ runReaderT server env

runDb :: SqlPersistT (LoggingT IO) a -> AppM a
runDb dbAction = ask >>= \env -> liftIO . runAppLogger env.logger . SQL.runSqlPool dbAction $ env.dbPool
