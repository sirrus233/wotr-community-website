module Types.App where

import Database.Esqueleto.Experimental (SqlPersistT)
import Database.Esqueleto.Experimental qualified as SQL
import Database.Redis qualified as Redis
import Servant (Handler)
import System.Log.FastLogger (LogStr, TimedFastLogger, ToLogStr (..))

data Env = Env {dbPool :: SQL.ConnectionPool, redisPool :: Redis.Connection, logger :: TimedFastLogger}

type AppM = ReaderT Env Handler

nt :: Env -> AppM a -> Handler a
nt env server = runReaderT server env

runDb :: SqlPersistT IO a -> AppM a
runDb dbAction = asks dbPool >>= liftIO . SQL.runSqlPool dbAction

-- TODO Run in handler w/o explicit logger pass?
log :: TimedFastLogger -> LogStr -> IO ()
log logger msg = logger (\time -> toLogStr time <> " " <> msg <> "\n")
