module Types.App where

import Data.Pool (Pool)
import Database.Redis qualified as Redis
import Database.SQLite.Simple qualified as SQL
import Servant (Handler)
import System.Log.FastLogger (LogStr, TimedFastLogger, ToLogStr (..))

data Env = Env {dbPool :: Pool SQL.Connection, redisPool :: Redis.Connection, logger :: TimedFastLogger}

type AppM = ReaderT Env Handler

nt :: Env -> AppM a -> Handler a
nt s x = runReaderT x s

log :: TimedFastLogger -> LogStr -> IO ()
log logger msg = logger (\time -> toLogStr time <> " " <> msg <> "\n")
