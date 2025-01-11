module AppConfig where

import Amazonka qualified as AWS
import Amazonka.S3 qualified as S3
import Auth (GoogleOAuth)
import Control.Monad.Logger (LoggingT (runLoggingT), filterLogger)
import Database.Esqueleto.Experimental qualified as SQL
import Database.Redis (ConnectInfo, defaultConnectInfo)
import Database.Redis qualified as Redis
import Logging (LogLevelFilter, Logger, filterInfo)
import Servant (Handler)

data Env = Env
  { dbPool :: SQL.ConnectionPool,
    redisPool :: Redis.Connection,
    logger :: Logger,
    aws :: AWS.Env,
    googleOAuth :: GoogleOAuth
  }

type AppM = ReaderT Env (LoggingT Handler)

databaseFile :: FilePath
databaseFile = "data/db.sqlite"

logFile :: FilePath
logFile = "logs/app.log"

maxGameLogSizeMB :: Int64
maxGameLogSizeMB = 1

gameLogBucket :: S3.BucketName
gameLogBucket = "infrastructurestack-gamereportbucket21a257d2-v7okzv3x39a9"

redisConfig :: ConnectInfo
redisConfig = defaultConnectInfo

logFilter :: LogLevelFilter
logFilter = filterInfo

runAppLogger :: (MonadIO m) => Logger -> LoggingT m a -> m a
runAppLogger logger loggingAction = runLoggingT (filterLogger logFilter loggingAction) logger

nt :: Env -> AppM a -> Handler a
nt env server = runAppLogger env.logger $ runReaderT server env
