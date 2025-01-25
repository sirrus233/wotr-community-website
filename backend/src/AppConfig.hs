module AppConfig where

import Amazonka qualified as AWS
import Amazonka.S3 qualified as S3
import Control.Monad.Logger (LoggingT (runLoggingT), filterLogger)
import Database.Esqueleto.Experimental qualified as SQL
import Database.Redis (ConnectInfo, defaultConnectInfo)
import Database.Redis qualified as Redis
import Logging (LogLevelFilter, Logger, filterInfo)
import Servant (Handler)

data Env = Env
  { dbPool :: SQL.ConnectionPool,
    authDbPool :: SQL.ConnectionPool, -- TODO Remove this if we can't get at it in the Auth handler
    redisPool :: Redis.Connection,
    logger :: Logger,
    aws :: AWS.Env
  }

type AppM = ReaderT Env (LoggingT Handler)

databaseFile :: FilePath
databaseFile = "data/db.sqlite"

authDatabaseFile :: FilePath
authDatabaseFile = "data/auth-db.sqlite"

googleClientId :: Text
googleClientId = "331114708951-rhdksfhejc8l5tif6qd3ofuj6uc2e4pg.apps.googleusercontent.com"

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
