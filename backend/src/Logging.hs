module Logging where

import Amazonka qualified as AWS
import Control.Monad.Logger (Loc, LogLevel (..), LogSource, LogStr, defaultLoc)
import Network.Wai (Request)
import System.Log.FastLogger
  ( FormattedTime,
    LogType' (..),
    ToLogStr (..),
    defaultBufSize,
    newTimeCache,
    newTimedFastLogger,
    simpleTimeFormat,
  )

type Logger = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

type LogLevelFilter = LogSource -> LogLevel -> Bool

data LoggableException = LoggableException
  { request :: Maybe Request,
    exception :: SomeException
  }
  deriving (Show)

formatLogMsg :: FormattedTime -> LogLevel -> LogStr -> LogStr
formatLogMsg time level msg = "[" <> levelStr level <> "] " <> toLogStr time <> ": " <> msg <> "\n"
  where
    levelStr :: LogLevel -> LogStr
    levelStr LevelDebug = "DEBUG"
    levelStr LevelInfo = "INFO"
    levelStr LevelWarn = "WARN"
    levelStr LevelError = "ERROR"
    levelStr (LevelOther l) = toLogStr l

filterDebug :: LogLevelFilter
filterDebug _ _ = True

filterInfo :: LogLevelFilter
filterInfo _ level
  | level == LevelDebug = False
  | otherwise = True

stdoutLogger :: IO Logger
stdoutLogger = do
  timeCache <- newTimeCache simpleTimeFormat
  (logger, _) <- newTimedFastLogger timeCache (LogStdout defaultBufSize)
  pure $ \_ _ level msg -> logger (\time -> formatLogMsg time level msg)

fileLogger :: FilePath -> IO Logger
fileLogger path = do
  timeCache <- newTimeCache simpleTimeFormat
  (logger, _) <- newTimedFastLogger timeCache (LogFileNoRotate path defaultBufSize)
  pure $ \_ _ level msg -> logger (\time -> formatLogMsg time level msg)

fromAwsLogLevel :: AWS.LogLevel -> LogLevel
fromAwsLogLevel AWS.Trace = LevelDebug
fromAwsLogLevel AWS.Debug = LevelDebug
fromAwsLogLevel AWS.Info = LevelInfo
fromAwsLogLevel AWS.Error = LevelError

toAwsLogger :: LogLevelFilter -> Logger -> AWS.Logger
toAwsLogger logFilter logger awsLevel builder = when (logFilter "" logLevel) (log logger logLevel (toLogStr builder))
  where
    logLevel = fromAwsLogLevel awsLevel

log :: Logger -> LogLevel -> LogStr -> IO ()
log logger = logger defaultLoc ""

logException :: Logger -> Maybe Request -> SomeException -> IO ()
logException logger req exc = log logger LevelError . show $ LoggableException req exc

(<>:) :: (Semigroup a, IsString a, Show b) => a -> b -> a
(<>:) a b = a <> show b
