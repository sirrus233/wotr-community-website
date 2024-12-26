module Logging where

import Control.Monad.Logger (Loc, LogLevel (..), LogSource, LogStr, defaultLoc)
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

log :: Logger -> LogLevel -> LogStr -> IO ()
log logger = logger defaultLoc ""

(<>:) :: (Semigroup a, IsString a, Show b) => a -> b -> a
(<>:) a b = a <> show b