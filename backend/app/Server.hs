module Main where

import Amazonka qualified as AWS
import Api (API)
import AppConfig (Env (..), databaseFile, logFile, maxGameLogSizeMB, nt, redisConfig, runAppLogger)
import AppServer (server)
import Control.Monad.Logger (LogLevel (..))
import Crypto.JWT (JWK)
import Database.Esqueleto.Experimental (defaultConnectionPoolConfig)
import Database.Persist.Sqlite (createSqlitePoolWithConfig)
import Database.Redis (connect)
import Logging (fileLogger, log)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors)
import Network.Wai.Middleware.Gzip (defaultGzipSettings, gzip)
import Network.Wai.Parse (defaultParseRequestBodyOptions, setMaxRequestFileSize)
import Servant (Application, Context (..), serveWithContextT)
import Servant.Auth.Server (Cookie, CookieSettings, JWTSettings, defaultCookieSettings, defaultJWTSettings, generateKey)
import Servant.Multipart (MultipartOptions (..), Tmp, defaultMultipartOptions)
import System.Directory (createDirectoryIfMissing)
import System.Environment (setEnv)
import System.FilePath (takeDirectory)
import Types.Database (migrateSchema)

type Middleware = Application -> Application

type ServerContext = '[MultipartOptions Tmp, CookieSettings, JWTSettings]

corsMiddleware :: Middleware
corsMiddleware = cors $ const $ Just policy
  where
    policy =
      CorsResourcePolicy
        { corsOrigins = Nothing,
          corsMethods = ["HEAD", "GET", "POST"],
          corsRequestHeaders = ["content-type"],
          corsExposedHeaders = Nothing,
          corsMaxAge = Nothing,
          corsVaryOrigin = True,
          corsRequireOrigin = False,
          corsIgnoreFailures = True
        }

gzipMiddleware :: Middleware
gzipMiddleware = gzip defaultGzipSettings

app :: Env -> JWK -> Application
app env key =
  gzipMiddleware . corsMiddleware $
    serveWithContextT (Proxy :: Proxy (API '[Cookie])) context (nt env) (server cookieCfg jwtCfg)
  where
    jwtCfg = defaultJWTSettings key
    cookieCfg = defaultCookieSettings
    maxSizeBytes = maxGameLogSizeMB * 1024 * 1024
    multipartOpts :: MultipartOptions Tmp
    multipartOpts =
      (defaultMultipartOptions (Proxy :: Proxy Tmp))
        { generalOptions = setMaxRequestFileSize maxSizeBytes defaultParseRequestBodyOptions
        }
    context = multipartOpts :. defaultCookieSettings :. defaultJWTSettings key :. EmptyContext

main :: IO ()
main = do
  args <- getArgs
  setEnv "AWS_PROFILE" "wotrcommunity"
  createDirectoryIfMissing True . takeDirectory $ databaseFile
  createDirectoryIfMissing True . takeDirectory $ logFile

  logger <- fileLogger logFile
  awsLogger <- AWS.newLogger AWS.Debug stdout -- TODO Replace Amazonka's logger with our real one
  dbPool <- runAppLogger logger $ createSqlitePoolWithConfig (toText databaseFile) defaultConnectionPoolConfig
  redisPool <- connect redisConfig
  aws <- AWS.newEnv AWS.discover >>= \awsEnv -> pure $ awsEnv {AWS.logger = awsLogger, AWS.region = AWS.Oregon}

  let env = Env {dbPool, redisPool, logger, aws}

  when ("migrate" `elem` args) $ migrateSchema dbPool logger

  log logger LevelInfo "Starting server."

  let certFile = "/etc/letsencrypt/live/api.waroftheringcommunity.net/fullchain.pem"
  let keyFile = "/etc/letsencrypt/live/api.waroftheringcommunity.net/privkey.pem"
  let tlsSettings_ = tlsSettings certFile keyFile
  let settings = setPort 8080 defaultSettings

  generateKey >>= runTLS tlsSettings_ settings . app env
