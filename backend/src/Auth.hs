{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Auth where

import Data.Aeson (eitherDecode)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Jose.Jwk (JwkSet)
import Network.HTTP.Conduit (Manager, Response (..), httpLbs, parseRequest)
import Network.Wai (Request (..))
import Servant (AuthProtect)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
   Admin
    email Text
    sessionId Text
    refreshToken Text
    UniqueAdminEmail email 
    UniqueAdminSessionId sessionId
    UniqueAdminRefreshToken refreshToken
    deriving Show
|]

data SessionIdCookie = SessionIdCookie -- Type-level tag for the auth scheme

type instance AuthServerData (AuthProtect SessionIdCookie) = AdminUser

newtype AdminUser = AdminUser {username :: Text} deriving (Eq, Show, Read, Generic)

fetchGoogleJWKSet :: Manager -> IO (Either String JwkSet)
fetchGoogleJWKSet mgr = do
  request <- parseRequest "https://www.googleapis.com/oauth2/v3/certs"
  response <- httpLbs request mgr
  pure $ eitherDecode (response.responseBody)

authHandler :: AuthHandler Request AdminUser
authHandler = mkAuthHandler $ \req -> do
  -- TODO
  pure $ AdminUser "TODO"

-- case lookup "Authorization" (requestHeaders req) >>= stripBearer of
--   Nothing -> throwError err401
--   Just token ->
--     liftIO (validateBearerToken token) >>= \case
--       Nothing -> throwError err403
--       Just userId -> pure $ AdminUser userId
