{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Auth where

import AppConfig (AppM)
import Crypto.JWT (JWKSet)
import Data.Aeson (eitherDecode)
import Data.ByteString qualified as BS
import Data.List (lookup)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Network.HTTP.Conduit (Manager, Response (..), httpLbs, parseRequest)
import Network.OAuth2.Experiment
  ( AuthorizationCodeApplication (..),
    AuthorizeState,
    ClientAuthenticationMethod (..),
    ClientSecret,
    Idp (..),
    IdpApplication,
  )
import Network.Wai (Request (..))
import Servant (err401, err403, throwError)
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import Types.Api (AdminUser (..))
import URI.ByteString.QQ (uri)

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

fetchGoogleJWKSet :: Manager -> IO (Either String JWKSet)
fetchGoogleJWKSet mgr = do
  request <- parseRequest "https://www.googleapis.com/oauth2/v3/certs"
  response <- httpLbs request mgr
  pure $ eitherDecode (response.responseBody)

data Google = Google deriving (Eq, Show)

type GoogleOAuth = IdpApplication Google AuthorizationCodeApplication

googleIdp :: Idp Google
googleIdp =
  Idp
    { idpAuthorizeEndpoint = [uri|https://accounts.google.com/o/oauth2/v2/auth|],
      idpTokenEndpoint = [uri|https://oauth2.googleapis.com/token|],
      idpUserInfoEndpoint = [uri|https://openidconnect.googleapis.com/v1/userinfo|],
      idpDeviceAuthorizationEndpoint = Just [uri|https://oauth2.googleapis.com/device/code|]
    }

googleOauthAppConfig :: ClientSecret -> AuthorizationCodeApplication
googleOauthAppConfig secret =
  AuthorizationCodeApplication
    { acClientId = "331114708951-rhdksfhejc8l5tif6qd3ofuj6uc2e4pg.apps.googleusercontent.com",
      acClientSecret = secret,
      acScope = fromList ["openid", "profile", "email"],
      acAuthorizeRequestExtraParams = fromList [("access_type", "offline")],
      acAuthorizeState = "",
      acRedirectUri = [uri|https://api.waroftheringcommunity.net:8080/auth/google/callback|],
      acName = "GoogleOAuthApp",
      acTokenRequestAuthenticationMethod = ClientSecretBasic
    }

withState :: AuthorizeState -> AuthorizationCodeApplication -> AuthorizationCodeApplication
withState authState oauth = oauth {acAuthorizeState = authState}

--- TODO BELOW THIS LINE ---

validateBearerToken :: Text -> IO (Maybe Text)
validateBearerToken token = do
  -- Here you might decode a JWT, verify signatures,
  -- or call an introspection endpoint.
  -- Return `Just userId` if valid, `Nothing` if not.
  pure $ Just token -- placeholder

stripBearer :: ByteString -> Maybe Text
stripBearer headerValue =
  if prefix `BS.isPrefixOf` headerValue
    then Just $ decodeUtf8 (BS.drop (BS.length prefix) headerValue)
    else Nothing
  where
    prefix = "Bearer "

authHandler :: AuthHandler Request AdminUser
authHandler = mkAuthHandler $ \req -> do
  case lookup "Authorization" (requestHeaders req) >>= stripBearer of
    Nothing -> throwError err401
    Just token ->
      liftIO (validateBearerToken token) >>= \case
        Nothing -> throwError err403
        Just userId -> pure $ AdminUser userId

-- import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
-- import Network.Wai (requestHeaders)
-- import Crypto.JWT (verifyJWT, JWTValidationSettings, defaultJWTValidationSettings, JWSError)

-- jwtAuthHandler :: JWK -> AuthHandler Request User
-- jwtAuthHandler signingKey = mkAuthHandler $ \req -> do
--   let maybeCookie = lookup "Cookie" (requestHeaders req)
--   case maybeCookie >>= lookupJWTInCookie of
--     Nothing    -> throwError err401 { errBody = "Missing JWT cookie" }
--     Just token -> do
--       user <- verifyJwtToken signingKey token
--       pure user

-- lookupJWTInCookie :: ByteString -> Maybe Text
-- lookupJWTInCookie cookieHeaderVal = do
--   let cookies = parseCookies cookieHeaderVal
--   tokenBS <- lookup "session_jwt" cookies
--   pure (decodeUtf8 tokenBS)

-- verifyJwtToken :: JWK -> Text -> Handler User
-- verifyJwtToken key tokenText = do
--   case decodeCompact (encodeUtf8 tokenText) of
--     Left _err -> throwError err401 { errBody = "Invalid token format" }
--     Right jwt -> do
--       let validationSettings = defaultJWTValidationSettings (const True)
--           -- you'd normally pass a 'StringOrURI' representing your 'aud' or something similar
--       res <- runExceptT (verifyJWT validationSettings key jwt)
--       case res of
--         Left jwserr -> throwError err401 { errBody = "Bad token: " <> fromString (show jwserr) }
--         Right claims -> do
--           -- parse claims, get 'sub' or 'email'
--           -- for example: let sub = preview (claimSub . _Just) claims
--           pure (User "alice" ... )  -- your custom user type
