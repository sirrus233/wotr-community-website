{-# LANGUAGE QuasiQuotes #-}

module Auth where

import Data.ByteString qualified as BS
import Data.List (lookup)
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
      acAuthorizeState = "CHANGE_THIS", -- TODO
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
