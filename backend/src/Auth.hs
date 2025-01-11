{-# LANGUAGE QuasiQuotes #-}

module Auth where

import Data.ByteString qualified as BS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Network.OAuth2.Experiment
  ( AuthorizationCodeApplication (..),
    ClientAuthenticationMethod (..),
    Idp (..),
    IdpApplication,
  )
import Network.Wai (Request (..))
import Servant (err401, err403, throwError)
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import Types.Api (AdminUser (..))
import URI.ByteString.QQ (uri)

data Google = Google deriving (Eq, Show)

type GoogleOAuth = IdpApplication AuthorizationCodeApplication Google

googleIdp :: Idp Google
googleIdp =
  Idp
    { idpAuthorizeEndpoint = [uri|https://accounts.google.com/o/oauth2/v2/auth|],
      idpTokenEndpoint = [uri|https://oauth2.googleapis.com/token|],
      idpUserInfoEndpoint = [uri|https://www.googleapis.com/oauth2/v2/userinfo|],
      idpDeviceAuthorizationEndpoint = Just [uri|https://oauth2.googleapis.com/device/code|]
    }

googleOauthAppConfig :: AuthorizationCodeApplication
googleOauthAppConfig =
  AuthorizationCodeApplication
    { acClientId = "xxxxx",
      acClientSecret = "xxxxx",
      acScope =
        fromList
          [ "https://www.googleapis.com/auth/userinfo.email",
            "https://www.googleapis.com/auth/userinfo.profile"
          ],
      acAuthorizeState = "CHANGE_ME",
      acAuthorizeRequestExtraParams = Map.empty,
      acRedirectUri = [uri|http://localhost/oauth2/callback|],
      acName = "sample-google-authorization-code-app",
      acTokenRequestAuthenticationMethod = ClientSecretBasic
    }

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
