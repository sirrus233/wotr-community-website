{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Auth where

import AppConfig (googleClientId)
import Crypto.JWT (JWTError (JWTExpired, JWTNotInAudience, JWTNotInIssuer))
import Crypto.Random (getSystemDRG, withDRG)
import Data.Aeson (FromJSON, eitherDecode)
import Data.Text (isSuffixOf)
import Data.Time.Clock.POSIX (getCurrentTime, posixSecondsToUTCTime)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Jose.Jwa (JwsAlg (RS256))
import Jose.Jwk (JwkSet (..))
import Jose.Jwt (JwtContent (..), JwtEncoding (..), JwtError, decode)
import Network.HTTP.Conduit (Manager, Response (..), httpLbs, parseRequest)
import Network.Wai (Request (..))
import Servant (AuthProtect)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import Types.Api (IdToken (..))

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

newtype AdminUser = AdminUser {userId :: Text} deriving (Eq, Show, Read, Generic)

data GoogleIdTokenClaims = GoogleIdTokenClaims
  { iss :: Text,
    sub :: Text,
    aud :: Text,
    hd :: Maybe Text,
    exp :: Integer,
    iat :: Integer,
    email :: Text,
    email_verified :: Bool
  }
  deriving (Generic)

instance FromJSON GoogleIdTokenClaims

fetchGoogleJWKSet :: Manager -> IO (Either String JwkSet)
fetchGoogleJWKSet mgr = do
  request <- parseRequest "https://www.googleapis.com/oauth2/v3/certs"
  response <- httpLbs request mgr
  pure $ eitherDecode (response.responseBody)

data TokenValidationError
  = JwtDecodeError JwtError
  | UnexpectedJwtTypeError
  | JwtDecodeClaimsError Text
  | JwtInvalidClaimsError JWTError
  | GoogleNotAuthoritativeError
  deriving (Show)

validateToken :: JwkSet -> IdToken -> IO (Either TokenValidationError AdminUser)
validateToken (JwkSet keys) (IdToken token) = do
  drg <- getSystemDRG
  let (tokenDecodeResult, _) = withDRG drg (decode keys (Just (JwsEncoding RS256)) (encodeUtf8 token))
  case tokenDecodeResult of
    Left err -> pure $ Left (JwtDecodeError err)
    Right (Unsecured _) -> pure $ Left UnexpectedJwtTypeError
    Right (Jwe _) -> pure $ Left UnexpectedJwtTypeError
    Right (Jws (_, content)) -> do
      case eitherDecode . fromStrict $ content of
        Left err -> pure $ Left (JwtDecodeClaimsError $ toText err)
        Right claims -> do
          now <- getCurrentTime
          pure $ validAud *> validIss *> validExp now *> googleAuthoritative *> (Right . AdminUser . sub) $ claims
  where
    -- See: https://developers.google.com/identity/gsi/web/guides/verify-google-id-token
    validAud claims
      | claims.aud == googleClientId = Right ()
      | otherwise = Left $ JwtInvalidClaimsError JWTNotInAudience

    validIss claims
      | claims.iss == "accounts.google.com" = Right ()
      | claims.iss == "https://accounts.google.com" = Right ()
      | otherwise = Left $ JwtInvalidClaimsError JWTNotInIssuer

    validExp now claims
      | now < (posixSecondsToUTCTime . fromIntegral $ claims.exp) = Right ()
      | otherwise = Left $ JwtInvalidClaimsError JWTExpired

    googleAuthoritative claims
      | "@gmail.com" `isSuffixOf` claims.email = Right ()
      | claims.email_verified && isJust claims.hd = Right ()
      | otherwise = Left GoogleNotAuthoritativeError

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
