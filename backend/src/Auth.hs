module Auth where

import AppConfig (AppM, Env, authCookieName, googleClientId, nt)
import Crypto.JWT (JWTError (JWTExpired, JWTNotInAudience, JWTNotInIssuer))
import Crypto.Random (getSystemDRG, withDRG)
import Data.Aeson (eitherDecode)
import Data.List (lookup)
import Data.Text (isSuffixOf)
import Data.Time.Clock.POSIX (getCurrentTime, posixSecondsToUTCTime)
import Database (getAdminBySessionId, runAuthDb)
import Database.Esqueleto.Experimental (Entity (..))
import Jose.Jwa (JwsAlg (RS256))
import Jose.Jwk (JwkSet (..))
import Jose.Jwt (JwtContent (..), JwtEncoding (..), JwtError, decode)
import Network.HTTP.Conduit (Manager, Response (..), httpLbs, parseRequest)
import Network.Wai (Request (..))
import Servant (ServerError (errBody), err401, throwError)
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import Types.Api (IdToken (..))
import Types.Auth (Authenticated (..), GoogleIdTokenClaims (..), SessionId (..), UserId (..))
import Types.Database (Admin (..))
import Web.Cookie (parseCookiesText)

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

validateToken :: JwkSet -> IdToken -> IO (Either TokenValidationError UserId)
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
          -- Kinda bad, as a user may have multiple or zero email addresses. Per Google's recommendation,
          -- we should be using `sub` as the canonical unique ID. But email works for our small use-case for now.
          pure $ validAud *> validIss *> validExp now *> googleAuthoritative *> (Right . UserId . email) $ claims
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

authHandlerProd :: Env -> AuthHandler Request Authenticated
authHandlerProd env = mkAuthHandler (nt env . handler)
  where
    handler :: Request -> AppM Authenticated
    handler req = do
      case parseCookiesText <$> lookup "Cookie" (requestHeaders req) of
        Nothing -> throwError err401 {errBody = "Missing Cookie header."}
        Just cookies -> do
          case lookup authCookieName cookies of
            Nothing -> throwError err401 {errBody = "Auth cookie not found."}
            Just sessionId -> do
              runAuthDb (getAdminBySessionId sessionId) >>= \case
                Nothing -> throwError err401 {errBody = "Invalid session."}
                Just (Entity _ admin) ->
                  pure $ Authenticated {userId = UserId admin.adminUserId, sessionId = SessionId sessionId}

authHandlerDev :: Env -> AuthHandler Request Authenticated
authHandlerDev env = mkAuthHandler (nt env . handler)
  where
    handler :: Request -> AppM Authenticated
    handler _ = pure Authenticated {userId = UserId "Frodo", sessionId = SessionId "Baggins"}
