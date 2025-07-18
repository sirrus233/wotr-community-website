module Types.Auth where

import Data.Aeson (FromJSON)
import Servant (AuthProtect)
import Servant.Server.Experimental.Auth (AuthServerData)

data SessionIdCookie -- Type-level tag for auth requiring a user session cookie

type instance AuthServerData (AuthProtect SessionIdCookie) = AuthenticatedUser

data ServiceCaller -- Type-level tag for auth requiring a header with our service's API secret

type instance AuthServerData (AuthProtect ServiceCaller) = AuthenticatedService

newtype UserId = UserId {unUserId :: Text}

newtype SessionId = SessionId {unSessionId :: Text}

data AuthenticatedUser = AuthenticatedUser {userId :: UserId, sessionId :: SessionId}

data AuthenticatedService = AuthenticatedService

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
