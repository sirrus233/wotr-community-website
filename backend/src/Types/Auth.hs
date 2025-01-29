module Types.Auth where

import Data.Aeson (FromJSON)
import Servant (AuthProtect)
import Servant.Server.Experimental.Auth (AuthServerData)

data SessionIdCookie = Proxy -- Type-level tag for the auth scheme

type instance AuthServerData (AuthProtect SessionIdCookie) = Authenticated

newtype UserId = UserId {unUserId :: Text}

newtype SessionId = SessionId {unSessionId :: Text}

data Authenticated = Authenticated {userId :: UserId, sessionId :: SessionId}

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
