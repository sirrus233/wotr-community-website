module Main where

import Data.Aeson (ToJSON)
import Network.Wai.Handler.Warp (run)
import Servant.API (Get, JSON, type (:>))
import Servant.Server (serve)

data User = User {name :: String, age :: Int, email :: String} deriving (Generic)

type UserAPI = "users" :> Get '[JSON] User

instance ToJSON User

userAPI :: Proxy UserAPI
userAPI = Proxy

main :: IO ()
main = run 8081 $ serve userAPI (pure $ User {name = "Gandalf", age = 6000, email = "mithrandir@valinor.net"})
