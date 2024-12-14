module Types.DataField where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as T
import Database.SQLite.Simple (ResultError (..), SQLData (..))
import Database.SQLite.Simple.FromField (FieldParser, FromField (..), fieldData, returnError)
import Database.SQLite.Simple.Ok (Ok (..))
import Database.SQLite.Simple.ToField (ToField (..))

defaultToField :: (Show a) => a -> SQLData
defaultToField a = SQLText (show a)

defaultListToField :: (Show a) => [a] -> SQLData
defaultListToField as = SQLText (T.intercalate "," . map show $ as)

defaultFromField :: (Read a, Typeable a) => FieldParser a
defaultFromField f = case fieldData f of
  SQLText t -> case readMaybe . toString $ t of
    Just a -> Ok a
    Nothing -> returnError ConversionFailed f "Unreadable text field."
  _ -> returnError ConversionFailed f "Unexpected non-text SQL value."

defaultListFromField :: (Read a, Typeable a) => FieldParser [a]
defaultListFromField f = case fieldData f of
  SQLText t -> case traverse (readMaybe . toString) . T.splitOn "," $ t of
    Just a -> Ok a
    Nothing -> returnError ConversionFailed f "Unreadable value in semantic list text field."
  _ -> returnError ConversionFailed f "Unexpected non-text SQL value."

type PlayerName = String

type PlayerId = Int

data Side = Free | Shadow deriving (Eq, Generic, Read, Show)

instance ToField Side where
  toField = defaultToField

instance FromField Side where
  fromField = defaultFromField

instance ToJSON Side

instance FromJSON Side

data Victory = Ring | Military | Concession deriving (Eq, Generic, Read, Show)

instance ToField Victory where
  toField = defaultToField

instance FromField Victory where
  fromField = defaultFromField

instance ToJSON Victory

instance FromJSON Victory

data Match = Ranked | Unranked deriving (Eq, Generic, Read, Show)

instance ToField Match where
  toField = defaultToField

instance FromField Match where
  fromField = defaultFromField

instance ToJSON Match

instance FromJSON Match

data Competition = League | Tournament deriving (Eq, Generic, Read, Show)

instance ToField [Competition] where
  toField = defaultListToField

instance FromField [Competition] where
  fromField = defaultListFromField

instance ToJSON Competition

instance FromJSON Competition

data League = GeneralLeague | LoMELeague | WoMELeague | SuperLeague | TTSLeague deriving (Eq, Generic, Read, Show)

instance ToField League where
  toField = defaultToField

instance FromField League where
  fromField = defaultFromField

instance ToJSON League

instance FromJSON League

data Expansion = LoME | WoME | KoME | Cities | FateOfErebor | Treebeard deriving (Eq, Generic, Read, Show)

instance ToField [Expansion] where
  toField = defaultListToField

instance FromField [Expansion] where
  fromField = defaultListFromField

instance ToJSON Expansion

instance FromJSON Expansion

-- TODO Shadow Strongholds (for FPMV)
data Stronghold
  = Rivendell
  | GreyHavens
  | HelmsDeep
  | Lorien
  | WoodlandRealm
  | Erebor
  | MinasTirith
  | DolAmroth
  | Shire
  | Edoras
  | Dale
  | Pelargir
  | EredLuin
  | IronHills
  deriving (Eq, Generic, Read, Show)

instance ToField [Stronghold] where
  toField = defaultListToField

instance FromField [Stronghold] where
  fromField = defaultListFromField

instance ToJSON Stronghold

instance FromJSON Stronghold
