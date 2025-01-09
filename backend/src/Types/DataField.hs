module Types.DataField where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as T
import Database.Esqueleto.Experimental (PersistField (..), PersistFieldSql, PersistValue (..), SqlType (..))
import Database.Persist.Sql (PersistFieldSql (..))

defaultToPersistValue :: (Show a) => a -> PersistValue
defaultToPersistValue a = PersistText (show a)

defaultListToPersistValue :: (Show a) => [a] -> PersistValue
defaultListToPersistValue as = PersistText (T.intercalate "," . map show $ as)

defaultFromPersistValue :: (Read a, Typeable a) => PersistValue -> Either Text a
defaultFromPersistValue v = case v of
  PersistText t -> maybeToRight "Unreadable text field." (readMaybe . toString $ t)
  _ -> Left "Unexpected non-text SQL value."

defaultListFromPersistValue :: (Read a, Typeable a) => PersistValue -> Either Text [a]
defaultListFromPersistValue v = case v of
  PersistText "" -> Right []
  PersistText t ->
    maybeToRight "Unreadable value in semantic list text field." (traverse (readMaybe . toString) . T.splitOn "," $ t)
  _ -> Left "Unexpected non-text SQL value."

type PlayerName = Text

type Rating = Int

type Year = Int

data Side = Free | Shadow deriving (Eq, Generic, Read, Show)

instance PersistField Side where
  toPersistValue :: Side -> PersistValue
  toPersistValue = defaultToPersistValue
  fromPersistValue = defaultFromPersistValue

instance PersistFieldSql Side where
  sqlType _ = SqlString

instance ToJSON Side

instance FromJSON Side

data Victory = Ring | Military | Concession deriving (Eq, Generic, Read, Show)

instance PersistField Victory where
  toPersistValue = defaultToPersistValue
  fromPersistValue = defaultFromPersistValue

instance PersistFieldSql Victory where
  sqlType _ = SqlString

instance ToJSON Victory

instance FromJSON Victory

data Match = Rated | Unrated deriving (Eq, Generic, Read, Show)

instance PersistField Match where
  toPersistValue = defaultToPersistValue
  fromPersistValue = defaultFromPersistValue

instance PersistFieldSql Match where
  sqlType _ = SqlString

instance ToJSON Match

instance FromJSON Match

data Competition = League | Tournament deriving (Eq, Generic, Read, Show)

instance PersistField [Competition] where
  toPersistValue = defaultListToPersistValue
  fromPersistValue = defaultListFromPersistValue

instance PersistFieldSql [Competition] where
  sqlType _ = SqlString

instance ToJSON Competition

instance FromJSON Competition

data League = GeneralLeague | LoMELeague | WoMELeague | SuperLeague | TTSLeague deriving (Eq, Generic, Read, Show)

instance PersistField League where
  toPersistValue = defaultToPersistValue
  fromPersistValue = defaultFromPersistValue

instance PersistFieldSql League where
  sqlType _ = SqlString

instance ToJSON League

instance FromJSON League

data Expansion = LoME | WoME | KoME | Cities | FateOfErebor | Treebeard deriving (Eq, Generic, Read, Show)

instance PersistField [Expansion] where
  toPersistValue = defaultListToPersistValue
  fromPersistValue = defaultListFromPersistValue

instance PersistFieldSql [Expansion] where
  sqlType _ = SqlString

instance ToJSON Expansion

instance FromJSON Expansion

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
  | MountGundabad
  | Angmar
  | Moria
  | DolGuldur
  | Orthanc
  | Morannon
  | BaradDur
  | MinasMorgul
  | Umbar
  | FarHarad
  | SouthRhun
  deriving (Eq, Generic, Read, Show)

instance PersistField [Stronghold] where
  toPersistValue = defaultListToPersistValue
  fromPersistValue = defaultListFromPersistValue

instance PersistFieldSql [Stronghold] where
  sqlType _ = SqlString

instance ToJSON Stronghold

instance FromJSON Stronghold
