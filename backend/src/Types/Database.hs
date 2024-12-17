module Types.Database where

import Data.Aeson (ToJSON)
import Data.Time (UTCTime)
import Database.SQLite.Simple (FromRow, ToRow)
import Types.DataField
  ( Competition,
    EloId,
    Expansion,
    League,
    Match,
    PlayerId,
    PlayerName,
    Rating,
    ReportId,
    Side,
    Stronghold,
    Victory,
  )

data WriteProcessedGameReport = WriteProcessedGameReport
  { timestamp :: UTCTime,
    winnerId :: PlayerId,
    loserId :: PlayerId,
    side :: Side,
    victory :: Victory,
    match :: Match,
    competition :: [Competition],
    league :: Maybe League,
    expansions :: [Expansion],
    treebeard :: Maybe Bool,
    actionTokens :: Int,
    dwarvenRings :: Int,
    turns :: Int,
    corruption :: Int,
    mordor :: Maybe Int,
    initialEyes :: Int,
    aragornTurn :: Maybe Int,
    strongholds :: [Stronghold],
    interestRating :: Int,
    comments :: Maybe Text
  }
  deriving (Generic)

instance ToRow WriteProcessedGameReport

data ReadProcessedGameReport = ReadProcessedGameReport
  { rid :: Int,
    timestamp :: UTCTime,
    winner :: PlayerName,
    loser :: PlayerName,
    side :: Side,
    victory :: Victory,
    match :: Match,
    competition :: [Competition],
    league :: Maybe League,
    expansions :: [Expansion],
    treebeard :: Maybe Bool,
    actionTokens :: Int,
    dwarvenRings :: Int,
    turns :: Int,
    corruption :: Int,
    mordor :: Maybe Int,
    initialEyes :: Int,
    aragornTurn :: Maybe Int,
    strongholds :: [Stronghold],
    interestRating :: Int,
    comments :: Maybe Text
  }
  deriving (Generic)

instance FromRow ReadProcessedGameReport

instance ToJSON ReadProcessedGameReport

data WritePlayer = WritePlayer
  { name :: PlayerName,
    country :: Maybe Text
  }
  deriving (Generic)

instance ToRow WritePlayer

data ReadPlayer = ReadPlayer
  { pid :: PlayerId,
    name :: PlayerName,
    country :: Maybe Text
  }
  deriving (Generic)

instance FromRow ReadPlayer

instance ToJSON ReadPlayer

data WriteRatingChange = WriteRatingChange
  { pid :: PlayerId,
    side :: Side,
    timestamp :: UTCTime,
    rid :: ReportId,
    ratingBefore :: Rating,
    ratingAfter :: Rating
  }
  deriving (Generic)

instance ToRow WriteRatingChange

data ReadRatingChange = ReadRatingChange
  { eid :: EloId,
    side :: Side,
    timestamp :: UTCTime,
    rid :: ReportId,
    ratingBefore :: Rating,
    ratingAfter :: Rating
  }
  deriving (Generic)

instance FromRow ReadRatingChange
