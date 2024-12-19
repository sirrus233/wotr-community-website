module Types.Api where

import Data.Aeson (FromJSON, ToJSON)
import Types.DataField (Competition, Expansion, League, Match, PlayerName, Rating, Side, Stronghold, Victory)
import Types.Database (ReadProcessedGameReport)

data GameReport = GameReport
  { winner :: PlayerName,
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

instance FromJSON GameReport

data SubmitGameReportResponse = SubmitGameReportResponse
  { report :: ReadProcessedGameReport,
    winnerRating :: Rating,
    loserRating :: Rating
  }
  deriving (Generic)

instance ToJSON SubmitGameReportResponse

-- TODO Game Logs