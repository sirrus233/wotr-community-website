module Types.Api where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime)
import Database.Esqueleto.Experimental (Entity (..))
import Types.DataField (Competition, Expansion, League, Match, PlayerName, Rating, Side, Stronghold, Victory, Year)
import Types.Database (GameReport (..), GameReportId, Player (..), PlayerId, PlayerStats, PlayerStatsTotal (..), PlayerStatsYear (..))

data RawGameReport = RawGameReport
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
  deriving (Generic, Show)

instance FromJSON RawGameReport

instance ToJSON RawGameReport

toGameReport :: UTCTime -> PlayerId -> PlayerId -> RawGameReport -> GameReport
toGameReport timestamp winnerId loserId r =
  GameReport
    { gameReportTimestamp = timestamp,
      gameReportWinnerId = winnerId,
      gameReportLoserId = loserId,
      gameReportSide = r.side,
      gameReportVictory = r.victory,
      gameReportMatch = r.match,
      gameReportCompetition = r.competition,
      gameReportLeague = r.league,
      gameReportExpansions = r.expansions,
      gameReportTreebeard = r.treebeard,
      gameReportActionTokens = r.actionTokens,
      gameReportDwarvenRings = r.dwarvenRings,
      gameReportTurns = r.turns,
      gameReportCorruption = r.corruption,
      gameReportMordor = r.mordor,
      gameReportInitialEyes = r.initialEyes,
      gameReportAragornTurn = r.aragornTurn,
      gameReportStrongholds = r.strongholds,
      gameReportInterestRating = r.interestRating,
      gameReportComments = r.comments
    }

data SubmitGameReportResponse = SubmitGameReportResponse
  { report :: RawGameReport,
    winnerRating :: Rating,
    loserRating :: Rating
  }
  deriving (Generic)

instance ToJSON SubmitGameReportResponse

data ProcessedGameReport = ProcessedGameReport
  { rid :: GameReportId,
    timestamp :: UTCTime,
    winnerId :: PlayerId,
    winner :: PlayerName,
    loserId :: PlayerId,
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

instance ToJSON ProcessedGameReport

fromGameReport :: (Entity GameReport, Entity Player, Entity Player) -> ProcessedGameReport
fromGameReport (report, winner, loser) =
  ProcessedGameReport
    { rid = entityKey report,
      timestamp = r.gameReportTimestamp,
      winnerId = r.gameReportWinnerId,
      winner = playerName . entityVal $ winner,
      loserId = r.gameReportLoserId,
      loser = playerName . entityVal $ loser,
      side = r.gameReportSide,
      victory = r.gameReportVictory,
      match = r.gameReportMatch,
      competition = r.gameReportCompetition,
      league = r.gameReportLeague,
      expansions = r.gameReportExpansions,
      treebeard = r.gameReportTreebeard,
      actionTokens = r.gameReportActionTokens,
      dwarvenRings = r.gameReportDwarvenRings,
      turns = r.gameReportTurns,
      corruption = r.gameReportCorruption,
      mordor = r.gameReportMordor,
      initialEyes = r.gameReportInitialEyes,
      aragornTurn = r.gameReportAragornTurn,
      strongholds = r.gameReportStrongholds,
      interestRating = r.gameReportInterestRating,
      comments = r.gameReportComments
    }
  where
    r = entityVal report

newtype GetReportsResponse = GetReportsResponse {reports :: [ProcessedGameReport]} deriving (Generic)

instance ToJSON GetReportsResponse

newtype GetLeaderboardRequest = GetLeaderboardRequest {year :: Year} deriving (Generic)

instance FromJSON GetLeaderboardRequest

data LeaderboardEntry = LeaderboardEntry
  { pid :: PlayerId,
    name :: PlayerName,
    country :: Maybe Text,
    currentRatingFree :: Rating,
    currentRatingShadow :: Rating,
    averageRating :: Double,
    totalGames :: Int,
    year :: Int,
    yearlyGames :: Int,
    yearlyWinsFree :: Int,
    yearlyWinsShadow :: Int,
    yearlyLossesFree :: Int,
    yearlyLossesShadow :: Int,
    yearlyWinRateFree :: Double,
    yearlyWinRateShadow :: Double
  }
  deriving (Generic)

instance ToJSON LeaderboardEntry

fromPlayerStats :: (Entity Player, PlayerStats) -> LeaderboardEntry
fromPlayerStats (player, (t, y)) =
  LeaderboardEntry
    { pid,
      name = p.playerName,
      country = p.playerCountry,
      currentRatingFree = t.playerStatsTotalRatingFree,
      currentRatingShadow = t.playerStatsTotalRatingShadow,
      averageRating =
        (fromIntegral t.playerStatsTotalRatingFree + fromIntegral t.playerStatsTotalRatingShadow) / 2,
      totalGames = t.playerStatsTotalGameCount,
      year = y.playerStatsYearYear,
      yearlyGames =
        y.playerStatsYearWinsFree + y.playerStatsYearWinsShadow + y.playerStatsYearLossesFree + y.playerStatsYearLossesShadow,
      yearlyWinsFree = y.playerStatsYearWinsFree,
      yearlyWinsShadow = y.playerStatsYearWinsShadow,
      yearlyLossesFree = y.playerStatsYearLossesFree,
      yearlyLossesShadow = y.playerStatsYearLossesShadow,
      yearlyWinRateFree =
        fromIntegral y.playerStatsYearWinsFree / fromIntegral (y.playerStatsYearWinsFree + y.playerStatsYearLossesFree),
      yearlyWinRateShadow =
        fromIntegral y.playerStatsYearWinsShadow / fromIntegral (y.playerStatsYearWinsShadow + y.playerStatsYearLossesShadow)
    }
  where
    pid = entityKey player
    p = entityVal player

newtype GetLeaderboardResponse = GetLeaderboardResponse {entries :: [LeaderboardEntry]} deriving (Generic)

instance ToJSON GetLeaderboardResponse

-- TODO Game Logs
