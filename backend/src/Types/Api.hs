module Types.Api where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime)
import Database.Esqueleto.Experimental (Entity (..))
import Types.DataField (Competition, Expansion, League, Match, PlayerName, Rating, Side, Stronghold, Victory)
import Types.Database (GameReport (..), GameReportId, Player (..), PlayerId, PlayerStatsTotal (..), PlayerStatsYear (..))

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
  deriving (Generic)

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

data LeaderboardEntry = LeaderboardEntry
  { pid :: PlayerId,
    name :: PlayerName,
    country :: Maybe Text,
    currentRatingFree :: Rating,
    currentRatingShadow :: Rating,
    averageRating :: Double,
    totalGames :: Int,
    totalWinsFree :: Int,
    totalWinsShadow :: Int,
    totalLossesFree :: Int,
    totalLossesShadow :: Int,
    totalWinRateFree :: Double,
    totalWinRateShadow :: Double,
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

fromPlayerStats :: (Entity Player, Entity PlayerStatsTotal, Entity PlayerStatsYear) -> LeaderboardEntry
fromPlayerStats (player, totalStats, yearStats) =
  LeaderboardEntry
    { pid = entityKey player,
      name = p.playerName,
      country = p.playerCountry,
      currentRatingFree = t.playerStatsTotalCurrentRatingFree,
      currentRatingShadow = t.playerStatsTotalCurrentRatingShadow,
      averageRating =
        (fromIntegral t.playerStatsTotalCurrentRatingFree + fromIntegral t.playerStatsTotalCurrentRatingShadow) / 2,
      totalGames =
        t.playerStatsTotalWinsFree + t.playerStatsTotalWinsShadow + t.playerStatsTotalLossesFree + t.playerStatsTotalLossesShadow,
      totalWinsFree = t.playerStatsTotalWinsFree,
      totalWinsShadow = t.playerStatsTotalWinsShadow,
      totalLossesFree = t.playerStatsTotalLossesFree,
      totalLossesShadow = t.playerStatsTotalLossesShadow,
      totalWinRateFree =
        fromIntegral t.playerStatsTotalWinsFree / fromIntegral (t.playerStatsTotalWinsFree + t.playerStatsTotalLossesFree),
      totalWinRateShadow =
        fromIntegral t.playerStatsTotalWinsShadow / fromIntegral (t.playerStatsTotalWinsShadow + t.playerStatsTotalLossesShadow),
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
    p = entityVal player
    t = entityVal totalStats
    y = entityVal yearStats

newtype GetLeaderboardResponse = GetLeaderboardResponse {entries :: [LeaderboardEntry]} deriving (Generic)

instance ToJSON GetLeaderboardResponse

-- TODO Game Logs
