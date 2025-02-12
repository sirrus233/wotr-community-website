module Types.Api where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict)
import Data.ByteString (StrictByteString)
import Data.Time (UTCTime)
import Database.Esqueleto.Experimental (Entity (..))
import Relude.Extra (lookupDefault)
import Servant (Header, Headers, MimeUnrender (..), NoContent, PlainText, SourceIO)
import Servant.Multipart (FileData (..), FromMultipart (..), MultipartData (..), Tmp, lookupFile, lookupInput)
import Types.DataField (Competition, Expansion, League, Match, PlayerName, Rating, Side, Stronghold, Victory)
import Types.Database
  ( GameReport (..),
    GameReportId,
    LeagueGameStatsMap,
    Player (..),
    PlayerId,
    PlayerStats,
    PlayerStatsTotal (..),
    PlayerStatsYear (..),
    ReportInsertion,
  )
import Web.Cookie (SetCookie)

type S3Url = Text

newtype IdToken = IdToken Text

instance MimeUnrender PlainText IdToken where
  mimeUnrender :: Proxy PlainText -> LByteString -> Either String IdToken
  mimeUnrender _ = Right . IdToken . decodeUtf8

-- Defining here to reduce verbosity, needed to work around https://github.com/haskell-servant/servant/issues/1267
type GoogleLoginResponse = Headers '[Header "Set-Cookie" SetCookie] NoContent

newtype UserInfoResponse = UserInfoResponse {isAdmin :: Bool} deriving (Generic)

instance ToJSON UserInfoResponse

data SubmitReportRequest = SubmitReportRequest
  { report :: RawGameReport,
    logFile :: Maybe (FileData Tmp)
  }
  deriving (Generic)

instance FromMultipart Tmp SubmitReportRequest where
  fromMultipart :: MultipartData Tmp -> Either String SubmitReportRequest
  fromMultipart multipartData = do
    rawJson <- lookupInput "report" multipartData
    report <- case eitherDecodeStrict (encodeUtf8 rawJson) of
      Left err -> Left $ "Error parsing RawGameReport JSON: " <> err
      Right a -> Right a
    logFile <- case lookupFile "logFile" multipartData of
      Left _ -> Right Nothing
      Right f -> Right . Just $ f
    pure $ SubmitReportRequest {..}

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
    comment :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON RawGameReport

toGameReport :: UTCTime -> PlayerId -> PlayerId -> Maybe S3Url -> RawGameReport -> GameReport
toGameReport timestamp winnerId loserId logFile r =
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
      gameReportComment = r.comment,
      gameReportLogFile = logFile
    }

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
    comment :: Maybe Text,
    logFile :: Maybe S3Url
  }
  deriving (Generic)

instance ToJSON ProcessedGameReport

fromGameReport :: ReportInsertion -> ProcessedGameReport
fromGameReport (Entity rid r, Entity _ winner, Entity _ loser) =
  ProcessedGameReport
    { rid,
      timestamp = r.gameReportTimestamp,
      winnerId = r.gameReportWinnerId,
      winner = winner.playerDisplayName,
      loserId = r.gameReportLoserId,
      loser = loser.playerDisplayName,
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
      comment = r.gameReportComment,
      logFile = r.gameReportLogFile
    }

data SubmitGameReportResponse = SubmitGameReportResponse
  { report :: ProcessedGameReport,
    winnerRating :: Rating,
    loserRating :: Rating
  }
  deriving (Generic)

instance ToJSON SubmitGameReportResponse

data GetReportsResponse = GetReportsResponse
  { reports :: [ProcessedGameReport],
    total :: Int
  }
  deriving (Generic)

instance ToJSON GetReportsResponse

data LeaderboardEntry = LeaderboardEntry
  { pid :: PlayerId,
    name :: PlayerName,
    country :: Maybe Text,
    isActive :: Bool,
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
fromPlayerStats (Entity pid p, (t, y)) =
  LeaderboardEntry
    { pid,
      name = p.playerDisplayName,
      country = p.playerCountry,
      isActive = p.playerIsActive,
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

newtype GetLeaderboardResponse = GetLeaderboardResponse {entries :: [LeaderboardEntry]} deriving (Generic)

instance ToJSON GetLeaderboardResponse

data EditPlayerRequest = EditPlayerRequest
  { pid :: PlayerId,
    name :: PlayerName,
    country :: Maybe Text
  }
  deriving (Generic)

instance FromJSON EditPlayerRequest

data RemapPlayerRequest = RemapPlayerRequest
  { fromPid :: PlayerId,
    toPid :: PlayerId
  }
  deriving (Generic)

instance FromJSON RemapPlayerRequest

newtype RemapPlayerResponse = RemapPlayerResponse
  { name :: PlayerName
  }
  deriving (Generic)

instance ToJSON RemapPlayerResponse

data ModifyReportRequest = ModifyReportRequest
  { rid :: GameReportId,
    timestamp :: Maybe UTCTime,
    report :: RawGameReport
  }
  deriving (Generic)

instance FromJSON ModifyReportRequest

newtype DeleteReportRequest = DeleteReportRequest
  { rid :: GameReportId
  }
  deriving (Generic)

instance FromJSON DeleteReportRequest

data LeaguePlayerStats = LeaguePlayerStats
  { name :: Text,
    summary :: LeaguePlayerStatsSummary,
    gameStatsByOpponent :: Map PlayerId LeagueGameStats
  }
  deriving (Generic)

instance ToJSON LeaguePlayerStats

data LeaguePlayerStatsSummary = LeaguePlayerStatsSummary
  { totalWins :: Int,
    totalGames :: Int,
    points :: Double
  }
  deriving (Generic)

instance ToJSON LeaguePlayerStatsSummary

data LeagueGameStats = LeagueGameStats
  { opponent :: Text,
    wins :: Int,
    losses :: Int
  }
  deriving (Generic)

instance ToJSON LeagueGameStats

type LeagueStatsResponse = Map PlayerId LeaguePlayerStats

fromLeagueGameStatsMap :: PlayerId -> LeagueGameStatsMap -> Map PlayerId LeagueGameStats
fromLeagueGameStatsMap playerId =
  fromList . fmap (\(opponentId, opponent, wins, losses) -> (opponentId, LeagueGameStats {..})) . lookupDefault [] playerId

type ExportResponse = (Headers '[Header "Content-Disposition" String]) (SourceIO StrictByteString)
