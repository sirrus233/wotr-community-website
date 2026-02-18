{-# LANGUAGE TemplateHaskell #-}

module Types.Api where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, eitherDecodeStrict, eitherDecodeStrictText)
import Data.Aeson.TypeScript.TH (deriveJSONAndTypeScript, deriveTypeScript)
import Data.ByteString (StrictByteString)
import Data.Time (UTCTime)
import Database.Esqueleto.Experimental (Entity (..))
import Relude.Extra (lookupDefault)
import Servant (FromHttpApiData (..), Header, Headers, MimeUnrender (..), NoContent, PlainText, SourceIO)
import Servant.Multipart (FileData (..), FromMultipart (..), MultipartData (..), Tmp, lookupFile, lookupInput)
import Types.DataField (Competition, Expansion, League, Match, PlayerName, Rating, Side, Stronghold, Victory)
import Types.Database
  ( GameReport (..),
    GameReportId,
    LeagueGameStatsMap,
    Player (..),
    PlayerId,
    PlayerStats,
    PlayerStatsAggregate (..),
    PlayerStatsTotal (..),
    PlayerStatsYear (..),
    ReportInsertion,
  )
import Types.TypeScriptInstances ()
import Web.Cookie (SetCookie)

type S3Url = Text

newtype IdToken = IdToken Text

instance MimeUnrender PlainText IdToken where
  mimeUnrender :: Proxy PlainText -> LByteString -> Either String IdToken
  mimeUnrender _ = Right . IdToken . decodeUtf8

-- Defining here to reduce verbosity, needed to work around https://github.com/haskell-servant/servant/issues/1267
type GoogleLoginResponse = Headers '[Header "Set-Cookie" SetCookie] NoContent

newtype UserInfoResponse = UserInfoResponse {isAdmin :: Bool} deriving (Generic)

$(deriveJSONAndTypeScript defaultOptions ''UserInfoResponse)

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
    musterPoints :: Int,
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

$(deriveJSONAndTypeScript defaultOptions ''RawGameReport)

data SubmitReportRequest = SubmitReportRequest
  { report :: RawGameReport,
    logFile :: Maybe (FileData Tmp)
  }
  deriving (Generic)

$(deriveTypeScript defaultOptions ''SubmitReportRequest)

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
      gameReportMusterPoints = r.musterPoints,
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
    musterPoints :: Int,
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

$(deriveJSONAndTypeScript defaultOptions ''ProcessedGameReport)

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
      musterPoints = r.gameReportMusterPoints,
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

$(deriveJSONAndTypeScript defaultOptions ''SubmitGameReportResponse)

data GetReportsResponse = GetReportsResponse
  { reports :: [ProcessedGameReport],
    total :: Int
  }
  deriving (Generic)

$(deriveJSONAndTypeScript defaultOptions ''GetReportsResponse)

data LeaderboardEntry = LeaderboardEntry
  { pid :: PlayerId,
    name :: PlayerName,
    country :: Maybe Text,
    isActive :: Bool,
    currentRatingFree :: Rating,
    currentRatingShadow :: Rating,
    averageRating :: Double,
    -- | {-# DEPRECATED No longer used, kept for backward compatibility #-}
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

$(deriveJSONAndTypeScript defaultOptions ''LeaderboardEntry)

fromPlayerStats :: (Entity Player, PlayerStats k) -> LeaderboardEntry
fromPlayerStats (Entity pid p, (t, agg)) =
  LeaderboardEntry
    { pid,
      name = p.playerDisplayName,
      country = p.playerCountry,
      isActive = p.playerIsActive,
      currentRatingFree = t.playerStatsTotalRatingFree,
      currentRatingShadow = t.playerStatsTotalRatingShadow,
      averageRating = (fromIntegral t.playerStatsTotalRatingFree + fromIntegral t.playerStatsTotalRatingShadow) / 2,
      totalGames = 0,
      year = aggYear, -- TODO This should be optional, but defaults 0 for backwards compatibility
      -- TODO In the same vein, these fields would more accurately be called "aggregateGames", etc.
      yearlyGames = aggGames,
      yearlyWinsFree = aggWinsFree,
      yearlyWinsShadow = aggWinsShadow,
      yearlyLossesFree = aggLossesFree,
      yearlyLossesShadow = aggLossesShadow,
      yearlyWinRateFree = fromIntegral aggWinsFree / fromIntegral (aggWinsFree + aggLossesFree),
      yearlyWinRateShadow = fromIntegral aggWinsShadow / fromIntegral (aggWinsShadow + aggLossesShadow)
    }
  where
    aggYear = case agg of AnnualAgg y -> y.playerStatsYearYear; AllTimeAgg {} -> 0
    aggGames = case agg of
      AnnualAgg _ -> aggWinsFree + aggWinsShadow + aggLossesFree + aggLossesShadow
      AllTimeAgg {} -> t.playerStatsTotalGameCount
    aggWinsFree = case agg of AnnualAgg y -> y.playerStatsYearWinsFree; AllTimeAgg {..} -> winsFree
    aggWinsShadow = case agg of AnnualAgg y -> y.playerStatsYearWinsShadow; AllTimeAgg {..} -> winsShadow
    aggLossesFree = case agg of AnnualAgg y -> y.playerStatsYearLossesFree; AllTimeAgg {..} -> lossesFree
    aggLossesShadow = case agg of AnnualAgg y -> y.playerStatsYearLossesShadow; AllTimeAgg {..} -> lossesShadow

newtype GetLeaderboardResponse = GetLeaderboardResponse {entries :: [LeaderboardEntry]} deriving (Generic)

$(deriveJSONAndTypeScript defaultOptions ''GetLeaderboardResponse)

data EditPlayerRequest = EditPlayerRequest
  { pid :: PlayerId,
    name :: PlayerName,
    country :: Maybe Text
  }
  deriving (Generic)

$(deriveJSONAndTypeScript defaultOptions ''EditPlayerRequest)

data RemapPlayerRequest = RemapPlayerRequest
  { fromPid :: PlayerId,
    toPid :: PlayerId
  }
  deriving (Generic)

$(deriveJSONAndTypeScript defaultOptions ''RemapPlayerRequest)

newtype RemapPlayerResponse = RemapPlayerResponse
  { name :: PlayerName
  }
  deriving (Generic)

$(deriveJSONAndTypeScript defaultOptions ''RemapPlayerResponse)

data ModifyReportRequest = ModifyReportRequest
  { rid :: GameReportId,
    timestamp :: Maybe UTCTime,
    report :: RawGameReport
  }
  deriving (Generic)

$(deriveJSONAndTypeScript defaultOptions ''ModifyReportRequest)

newtype DeleteReportRequest = DeleteReportRequest
  { rid :: GameReportId
  }
  deriving (Generic)

$(deriveJSONAndTypeScript defaultOptions ''DeleteReportRequest)

data LeaguePlayerStatsSummary = LeaguePlayerStatsSummary
  { totalWins :: Int,
    totalGames :: Int,
    points :: Double
  }
  deriving (Generic)

$(deriveJSONAndTypeScript defaultOptions ''LeaguePlayerStatsSummary)

data LeagueGameStats = LeagueGameStats
  { opponent :: Text,
    wins :: Int,
    losses :: Int
  }
  deriving (Generic)

$(deriveJSONAndTypeScript defaultOptions ''LeagueGameStats)

data LeaguePlayerStats = LeaguePlayerStats
  { name :: Text,
    summary :: LeaguePlayerStatsSummary,
    gameStatsByOpponent :: Map PlayerId LeagueGameStats
  }
  deriving (Generic)

$(deriveJSONAndTypeScript defaultOptions ''LeaguePlayerStats)

type LeagueStatsResponse = Map PlayerId LeaguePlayerStats

fromLeagueGameStatsMap :: PlayerId -> LeagueGameStatsMap -> Map PlayerId LeagueGameStats
fromLeagueGameStatsMap playerId =
  fromList . fmap (\(opponentId, opponent, wins, losses) -> (opponentId, LeagueGameStats {..})) . lookupDefault [] playerId

type ExportResponse = (Headers '[Header "Content-Disposition" String]) (SourceIO StrictByteString)

data TimestampFilter = Before UTCTime | After UTCTime | Between UTCTime UTCTime deriving (Generic)

$(deriveJSONAndTypeScript defaultOptions ''TimestampFilter)

data InequalityFilter = InequalityFilter Ordering Int deriving (Generic)

$(deriveJSONAndTypeScript defaultOptions ''InequalityFilter)

data VictoryFilter
  = VictorySideFilter Side
  | VictoryKindFilter Victory
  | VictoryComboFilter Side Victory
  deriving (Generic)

$(deriveJSONAndTypeScript defaultOptions ''VictoryFilter)

data NullableFilter f = NullFilter | ValueFilter f deriving (Generic)

instance (FromJSON f) => FromJSON (NullableFilter f)

instance (ToJSON f) => ToJSON (NullableFilter f)

$(deriveTypeScript defaultOptions ''NullableFilter)

-- TODO Unused until competition can be filtered in the DB
-- data CompetitionFilter = RatedFilter Match | CompetitionFilter Competition deriving (Generic)
-- instance FromJSON CompetitionFilter

data GameReportFilterSpec = GameReportFilterSpec
  { players :: Maybe [PlayerId],
    pairing :: Maybe (PlayerId, Maybe PlayerId),
    timestamp :: Maybe TimestampFilter,
    winners :: Maybe [PlayerId],
    losers :: Maybe [PlayerId],
    turns :: Maybe InequalityFilter,
    victory :: Maybe (NonEmpty VictoryFilter),
    -- TODO competition
    leagues :: Maybe [League],
    -- TODO expansions
    tokens :: Maybe InequalityFilter,
    dwarvenRings :: Maybe InequalityFilter,
    musterPoints :: Maybe InequalityFilter,
    corruption :: Maybe InequalityFilter,
    mordor :: Maybe (NullableFilter InequalityFilter),
    aragorn :: Maybe (NullableFilter InequalityFilter),
    treebeard :: Maybe Bool,
    initialEyes :: Maybe InequalityFilter,
    -- TODO spCaptured
    -- TODO spvp
    -- TODO fpCaptured
    -- TODO fpvp
    interestRating :: Maybe InequalityFilter,
    hasLog :: Maybe Bool
  }
  deriving (Generic)

$(deriveJSONAndTypeScript defaultOptions ''GameReportFilterSpec)

instance FromHttpApiData GameReportFilterSpec where
  parseQueryParam :: Text -> Either Text GameReportFilterSpec
  parseQueryParam = first toText . eitherDecodeStrictText
