module AppServer where

import Amazonka qualified as AWS
import Amazonka.S3 qualified as S3
import Api (API, Protected, Service, Unprotected)
import AppConfig (AppM, Env (..), authCookieName, gameLogBucket)
import Auth (fetchGoogleJWKSet, validateToken)
import Codec.Archive.Zip (addEntryToArchive, emptyArchive, fromArchive, toEntry)
import Control.Monad.Logger (MonadLogger, logErrorN, logInfoN)
import Data.ByteString.Lazy (toChunks)
import Data.Csv (encodeByName)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime (..), defaultTimeLocale, formatTime, getCurrentTime, toGregorian)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Data.Validation (Validation (..))
import Database
  ( DBAction,
    SortOrder (..),
    deleteGameReport,
    deletePlayer,
    getAllGameReports,
    getAllStats,
    getGameReports,
    getLeagueGameStats,
    getLeaguePlayerSummary,
    getNumGameReports,
    getPlayerByName,
    getPlayerStats,
    insertGameReport,
    insertLeaguePlayer,
    insertPlayerIfNotExists,
    repsertPlayerStats,
    resetStats,
    runAuthDb,
    runDb,
    updateActiveStatus,
    updateAdminSessionId,
    updateLeaguePlayer,
    updatePlayerCountry,
    updatePlayerName,
    updateReports,
  )
import Database.Esqueleto.Experimental (Entity (..), PersistStoreRead (..), PersistStoreWrite (..), Value (..), toSqlKey)
import Database.Persist (selectList)
import Logging ((<>:))
import Network.HTTP.Client.Conduit (newManager)
import Relude.Extra (groupBy, lookupDefault)
import Servant
  ( AuthProtect,
    NoContent (..),
    ServerError (..),
    ServerT,
    addHeader,
    err401,
    err404,
    err422,
    err500,
    throwError,
    type (:<|>) (..),
  )
import Servant.Multipart (FileData (..))
import Servant.Server.Experimental.Auth (AuthServerData)
import Servant.Types.SourceT (source)
import Types.Api
  ( DeleteReportRequest (..),
    EditPlayerRequest (..),
    ExportResponse,
    GameReportFilterSpec,
    GetLeaderboardResponse (GetLeaderboardResponse),
    GetReportsResponse (..),
    GoogleLoginResponse,
    IdToken,
    LeaderboardEntry (..),
    LeaguePlayerStats (..),
    LeaguePlayerStatsSummary (..),
    LeagueStatsResponse,
    ModifyReportRequest (..),
    ProcessedGameReport,
    RawGameReport (..),
    RemapPlayerRequest (..),
    RemapPlayerResponse (..),
    S3Url,
    SubmitGameReportResponse (..),
    SubmitReportRequest (..),
    UserInfoResponse (..),
    fromGameReport,
    fromLeagueGameStatsMap,
    fromPlayerStats,
    toGameReport,
  )
import Types.Auth (AuthenticatedUser (..), ServiceCaller, SessionId (..), SessionIdCookie)
import Types.DataField (League (..), LeagueTier, Match (..), PlayerName, Rating, Side (..), Year)
import Types.Database
  ( ExportGameReport (..),
    ExportLeaguePlayer (..),
    ExportPlayerStatsInitial (..),
    ExportPlayerStatsTotal (..),
    ExportPlayerStatsYear (..),
    GameReport (..),
    LeaguePlayer (..),
    MaybePlayerStats,
    Player (..),
    PlayerId,
    PlayerStats,
    PlayerStatsAggregate (..),
    PlayerStatsInitial (..),
    PlayerStatsTotal (..),
    PlayerStatsYear (..),
    ReportInsertion,
    StatAggregationPeriod (..),
    defaultPlayerStatsAllTime,
    defaultPlayerStatsTotal,
    defaultPlayerStatsYear,
    gameReportCsvHeader,
    leaguePlayerCsvHeader,
    playerCsvHeader,
    playerStatsInitialCsvHeader,
    playerStatsTotalCsvHeader,
    playerStatsYearCsvHeader,
    updatedPlayerStatsLose,
    updatedPlayerStatsWin,
  )
import Validation (validateLogFile, validateReport)
import Web.Cookie (SetCookie (..), defaultSetCookie, sameSiteStrict)
import Prelude hiding (get, on)

defaultRating :: Rating
defaultRating = 500

ratingThresholds :: IntMap (Rating, Rating)
ratingThresholds =
  fromList
    [ (10, (16, 16)),
      (33, (15, 17)),
      (56, (14, 18)),
      (79, (13, 19)),
      (102, (12, 20)),
      (126, (11, 21)),
      (151, (10, 22)),
      (178, (9, 23)),
      (207, (8, 24)),
      (236, (7, 25)),
      (270, (6, 26)),
      (308, (5, 27)),
      (352, (4, 28)),
      (409, (3, 29)),
      (499, (2, 30))
    ]

maxThreshold :: (Rating, Rating)
maxThreshold = (1, 31)

ratingAdjustment :: Rating -> Rating -> Rating
ratingAdjustment winner loser
  | winner >= loser = smallAdjust
  | otherwise = bigAdjust
  where
    diff = abs (winner - loser)
    (smallAdjust, bigAdjust) = maybe maxThreshold snd (IntMap.lookupGE diff ratingThresholds)

yearOf :: UTCTime -> Year
yearOf = (\(y, _, _) -> fromIntegral y) . toGregorian . utctDay

readStats :: PlayerId -> StatAggregationPeriod k -> MaybePlayerStats k -> PlayerStats k
readStats pid aggPeriod mStats = case mStats of
  (Nothing, Nothing) -> (defaultPlayerStatsTotal_, defaultPlayerStatsAgg)
  (Nothing, Just statsYear) -> (defaultPlayerStatsTotal_, statsYear)
  (Just statsTotal, Nothing) -> (statsTotal, defaultPlayerStatsAgg)
  (Just statsTotal, Just statsYear) -> (statsTotal, statsYear)
  where
    defaultPlayerStatsTotal_ = defaultPlayerStatsTotal pid
    defaultPlayerStatsAgg = case aggPeriod of
      Annual year -> defaultPlayerStatsYear pid year
      AllTime -> defaultPlayerStatsAllTime

readOrError :: (Monad m, MonadLogger m) => Text -> DBAction m (Maybe a) -> DBAction m a
readOrError errMsg action =
  action >>= \case
    Just a -> pure a
    Nothing -> do
      logErrorN errMsg
      throwError err404 {errBody = show errMsg}

toS3Key :: UTCTime -> PlayerName -> PlayerName -> S3.ObjectKey
toS3Key timestamp freePlayer shadowPlayer =
  S3.ObjectKey $ formattedPath <> formattedFilename
  where
    formattedPath = toText . formatTime defaultTimeLocale "%Y/%m/%d/" $ timestamp
    formattedTimestamp = toText . formatTime defaultTimeLocale "%Y-%m-%d_%H%M%S" $ timestamp
    formattedFilename = formattedTimestamp <> "_FP_" <> freePlayer <> "_SP_" <> shadowPlayer <> ".log"

toS3Url :: AWS.Region -> S3.ObjectKey -> S3Url
toS3Url region (S3.ObjectKey key) =
  "https://" <> S3.fromBucketName gameLogBucket <> ".s3." <> AWS.fromRegion region <> ".amazonaws.com/" <> key

putS3Object :: (MonadIO m) => AWS.Env -> S3.ObjectKey -> FilePath -> m S3.PutObjectResponse
putS3Object awsEnv key path =
  liftIO $
    AWS.chunkedFile AWS.defaultChunkSize path >>= AWS.runResourceT . AWS.send awsEnv . S3.newPutObject gameLogBucket key

insertReport :: (MonadIO m, MonadLogger m) => UTCTime -> RawGameReport -> Maybe S3Url -> DBAction m ReportInsertion
insertReport timestamp rawReport s3Url = do
  winner <- insertPlayerIfNotExists rawReport.winner Nothing
  loser <- insertPlayerIfNotExists rawReport.loser Nothing
  report <- insertGameReport $ toGameReport timestamp (entityKey winner) (entityKey loser) s3Url rawReport
  pure (report, winner, loser)

insertReport_ :: (MonadIO m, MonadLogger m) => UTCTime -> RawGameReport -> Maybe S3Url -> DBAction m ()
insertReport_ timestamp rawReport s3Url = insertReport timestamp rawReport s3Url >> pass

processReport :: (MonadIO m, MonadLogger m) => ReportInsertion -> DBAction m (ProcessedGameReport, Rating, Rating)
processReport (report@(Entity _ GameReport {..}), winnerPlayer@(Entity winnerId winner), loserPlayer@(Entity loserId loser)) = do
  let year = yearOf gameReportTimestamp
  let (winnerSide, loserSide) = (gameReportSide, other gameReportSide)

  (winnerStatsTotal, winnerStatsAgg) <-
    readStats winnerId (Annual year) <$> readOrError ("Missing stats for " <>: winner) (getPlayerStats winnerId year)
  (loserStatsTotal, loserStatsAgg) <-
    readStats loserId (Annual year) <$> readOrError ("Missing stats for " <>: loser) (getPlayerStats loserId year)

  let AnnualAgg winnerStatsYear = winnerStatsAgg
  let AnnualAgg loserStatsYear = loserStatsAgg

  let (winnerRatingOld, loserRatingOld) = (getRating winnerSide winnerStatsTotal, getRating loserSide loserStatsTotal)
  let adjustment = if gameReportMatch == Rated then ratingAdjustment winnerRatingOld loserRatingOld else 0
  let (winnerRating, loserRating) = (winnerRatingOld + adjustment, loserRatingOld - adjustment)

  logInfoN $ "Rating diff: " <>: adjustment
  logInfoN $ "Adjustment for " <> winner.playerDisplayName <> " (" <>: gameReportSide <> "): " <>: winnerRatingOld <> " -> " <>: winnerRating
  logInfoN $ "Adjustment for " <> loser.playerDisplayName <> " (" <>: other gameReportSide <> "): " <>: loserRatingOld <> " -> " <>: loserRating

  repsertPlayerStats $ updatedPlayerStatsWin winnerSide winnerRating winnerStatsTotal winnerStatsYear
  repsertPlayerStats $ updatedPlayerStatsLose loserSide loserRating loserStatsTotal loserStatsYear

  pure (fromGameReport (report, winnerPlayer, loserPlayer), winnerRating, loserRating)
  where
    other side = case side of Free -> Shadow; Shadow -> Free

    getRating side (PlayerStatsTotal {..}) = case side of
      Free -> playerStatsTotalRatingFree
      Shadow -> playerStatsTotalRatingShadow

reprocessReports :: (MonadIO m, MonadLogger m) => DBAction m ()
reprocessReports = do
  resetStats
  reports <- getAllGameReports OldestToNewest
  forM_ reports processReport
  updateActiveStatus

leaguePoints2025 :: League -> Int -> Int -> [(Int, Int)] -> Double
leaguePoints2025 league totalWins totalGames stats = baseScore + unplayedMultiplier * unplayedGames
  where
    baseScore = 0.1 * fromIntegral totalGames + fromIntegral totalWins
    winRate = fromIntegral totalWins / fromIntegral totalGames :: Double
    unplayedMultiplier
      | (league == GeneralLeague || league == LoMELeague) && totalGames < 14 = 0
      | (league == GeneralLeague || league == LoMELeague) && totalGames < 24 = 0.5 * winRate
      | league == GeneralLeague || league == LoMELeague = winRate
      | totalGames < 8 = 0
      | totalGames < 18 = 0.5 * winRate
      | otherwise = winRate
    unplayedGames = fromIntegral . sum . map (\(wins, losses) -> 2 - wins - losses) $ stats

-- Same as 2025 algorithm, but with adjustments to minimum game thresholds
leaguePoints2026 :: League -> Int -> Int -> [(Int, Int)] -> Double
leaguePoints2026 league totalWins totalGames stats = baseScore + unplayedMultiplier * unplayedGames
  where
    baseScore = 0.1 * fromIntegral totalGames + fromIntegral totalWins
    winRate = fromIntegral totalWins / fromIntegral totalGames :: Double
    unplayedMultiplier
      | (league == GeneralLeague || league == LoMELeague) && totalGames < 12 = 0
      | (league == GeneralLeague || league == LoMELeague) && totalGames < 18 = 0.5 * winRate
      | league == GeneralLeague || league == LoMELeague = winRate
      | totalGames < 8 = 0
      | totalGames < 12 = 0.5 * winRate
      | otherwise = winRate
    unplayedGames = fromIntegral . sum . map (\(wins, losses) -> 2 - wins - losses) $ stats

leaguePoints :: League -> LeagueTier -> Year -> Int -> Int -> [(Int, Int)] -> Double
leaguePoints league _ year totalWins totalGames stats
  | year == 2025 = leaguePoints2025 league totalWins totalGames stats
  | year == 2026 = leaguePoints2026 league totalWins totalGames stats
  | otherwise = 0

authGoogleLoginHandler :: IdToken -> AppM GoogleLoginResponse
authGoogleLoginHandler idToken = do
  httpConnMgr <- newManager
  jwks <- liftIO (fetchGoogleJWKSet httpConnMgr) >>= either (\err -> throwError err500 {errBody = show err}) pure -- TODO cache this
  userId <- liftIO (validateToken jwks idToken) >>= either (\err -> throwError err401 {errBody = show err}) pure
  sessionId <- liftIO $ UUID.toText <$> UUID.nextRandom
  count <- runAuthDb $ updateAdminSessionId userId (Just . SessionId $ sessionId)
  when (count == 0) (throwError err401 {errBody = "Non-admin login."})
  let cookie =
        defaultSetCookie
          { setCookieName = encodeUtf8 authCookieName,
            setCookieValue = encodeUtf8 sessionId,
            setCookieMaxAge = Just (60 * 60 * 24 * 365),
            setCookieHttpOnly = True,
            setCookieSecure = True,
            setCookieSameSite = Just sameSiteStrict,
            setCookieDomain = Just ".waroftheringcommunity.net",
            setCookiePath = Just "/"
          }
  pure (addHeader cookie NoContent)

logoutHandler :: AuthenticatedUser -> AppM NoContent
logoutHandler (AuthenticatedUser {userId}) = do
  _ <- runAuthDb $ updateAdminSessionId userId Nothing
  pure NoContent

userInfoHandler :: AppM UserInfoResponse
userInfoHandler = pure $ UserInfoResponse {isAdmin = True} -- True by default if this protected handler is reached

submitReportHandler :: SubmitReportRequest -> AppM SubmitGameReportResponse
submitReportHandler (SubmitReportRequest rawReport logFileData) = do
  awsEnv <- asks aws

  whenJust logFileData (validateLogFile . fdPayload)

  case validateReport rawReport of
    Failure errors -> throwError $ err422 {errBody = show errors}
    Success (RawGameReport {..}) -> runDb $ do
      logInfoN $ "Processing game between " <> winner <> " and " <> loser <> "."
      timestamp <- liftIO getCurrentTime
      let (freePlayer, shadowPlayer) = case side of Free -> (winner, loser); Shadow -> (loser, winner)
      let key = toS3Key timestamp freePlayer shadowPlayer
      let s3Path = toS3Url awsEnv.region key <$ logFileData
      (processedReport, winnerRating, loserRating) <- processReport =<< insertReport timestamp rawReport s3Path
      mapM_ (putS3Object awsEnv key . fdPayload) logFileData
      pure SubmitGameReportResponse {report = processedReport, winnerRating, loserRating}

getReportsHandler :: Maybe Int64 -> Maybe Int64 -> Maybe GameReportFilterSpec -> AppM GetReportsResponse
getReportsHandler limit offset filterSpec =
  runDb (getNumGameReports filterSpec) >>= \total ->
    runDb (getGameReports limit' offset' filterSpec) >>= \reports ->
      pure GetReportsResponse {reports = fromGameReport <$> reports, total}
  where
    maxLimit = 500
    (limit', offset') = case (limit, offset) of
      (Nothing, _) -> (maxLimit, 0)
      (Just lim, Nothing) -> (lim, 0)
      (Just lim, Just off) -> (lim, off)

getLeaderboardHandler :: Maybe Year -> AppM GetLeaderboardResponse
getLeaderboardHandler = \case
  Nothing -> go AllTime
  Just year -> go $ Annual year
  where
    go :: StatAggregationPeriod k -> AppM GetLeaderboardResponse
    go aggPeriod =
      runDb (getAllStats aggPeriod)
        <&> ( GetLeaderboardResponse
                . sortOn (Down . liftA2 (,) isActive averageRating)
                . map (fromPlayerStats . (\(player, stats) -> (player, readStats (entityKey player) aggPeriod stats)))
            )

getLeagueStatsHandler :: League -> LeagueTier -> Year -> AppM LeagueStatsResponse
getLeagueStatsHandler league tier year = do
  (summariesByPlayer, statsByPair) <- runDb $ do
    summaries <- getLeaguePlayerSummary league tier year
    stats <- getLeagueGameStats league tier year
    let summariesByPlayer =
          fmap (\(Value n, Value w, Value g) -> (n, w, g)) . Map.mapKeys unValue . fromList $ summaries
        statsByPair =
          fmap (fmap (\(Value oid, Value o, Value w, Value l) -> (oid, o, w, l)) . toList . fmap snd)
            . Map.mapKeys unValue
            . groupBy fst
            $ stats
    pure (summariesByPlayer, statsByPair)

  pure $
    Map.mapWithKey
      ( \playerId (name, totalWins, totalGames) ->
          LeaguePlayerStats
            { name,
              summary =
                LeaguePlayerStatsSummary
                  { totalWins,
                    totalGames,
                    points =
                      leaguePoints
                        league
                        tier
                        year
                        totalWins
                        totalGames
                        (map (\(_, _, a, b) -> (a, b)) . lookupDefault [] playerId $ statsByPair)
                  },
              gameStatsByOpponent = fromLeagueGameStatsMap playerId statsByPair
            }
      )
      summariesByPlayer

exportHandler :: AppM ExportResponse
exportHandler = do
  (players, gameReports, playerStatsYears, playerStatsTotals, playerStatsInits, leaguePlayers) <- runDb $ do
    players <- lift (selectList @Player [] [])
    gameReports <- fmap entityVal <$> lift (selectList @GameReport [] [])
    playerStatsYears <- fmap entityVal <$> lift (selectList @PlayerStatsYear [] [])
    playerStatsTotals <- fmap entityVal <$> lift (selectList @PlayerStatsTotal [] [])
    playerStatsInits <- fmap entityVal <$> lift (selectList @PlayerStatsInitial [] [])
    leaguePlayers <- fmap entityVal <$> lift (selectList @LeaguePlayer [] [])

    logInfoN "Extracting data for export..."
    logInfoN $ "Extracted Players: " <>: length players
    logInfoN $ "Extracted Reports: " <>: length gameReports
    logInfoN $ "Extracted PlayerStatsYears: " <>: length playerStatsYears
    logInfoN $ "Extracted PlayerStatsTotals: " <>: length playerStatsTotals
    logInfoN $ "Extracted PlayerStatsInits: " <>: length playerStatsInits
    logInfoN $ "Extracted LeaguePlayers: " <>: length leaguePlayers

    pure (players, gameReports, playerStatsYears, playerStatsTotals, playerStatsInits, leaguePlayers)

  let exportPlayers = entityVal <$> players
      playerNameMap = Map.fromList $ (\(Entity pid player) -> (pid, player.playerDisplayName)) <$> players
      lookupPlayerName pid = Map.findWithDefault "(unknown player)" pid playerNameMap
      exportGameReports =
        gameReports <&> \report@GameReport {..} ->
          ExportGameReport
            { exportGameReportRecord = report,
              exportGameReportWinnerName = lookupPlayerName gameReportWinnerId,
              exportGameReportLoserName = lookupPlayerName gameReportLoserId
            }
      exportPlayerStatsYears =
        playerStatsYears <&> \stats@PlayerStatsYear {..} ->
          ExportPlayerStatsYear
            { exportPlayerStatsYearRecord = stats,
              exportPlayerStatsYearPlayerName = lookupPlayerName playerStatsYearPlayerId
            }
      exportPlayerStatsTotals =
        playerStatsTotals <&> \stats@PlayerStatsTotal {..} ->
          ExportPlayerStatsTotal
            { exportPlayerStatsTotalRecord = stats,
              exportPlayerStatsTotalPlayerName = lookupPlayerName playerStatsTotalPlayerId
            }
      exportPlayerStatsInits =
        playerStatsInits <&> \stats@PlayerStatsInitial {..} ->
          ExportPlayerStatsInitial
            { exportPlayerStatsInitialRecord = stats,
              exportPlayerStatsInitialPlayerName = lookupPlayerName playerStatsInitialPlayerId
            }
      exportLeaguePlayers =
        leaguePlayers <&> \leaguePlayer@LeaguePlayer {..} ->
          ExportLeaguePlayer
            { exportLeaguePlayerRecord = leaguePlayer,
              exportLeaguePlayerPlayerName = lookupPlayerName leaguePlayerPlayerId
            }

      entries =
        [ toEntry "players.csv" 0 . encodeByName playerCsvHeader $ exportPlayers,
          toEntry "gameReports.csv" 0 . encodeByName gameReportCsvHeader $ exportGameReports,
          toEntry "playerStatsYears.csv" 0 . encodeByName playerStatsYearCsvHeader $ exportPlayerStatsYears,
          toEntry "playerStatsTotals.csv" 0 . encodeByName playerStatsTotalCsvHeader $ exportPlayerStatsTotals,
          toEntry "playerStatsInits.csv" 0 . encodeByName playerStatsInitialCsvHeader $ exportPlayerStatsInits,
          toEntry "leaguePlayers.csv" 0 . encodeByName leaguePlayerCsvHeader $ exportLeaguePlayers
        ]

      exportArchive = toChunks . fromArchive . foldr addEntryToArchive emptyArchive $ entries

  pure $ addHeader "attachment; filename=\"wotr-community-data.zip\"" (source exportArchive)

adminEditPlayerHandler :: EditPlayerRequest -> AppM NoContent
adminEditPlayerHandler EditPlayerRequest {pid, name, country} = runDb $ do
  player <- getPlayerByName name
  case player of
    Nothing -> updatePlayerName pid name
    Just p | entityKey p == pid -> updatePlayerName pid name
    Just _ -> throwError err422 {errBody = "Name " <>: name <> " already taken."}

  updatePlayerCountry pid country
  pure NoContent

adminRemapPlayerHandler :: RemapPlayerRequest -> AppM RemapPlayerResponse
adminRemapPlayerHandler RemapPlayerRequest {fromPid, toPid} = runDb $ do
  when (fromPid == toPid) (throwError err422 {errBody = "Cannot remap identical player IDs."})

  _ <- readOrError ("Cannot find player " <>: fromPid) $ lift . get $ fromPid
  player <- readOrError ("Cannot find player " <>: toPid) $ lift . get $ toPid

  updateCount <- updateReports fromPid toPid
  updateLeaguePlayer fromPid toPid
  deletePlayer fromPid
  -- Warning: a deleted player that existed in pre-2022 data will be reintroduced via reprocessReports
  when (updateCount > 0) reprocessReports

  pure $ RemapPlayerResponse player.playerDisplayName

adminModifyReportHandler :: ModifyReportRequest -> AppM NoContent
adminModifyReportHandler ModifyReportRequest {rid, timestamp, report} = case validateReport report of
  Failure errors -> throwError $ err422 {errBody = show errors}
  Success _ -> runDb $ do
    oldReport <- readOrError ("Cannot find report " <>: rid) $ lift . get $ rid
    Entity newWinnerId _ <- readOrError ("Cannot find player " <>: report.winner) $ getPlayerByName report.winner
    Entity newLoserId _ <- readOrError ("Cannot find player " <>: report.loser) $ getPlayerByName report.loser
    let newTimestamp = fromMaybe oldReport.gameReportTimestamp timestamp

    let newReport = toGameReport newTimestamp newWinnerId newLoserId oldReport.gameReportLogFile report
    lift $ replace rid newReport

    when (mustReprocess oldReport newReport) reprocessReports

    pure NoContent
  where
    mustReprocess :: GameReport -> GameReport -> Bool
    mustReprocess old new
      | old.gameReportTimestamp /= new.gameReportTimestamp = True
      | old.gameReportWinnerId /= new.gameReportWinnerId = True
      | old.gameReportLoserId /= new.gameReportLoserId = True
      | old.gameReportSide /= new.gameReportSide = True
      | old.gameReportMatch /= new.gameReportMatch = True
      | otherwise = False

adminDeleteReportHandler :: DeleteReportRequest -> AppM NoContent
adminDeleteReportHandler DeleteReportRequest {rid} = runDb $ do
  _ <- readOrError ("Cannot find report " <>: rid) $ lift . get $ rid
  deleteGameReport rid
  reprocessReports
  pure NoContent

adminAddLeaguePlayerHandler :: League -> LeagueTier -> Year -> Maybe Int64 -> Maybe PlayerName -> AppM NoContent
adminAddLeaguePlayerHandler _ _ _ Nothing Nothing =
  throwError err422 {errBody = "Either playerId or playerName must be provided."}
adminAddLeaguePlayerHandler _ _ _ (Just _) (Just _) = do
  throwError err422 {errBody = "Only one of playerId or playerName can be provided."}
adminAddLeaguePlayerHandler league tier year (Just playerId) Nothing = do
  runDb $ insertLeaguePlayer $ LeaguePlayer league tier year (toSqlKey playerId)
  pure NoContent
adminAddLeaguePlayerHandler league tier year Nothing (Just playerName) = do
  runDb $ do
    playerId <- entityKey <$> insertPlayerIfNotExists playerName Nothing
    insertLeaguePlayer $ LeaguePlayer league tier year playerId
  pure NoContent

updateActiveStatusHandler :: AppM NoContent
updateActiveStatusHandler = logInfoN "Updating player active statuses." >> runDb updateActiveStatus >> pure NoContent

unprotected :: ServerT Unprotected AppM
unprotected =
  authGoogleLoginHandler
    :<|> submitReportHandler
    :<|> getReportsHandler
    :<|> getLeaderboardHandler
    :<|> getLeagueStatsHandler
    :<|> exportHandler

service :: AuthServerData (AuthProtect ServiceCaller) -> ServerT Service AppM
service _ =
  updateActiveStatusHandler

protected :: AuthServerData (AuthProtect SessionIdCookie) -> ServerT Protected AppM
protected auth =
  logoutHandler auth
    :<|> userInfoHandler
    :<|> adminEditPlayerHandler
    :<|> adminRemapPlayerHandler
    :<|> adminModifyReportHandler
    :<|> adminDeleteReportHandler
    :<|> adminAddLeaguePlayerHandler

server :: ServerT API AppM
server = protected :<|> service :<|> unprotected
