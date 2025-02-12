module Database where

import AppConfig (AppM, Env (..), runAppLogger)
import Control.Monad.Logger (LoggingT, MonadLogger, logInfoN)
import Data.Text qualified as T
import Data.Time (UTCTime (..), addUTCTime, fromGregorian, getCurrentTime, nominalDay)
import Database.Esqueleto.Experimental
  ( ConnectionPool,
    Entity (..),
    From,
    PersistStoreWrite (..),
    SqlExpr,
    SqlPersistT,
    Value (..),
    asc,
    case_,
    coalesceDefault,
    countRows,
    delete,
    desc,
    else_,
    from,
    getBy,
    groupBy,
    innerJoin,
    isNothing_,
    just,
    leftJoin,
    limit,
    not_,
    offset,
    on,
    orderBy,
    runSqlPool,
    select,
    selectOne,
    set,
    subSelectCount,
    subSelectUnsafe,
    sum_,
    table,
    then_,
    update,
    updateCount,
    val,
    when_,
    where_,
    (!=.),
    (&&.),
    (+.),
    (/.),
    (<.),
    (=.),
    (==.),
    (>=.),
    (?.),
    (^.),
    (||.),
    type (:&) (..),
  )
import Servant (ServerError, throwError)
import Types.Auth (SessionId (..), UserId (..))
import Types.DataField (League, LeagueTier, PlayerName, Year)
import Types.Database
  ( Admin,
    EntityField (..),
    GameReport (..),
    Key (..),
    LeagueGameStatsRecord,
    LeagueGameSummaryRecord,
    LeaguePlayer (..),
    MaybePlayerStats,
    Player (..),
    PlayerId,
    PlayerStats,
    PlayerStatsInitial (..),
    PlayerStatsTotal (..),
    PlayerStatsYear (..),
    Unique (..),
    toPlayerStatsTotal,
  )
import Prelude hiding (get, on)

type DBAction m = ExceptT ServerError (SqlPersistT m)

data SortOrder = OldestToNewest | NewestToOldest

runDbWithPool :: ConnectionPool -> DBAction (LoggingT IO) a -> AppM a
runDbWithPool pool dbAction = do
  env <- ask
  result <- liftIO . runAppLogger env.logger . runSqlPool (runExceptT dbAction) $ pool
  case result of
    Left err -> throwError err
    Right a -> pure a

runDb :: DBAction (LoggingT IO) a -> AppM a
runDb dbAction = asks dbPool >>= (`runDbWithPool` dbAction)

runAuthDb :: DBAction (LoggingT IO) a -> AppM a
runAuthDb dbAction = asks authDbPool >>= (`runDbWithPool` dbAction)

normalizeName :: Text -> Text
normalizeName = T.toLower . T.strip

getAdminBySessionId :: (MonadIO m, MonadLogger m) => Text -> DBAction m (Maybe (Entity Admin))
getAdminBySessionId sessionId = lift . getBy . UniqueAdminSessionId $ Just sessionId

getPlayerByName :: (MonadIO m, MonadLogger m) => PlayerName -> DBAction m (Maybe (Entity Player))
getPlayerByName = lift . getBy . UniquePlayerName . normalizeName

joinedPlayerStats ::
  Year ->
  From (SqlExpr (Entity Player) :& SqlExpr (Maybe (Entity PlayerStatsTotal)) :& SqlExpr (Maybe (Entity PlayerStatsYear)))
joinedPlayerStats year =
  table @Player
    `leftJoin` table @PlayerStatsTotal
      `on` (\(player :& totalStats) -> just (player ^. PlayerId) ==. totalStats ?. PlayerStatsTotalPlayerId)
    `leftJoin` table @PlayerStatsYear
      `on` ( \(player :& _ :& yearStats) ->
               just (player ^. PlayerId) ==. yearStats ?. PlayerStatsYearPlayerId
                 &&. (yearStats ?. PlayerStatsYearYear ==. just (val year))
           )

getPlayerStats :: (MonadIO m, MonadLogger m) => PlayerId -> Year -> DBAction m (Maybe MaybePlayerStats)
getPlayerStats pid year = lift . selectOne $ do
  (player :& totalStats :& yearStats) <- from $ joinedPlayerStats year
  where_ (player ^. PlayerId ==. val pid)
  pure (totalStats, yearStats)

getAllStats :: (MonadIO m, MonadLogger m) => Year -> DBAction m [(Entity Player, MaybePlayerStats)]
getAllStats year = do
  lift . select $ do
    (player :& totalStats :& yearStats) <- from $ joinedPlayerStats year
    pure (player, (totalStats, yearStats))

getInitialStats :: (MonadIO m, MonadLogger m) => DBAction m [Entity PlayerStatsInitial]
getInitialStats = lift . select $ from $ table @PlayerStatsInitial

joinedGameReports :: From (SqlExpr (Entity GameReport) :& SqlExpr (Entity Player) :& SqlExpr (Entity Player))
joinedGameReports =
  table @GameReport
    `innerJoin` table @Player
      `on` (\(report :& winner) -> report ^. GameReportWinnerId ==. winner ^. PlayerId)
    `innerJoin` table @Player
      `on` (\(report :& _ :& loser) -> report ^. GameReportLoserId ==. loser ^. PlayerId)

getGameReports :: (MonadIO m, MonadLogger m) => Int64 -> Int64 -> DBAction m [(Entity GameReport, Entity Player, Entity Player)]
getGameReports limit' offset' = lift . select $ do
  (report :& winner :& loser) <- from joinedGameReports
  orderBy [desc (report ^. GameReportTimestamp)]
  limit limit'
  offset offset'
  pure (report, winner, loser)

getAllGameReports :: (MonadIO m, MonadLogger m) => SortOrder -> DBAction m [(Entity GameReport, Entity Player, Entity Player)]
getAllGameReports sortOrder = lift . select $ do
  (report :& winner :& loser) <- from joinedGameReports
  let sortOrder' = case sortOrder of
        OldestToNewest -> asc
        NewestToOldest -> desc
  orderBy [sortOrder' (report ^. GameReportTimestamp)]
  pure (report, winner, loser)

getNumGameReports :: (MonadIO m, MonadLogger m) => DBAction m Int
getNumGameReports = do
  count <- lift . selectOne $ do
    _ <- from $ table @GameReport
    pure countRows
  pure $ unValue . fromMaybe (Value 0) $ count

joinedLeagueResults ::
  League ->
  LeagueTier ->
  Year ->
  From
    ( SqlExpr (Entity LeaguePlayer)
        :& SqlExpr (Entity LeaguePlayer)
        :& SqlExpr (Entity Player)
        :& SqlExpr (Entity Player)
        :& SqlExpr (Maybe (Entity GameReport))
    )
joinedLeagueResults league tier year =
  table @LeaguePlayer
    `innerJoin` table @LeaguePlayer
      `on` (\(leaguePlayer :& leagueOpponent) -> isInLeague leaguePlayer &&. isInLeague leagueOpponent)
    `innerJoin` table @Player
      `on` (\(leaguePlayer :& _ :& player) -> leaguePlayer ^. LeaguePlayerPlayerId ==. player ^. PlayerId)
    `innerJoin` table @Player
      `on` (\(_ :& leagueOpponent :& _ :& opponent) -> leagueOpponent ^. LeaguePlayerPlayerId ==. opponent ^. PlayerId)
    `leftJoin` table @GameReport
      `on` ( \(_ :& player :& opponent :& report) ->
               isGamePlayedBy report player opponent
                 &&. (report ?. GameReportLeague ==. just (val $ Just league))
                 &&. (report ?. GameReportTimestamp >=. just (val startTime))
                 &&. (report ?. GameReportTimestamp <. just (val endTime))
           )
  where
    startTime = UTCTime (fromGregorian (fromIntegral year) 1 1) 0
    endTime = UTCTime (fromGregorian (fromIntegral year + 1) 1 1) 0

    isInLeague lp =
      (lp ^. LeaguePlayerLeague ==. val league)
        &&. lp ^. LeaguePlayerTier ==. val tier
        &&. lp ^. LeaguePlayerYear ==. val year

    playerWon report player = report ?. GameReportWinnerId ==. just (player ^. PlayerId)
    playerLost report player = report ?. GameReportLoserId ==. just (player ^. PlayerId)

    isGamePlayedBy report player opponent =
      (playerWon report player &&. playerLost report opponent) ||. (playerWon report opponent &&. playerLost report player)

getLeaguePlayerSummary :: (MonadIO m, MonadLogger m) => League -> LeagueTier -> Year -> DBAction m [LeagueGameSummaryRecord]
getLeaguePlayerSummary league tier year = lift . select $ do
  (_ :& player :& opponent :& report) <- from $ joinedLeagueResults league tier year
  where_ $ (player ^. PlayerId) !=. (opponent ^. PlayerId)
  groupBy (player ^. PlayerId)
  let wins =
        coalesceDefault
          [sum_ $ case_ [when_ (playerWon report player) then_ (val (1 :: Int))] (else_ $ val 0)]
          (val 0)

      gameCount =
        coalesceDefault
          [sum_ $ case_ [when_ (not_ . isNothing_ $ report ?. GameReportId) then_ (val (1 :: Int))] (else_ $ val 0)]
          (val 0)

  pure (player ^. PlayerId, (player ^. PlayerDisplayName, wins, gameCount))
  where
    playerWon report player = report ?. GameReportWinnerId ==. just (player ^. PlayerId)

getLeagueGameStats :: (MonadIO m, MonadLogger m) => League -> LeagueTier -> Year -> DBAction m [LeagueGameStatsRecord]
getLeagueGameStats league tier year = lift . select $ do
  (_ :& player :& opponent :& report) <- from $ joinedLeagueResults league tier year
  where_ $ (player ^. PlayerId) !=. (opponent ^. PlayerId)
  groupBy (player ^. PlayerId, opponent ^. PlayerId)

  let wins =
        coalesceDefault
          [sum_ $ case_ [when_ (playerWon report player) then_ (val (1 :: Int))] (else_ (val 0))]
          (val 0)

      losses =
        coalesceDefault
          [sum_ $ case_ [when_ (playerWon report opponent) then_ (val (1 :: Int))] (else_ (val 0))]
          (val 0)

  pure (player ^. PlayerId, (opponent ^. PlayerId, opponent ^. PlayerDisplayName, wins, losses))
  where
    playerWon report player = report ?. GameReportWinnerId ==. just (player ^. PlayerId)

insertPlayerIfNotExists :: (MonadIO m, MonadLogger m) => PlayerName -> Maybe Text -> DBAction m (Entity Player)
insertPlayerIfNotExists name country = do
  player <- getPlayerByName name
  case player of
    Just p -> pure p
    Nothing -> lift $ do
      logInfoN $ "Adding new player " <> normalizeName name <> " to database."
      let p = Player (normalizeName name) name country False
      pid <- insert p
      pure $ Entity pid p

insertInitialStats :: (MonadIO m, MonadLogger m) => PlayerStatsInitial -> DBAction m ()
insertInitialStats = lift . insert_

insertGameReport :: (MonadIO m, MonadLogger m) => GameReport -> DBAction m (Entity GameReport)
insertGameReport report = lift $ do
  rid <- insert report
  pure $ Entity rid report

insertLeaguePlayer :: (MonadIO m, MonadLogger m) => LeaguePlayer -> DBAction m ()
insertLeaguePlayer = lift . insert_

repsertPlayerStats :: (MonadIO m, MonadLogger m) => PlayerStats -> DBAction m ()
repsertPlayerStats (totalStats@(PlayerStatsTotal {..}), yearStats@(PlayerStatsYear {..})) = lift $ do
  repsert (PlayerStatsTotalKey playerStatsTotalPlayerId) totalStats
  repsert (PlayerStatsYearKey playerStatsYearPlayerId playerStatsYearYear) yearStats

updatePlayerName :: (MonadIO m, MonadLogger m) => PlayerId -> PlayerName -> DBAction m ()
updatePlayerName pid name = lift $ do
  update $ \player -> do
    set player [PlayerName =. val (normalizeName name), PlayerDisplayName =. val name]
    where_ (player ^. PlayerId ==. val pid)

updatePlayerCountry :: (MonadIO m, MonadLogger m) => PlayerId -> Text -> DBAction m ()
updatePlayerCountry pid country = lift $ do
  update $ \player -> do
    set player [PlayerCountry =. just (val country)]
    where_ (player ^. PlayerId ==. val pid)

updateReports :: (MonadIO m, MonadLogger m) => PlayerId -> PlayerId -> DBAction m ()
updateReports fromPid toPid = lift $ do
  update $ \report -> do
    set
      report
      [ GameReportWinnerId
          =. case_
            [when_ (report ^. GameReportWinnerId ==. val fromPid) then_ (val toPid)]
            (else_ (report ^. GameReportWinnerId)),
        GameReportLoserId
          =. case_
            [when_ (report ^. GameReportLoserId ==. val fromPid) then_ (val toPid)]
            (else_ (report ^. GameReportLoserId))
      ]

updateActiveStatus :: (MonadIO m, MonadLogger m) => DBAction m ()
updateActiveStatus = do
  now <- liftIO getCurrentTime
  let cutoffDay = addUTCTime (negate $ 365 * nominalDay) now
      stdActiveRequirement = 4 :: Int
      highActiveRequirement = 12 :: Int
      highActiveThreshold = 700 :: Int

  lift $ update $ \player -> do
    let activeGames = countReports player cutoffDay
    let avgRating = playerAvgRating player
    set
      player
      [ PlayerIsActive
          =. case_
            [ when_ ((avgRating >=. val highActiveThreshold) &&. (activeGames <. val highActiveRequirement)) then_ (val False),
              when_ (activeGames <. val stdActiveRequirement) then_ (val False)
            ]
            (else_ val True)
      ]
  where
    countReports player cutoffDay =
      subSelectCount $ do
        report <- from $ table @GameReport
        where_
          ( ( (report ^. GameReportWinnerId ==. player ^. PlayerId)
                ||. (report ^. GameReportLoserId ==. player ^. PlayerId)
            )
              &&. report ^. GameReportTimestamp >=. val cutoffDay
          )

    playerAvgRating player =
      -- Should only be a single stats entry per player, which would make this safe
      subSelectUnsafe $ do
        stats <- from $ table @PlayerStatsTotal
        where_ (stats ^. PlayerStatsTotalPlayerId ==. player ^. PlayerId)
        let freeRating = stats ^. PlayerStatsTotalRatingFree
            shadowRating = stats ^. PlayerStatsTotalRatingShadow
        pure $ (freeRating +. shadowRating) /. val 2

updateAdminSessionId :: (MonadIO m, MonadLogger m) => UserId -> Maybe SessionId -> DBAction m Int64
updateAdminSessionId (UserId userId) sessionId = lift $ updateCount $ \admin -> do
  set admin [AdminSessionId =. val (unSessionId <$> sessionId)]
  where_ (admin ^. AdminUserId ==. val userId)

deletePlayer :: (MonadIO m, MonadLogger m) => PlayerId -> DBAction m ()
deletePlayer pid = lift $ do
  delete $ do
    player <- from $ table @Player
    where_ (player ^. PlayerId ==. val pid)

deleteGameReport :: (MonadIO m, MonadLogger m) => Key GameReport -> DBAction m ()
deleteGameReport rid = lift $ do
  delete $ do
    report <- from $ table @GameReport
    where_ (report ^. GameReportId ==. val rid)

deleteStats :: (MonadIO m, MonadLogger m) => DBAction m ()
deleteStats = lift $ do
  delete $ do
    _ <- from $ table @PlayerStatsTotal
    pass
  delete $ do
    _ <- from $ table @PlayerStatsYear
    pass

resetStats :: (MonadIO m, MonadLogger m) => DBAction m ()
resetStats = do
  deleteStats
  initialStats <- getInitialStats
  lift $ insertMany_ (map (toPlayerStatsTotal . entityVal) initialStats)
