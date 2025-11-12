module Database where

import AppConfig (AppM, Env (..), runAppLogger)
import Control.Monad.Logger (LoggingT, MonadLogger, logInfoN)
import Data.Foldable1 (foldr1)
import Data.Text qualified as T
import Data.Time (UTCTime (..), addUTCTime, fromGregorian, getCurrentTime, nominalDay)
import Database.Esqueleto.Experimental
  ( ConnectionPool,
    Entity (..),
    From,
    PersistEntity,
    PersistField,
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
    in_,
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
    valList,
    when_,
    where_,
    (!=.),
    (&&.),
    (+.),
    (/.),
    (<.),
    (<=.),
    (=.),
    (==.),
    (>.),
    (>=.),
    (?.),
    (^.),
    (||.),
    type (:&) (..),
  )
import Relude.Extra (bimapF, secondF)
import Servant (ServerError, throwError)
import Types.Api (GameReportFilterSpec (..), InequalityFilter (..), NullableFilter (..), TimestampFilter (..), VictoryFilter (..))
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

toFilterExpression ::
  SqlExpr (Entity GameReport) ->
  GameReportFilterSpec ->
  SqlExpr (Value Bool)
toFilterExpression report spec = foldr ((&&.) . fromMaybe (val True)) (val True) filterList
  where
    toInequalityFilter ::
      (PersistEntity e) =>
      SqlExpr (Entity e) -> EntityField e Int -> InequalityFilter -> SqlExpr (Value Bool)
    toInequalityFilter entity field = \case
      InequalityFilter LT a -> (entity ^. field) <. val a
      InequalityFilter GT a -> (entity ^. field) >. val a
      InequalityFilter EQ a -> (entity ^. field) ==. val a
    toNullableInequalityFilter ::
      (PersistEntity e) =>
      SqlExpr (Entity e) -> EntityField e (Maybe Int) -> NullableFilter InequalityFilter -> SqlExpr (Value Bool)
    toNullableInequalityFilter entity field = \case
      NullFilter -> isNothing_ (entity ^. field)
      ValueFilter i -> case i of
        InequalityFilter LT a -> (entity ^. field) <. (just . val $ a)
        InequalityFilter GT a -> (entity ^. field) >. (just . val $ a)
        InequalityFilter EQ a -> (entity ^. field) ==. (just . val $ a)
    toListFilter ::
      (PersistEntity e, PersistField f) =>
      SqlExpr (Entity e) -> EntityField e f -> [f] -> SqlExpr (Value Bool)
    toListFilter entity field = ((entity ^. field) `in_`) . valList
    filterList :: [Maybe (SqlExpr (Value Bool))]
    filterList =
      [ playerFilter,
        pairingFilter,
        timestampFilter,
        winnerFilter,
        loserFilter,
        turnsFilter,
        victoryFilter,
        leagueFilter,
        tokensFilter,
        dwarvenRings,
        musterPoints,
        corruption,
        mordor,
        aragorn,
        treebeard,
        initialEyes,
        interestRating,
        hasLog
      ]
    playerFilter = liftA2 (||.) (toListFilter report GameReportWinnerId <$> spec.players) (toListFilter report GameReportLoserId <$> spec.players)
    pairingFilter =
      spec.pairing <&> \case
        (player1, Nothing) -> (report ^. GameReportWinnerId ==. val player1) ||. (report ^. GameReportLoserId ==. val player1)
        (player1, Just player2) ->
          ((report ^. GameReportWinnerId ==. val player1) ||. (report ^. GameReportLoserId ==. val player1))
            &&. ((report ^. GameReportWinnerId ==. val player2) ||. (report ^. GameReportLoserId ==. val player2))
    timestampFilter =
      spec.timestamp <&> \case
        Before t -> report ^. GameReportTimestamp <=. val t
        After t -> report ^. GameReportTimestamp >=. val t
        Between start end -> (report ^. GameReportTimestamp >=. val start) &&. (report ^. GameReportTimestamp <=. val end)
    winnerFilter = toListFilter report GameReportWinnerId <$> spec.winners
    loserFilter = toListFilter report GameReportLoserId <$> spec.losers
    turnsFilter = toInequalityFilter report GameReportTurns <$> spec.turns
    victoryFilter =
      spec.victory
        <&> foldr1 (||.)
        . fmap
          ( \case
              VictoryKindFilter k -> report ^. GameReportVictory ==. val k
              VictorySideFilter s -> report ^. GameReportSide ==. val s
              VictoryComboFilter s k -> (report ^. GameReportSide) ==. val s &&. (report ^. GameReportVictory) ==. val k
          )
    leagueFilter =
      spec.leagues <&> \case
        [] -> isNothing_ (report ^. GameReportLeague)
        leagues -> toListFilter report GameReportLeague . map Just $ leagues
    tokensFilter = toInequalityFilter report GameReportActionTokens <$> spec.tokens
    dwarvenRings = toInequalityFilter report GameReportDwarvenRings <$> spec.dwarvenRings
    musterPoints = toInequalityFilter report GameReportMusterPoints <$> spec.musterPoints
    corruption = toInequalityFilter report GameReportCorruption <$> spec.corruption
    mordor = toNullableInequalityFilter report GameReportMordor <$> spec.mordor
    aragorn = toNullableInequalityFilter report GameReportAragornTurn <$> spec.aragorn
    treebeard =
      spec.treebeard <&> \case
        True -> (report ^. GameReportTreebeard) ==. (just . val $ True)
        False -> (report ^. GameReportTreebeard) ==. (just . val $ False) ||. isNothing_ (report ^. GameReportTreebeard)
    initialEyes = toInequalityFilter report GameReportInitialEyes <$> spec.initialEyes
    interestRating = toInequalityFilter report GameReportInterestRating <$> spec.interestRating
    hasLog =
      spec.hasLog <&> \case
        True -> not_ . isNothing_ $ (report ^. GameReportLogFile)
        False -> isNothing_ (report ^. GameReportLogFile)

getAdminBySessionId :: (MonadIO m, MonadLogger m) => Text -> DBAction m (Maybe (Entity Admin))
getAdminBySessionId sessionId = lift . getBy . UniqueAdminSessionId $ Just sessionId

getPlayerByName :: (MonadIO m, MonadLogger m) => PlayerName -> DBAction m (Maybe (Entity Player))
getPlayerByName = lift . getBy . UniquePlayerName . normalizeName

joinedYearlyPlayerStats ::
  Year ->
  From (SqlExpr (Entity Player) :& SqlExpr (Maybe (Entity PlayerStatsTotal)) :& SqlExpr (Maybe (Entity PlayerStatsYear)))
joinedYearlyPlayerStats year =
  table @Player
    `leftJoin` table @PlayerStatsTotal
      `on` (\(player :& totalStats) -> just (player ^. PlayerId) ==. totalStats ?. PlayerStatsTotalPlayerId)
    `leftJoin` table @PlayerStatsYear
      `on` ( \(player :& _ :& yearStats) ->
               just (player ^. PlayerId) ==. yearStats ?. PlayerStatsYearPlayerId
                 &&. yearStats ?. PlayerStatsYearYear ==. just (val year)
           )

joinedTotalPlayerStats ::
  From (SqlExpr (Entity Player) :& SqlExpr (Maybe (Entity PlayerStatsInitial)) :& SqlExpr (Maybe (Entity PlayerStatsTotal)) :& SqlExpr (Maybe (Entity PlayerStatsYear)))
joinedTotalPlayerStats =
  table @Player
    `leftJoin` table @PlayerStatsInitial
      `on` (\(player :& initialStats) -> just (player ^. PlayerId) ==. initialStats ?. PlayerStatsInitialPlayerId)
    `leftJoin` table @PlayerStatsTotal
      `on` (\(player :& _ :& totalStats) -> just (player ^. PlayerId) ==. totalStats ?. PlayerStatsTotalPlayerId)
    `leftJoin` table @PlayerStatsYear
      `on` (\(player :& _ :& _ :& yearStats) -> just (player ^. PlayerId) ==. yearStats ?. PlayerStatsYearPlayerId)

getPlayerStats :: (MonadIO m, MonadLogger m) => PlayerId -> Year -> DBAction m (Maybe MaybePlayerStats)
getPlayerStats pid year = do
  rows <- lift . selectOne $ do
    (player :& totalStats :& yearStats) <- from $ joinedYearlyPlayerStats year
    where_ (player ^. PlayerId ==. val pid)
    pure (totalStats, yearStats)
  pure $ bimapF (fmap entityVal) (fmap entityVal) rows

getAllStatsByYear :: (MonadIO m, MonadLogger m) => Year -> DBAction m [(Entity Player, MaybePlayerStats)]
getAllStatsByYear year = do
  rows <- lift . select $ do
    (player :& totalStats :& yearStats) <- from $ joinedYearlyPlayerStats year
    pure (player, (totalStats, yearStats))
  pure $ secondF (bimap (fmap entityVal) (fmap entityVal)) rows

getAllStatsTotal :: (MonadIO m, MonadLogger m) => DBAction m [(Entity Player, (Maybe PlayerStatsInitial, MaybePlayerStats))]
getAllStatsTotal = do
  rows <- lift . select $ do
    (player :& initialStats :& totalStats :& yearStats) <- from joinedTotalPlayerStats
    groupBy (yearStats ?. PlayerStatsYearPlayerId)
    pure
      ( player,
        ( totalStats,
          initialStats,
          ( yearStats ?. PlayerStatsYearPlayerId,
            just $ val 0,
            sum_ (yearStats ?. PlayerStatsYearWinsFree),
            sum_ (yearStats ?. PlayerStatsYearWinsShadow),
            sum_ (yearStats ?. PlayerStatsYearLossesFree),
            sum_ (yearStats ?. PlayerStatsYearLossesShadow)
          )
        )
      )
  pure $
    fmap
      ( \(p, (t, i, (Value pid, Value year, Value winsFree, Value winsShadow, Value lossesFree, Value lossesShadow))) ->
          ( p,
            ( fmap entityVal i,
              ( fmap entityVal t,
                PlayerStatsYear <$> pid <*> year <*> winsFree <*> winsShadow <*> lossesFree <*> lossesShadow
              )
            )
          )
      )
      rows

getInitialStats :: (MonadIO m, MonadLogger m) => DBAction m [Entity PlayerStatsInitial]
getInitialStats = lift . select $ from $ table @PlayerStatsInitial

joinedGameReports :: From (SqlExpr (Entity GameReport) :& SqlExpr (Entity Player) :& SqlExpr (Entity Player))
joinedGameReports =
  table @GameReport
    `innerJoin` table @Player
      `on` (\(report :& winner) -> report ^. GameReportWinnerId ==. winner ^. PlayerId)
    `innerJoin` table @Player
      `on` (\(report :& _ :& loser) -> report ^. GameReportLoserId ==. loser ^. PlayerId)

getGameReports ::
  (MonadIO m, MonadLogger m) =>
  Int64 ->
  Int64 ->
  Maybe GameReportFilterSpec ->
  DBAction m [(Entity GameReport, Entity Player, Entity Player)]
getGameReports limit' offset' filterSpec = lift . select $ do
  (report :& winner :& loser) <- from joinedGameReports
  orderBy [desc (report ^. GameReportTimestamp)]
  limit limit'
  offset offset'
  for_ filterSpec (where_ . toFilterExpression report)
  pure (report, winner, loser)

getAllGameReports :: (MonadIO m, MonadLogger m) => SortOrder -> DBAction m [(Entity GameReport, Entity Player, Entity Player)]
getAllGameReports sortOrder = lift . select $ do
  (report :& winner :& loser) <- from joinedGameReports
  let sortOrder' = case sortOrder of
        OldestToNewest -> asc
        NewestToOldest -> desc
  orderBy [sortOrder' (report ^. GameReportTimestamp)]
  pure (report, winner, loser)

getNumGameReports :: (MonadIO m, MonadLogger m) => Maybe GameReportFilterSpec -> DBAction m Int
getNumGameReports filterSpec = do
  count <- lift . selectOne $ do
    report <- from $ table @GameReport
    for_ filterSpec (where_ . toFilterExpression report)
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

updatePlayerCountry :: (MonadIO m, MonadLogger m) => PlayerId -> Maybe Text -> DBAction m ()
updatePlayerCountry pid country = lift $ do
  update $ \player -> do
    set player [PlayerCountry =. val country]
    where_ (player ^. PlayerId ==. val pid)

updateReports :: (MonadIO m, MonadLogger m) => PlayerId -> PlayerId -> DBAction m Int64
updateReports fromPid toPid = lift $ do
  updateCount $ \report -> do
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

updateLeaguePlayer :: (MonadIO m, MonadLogger m) => PlayerId -> PlayerId -> DBAction m ()
updateLeaguePlayer fromPid toPid = lift $ do
  leagues <- select $ do
    league <- from $ table @LeaguePlayer
    where_ (league ^. LeaguePlayerPlayerId ==. val fromPid)
    pure league

  repsertMany $
    map
      ( \(Entity _ lp) ->
          ( LeaguePlayerKey lp.leaguePlayerLeague lp.leaguePlayerTier lp.leaguePlayerYear toPid,
            lp {leaguePlayerPlayerId = toPid}
          )
      )
      leagues

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
