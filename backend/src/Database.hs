module Database where

import AppConfig (AppM, Env (..), runAppLogger)
import Control.Monad.Logger (LoggingT, MonadLogger, logInfoN)
import Data.Text qualified as T
import Database.Esqueleto.Experimental
  ( Entity (..),
    From,
    PersistStoreWrite (..),
    SqlExpr,
    SqlPersistT,
    case_,
    delete,
    desc,
    else_,
    from,
    getBy,
    innerJoin,
    just,
    leftJoin,
    limit,
    offset,
    on,
    orderBy,
    runSqlPool,
    select,
    selectOne,
    set,
    table,
    then_,
    update,
    val,
    when_,
    where_,
    (&&.),
    (=.),
    (==.),
    (?.),
    (^.),
    type (:&) (..),
  )
import Servant (ServerError, throwError)
import Types.DataField (PlayerName, Year)
import Types.Database
  ( EntityField (..),
    GameReport (..),
    Key (..),
    MaybePlayerStats,
    Player (..),
    PlayerId,
    PlayerStats,
    PlayerStatsInitial (..),
    PlayerStatsTotal (..),
    PlayerStatsYear (..),
    Unique (..),
    defaultPlayerStatsYear,
    toPlayerStatsTotal,
  )
import Types.Migration (ParsedLegacyLadderEntry (..))
import Prelude hiding (get, on)

type DBAction m = ExceptT ServerError (SqlPersistT m)

runDb :: DBAction (LoggingT IO) a -> AppM a
runDb dbAction = do
  env <- ask
  result <- liftIO . runAppLogger env.logger . runSqlPool (runExceptT dbAction) $ env.dbPool
  case result of
    Left err -> throwError err
    Right a -> pure a

normalizeName :: Text -> Text
normalizeName = T.toLower . T.strip

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

getAllGameReports :: (MonadIO m, MonadLogger m) => DBAction m [(Entity GameReport, Entity Player, Entity Player)]
getAllGameReports = lift . select $ do
  (report :& winner :& loser) <- from joinedGameReports
  orderBy [desc (report ^. GameReportTimestamp)]
  pure (report, winner, loser)

insertPlayerIfNotExists :: (MonadIO m, MonadLogger m) => PlayerName -> DBAction m (Entity Player)
insertPlayerIfNotExists name = do
  player <- getPlayerByName name
  case player of
    Just p -> pure p
    Nothing -> lift $ do
      logInfoN $ "Adding new player " <> normalizeName name <> " to database."
      let p = Player (normalizeName name) name Nothing
      pid <- insert p
      pure $ Entity pid p

insertInitialStats :: (MonadIO m, MonadLogger m) => PlayerStatsInitial -> DBAction m ()
insertInitialStats = lift . insert_

insertGameReport :: (MonadIO m, MonadLogger m) => GameReport -> DBAction m (Entity GameReport)
insertGameReport report = lift $ do
  rid <- insert report
  pure $ Entity rid report

insertLegacyEntry :: (MonadIO m, MonadLogger m) => ParsedLegacyLadderEntry -> DBAction m ()
insertLegacyEntry entry = do
  (Entity playerId _) <- insertPlayerIfNotExists entry.player

  let initialStats = PlayerStatsInitial playerId entry.freeRating entry.shadowRating entry.gamesPlayedTotal
  let totalStats = PlayerStatsTotal playerId entry.freeRating entry.shadowRating entry.gamesPlayedTotal
  let yearStats = defaultPlayerStatsYear playerId 2022

  insertInitialStats initialStats
  repsertPlayerStats (totalStats, yearStats)

repsertPlayerStats :: (MonadIO m, MonadLogger m) => PlayerStats -> DBAction m ()
repsertPlayerStats (totalStats@(PlayerStatsTotal {..}), yearStats@(PlayerStatsYear {..})) = lift $ do
  repsert (PlayerStatsTotalKey playerStatsTotalPlayerId) totalStats
  repsert (PlayerStatsYearKey playerStatsYearPlayerId playerStatsYearYear) yearStats

updatePlayerName :: (MonadIO m, MonadLogger m) => PlayerId -> PlayerName -> DBAction m ()
updatePlayerName pid name = lift $ do
  update $ \player -> do
    set player [PlayerName =. val (normalizeName name), PlayerDisplayName =. val name]
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

deletePlayer :: (MonadIO m, MonadLogger m) => PlayerId -> DBAction m ()
deletePlayer pid = lift $ do
  delete $ do
    player <- from $ table @Player
    where_ (player ^. PlayerId ==. val pid)

  delete $ do
    player <- from $ table @PlayerStatsInitial
    where_ (player ^. PlayerStatsInitialPlayerId ==. val pid)

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
