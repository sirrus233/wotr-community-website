module Database where

import AppConfig (AppM, Env (..), runAppLogger)
import Control.Monad.Logger (LoggingT, MonadLogger, logInfoN)
import Database.Esqueleto.Experimental
  ( Entity (..),
    From,
    Key,
    SqlExpr,
    SqlPersistT,
    desc,
    from,
    getBy,
    innerJoin,
    insert,
    insert_,
    limit,
    on,
    orderBy,
    replace,
    runSqlPool,
    select,
    selectOne,
    table,
    val,
    where_,
    (&&.),
    (==.),
    (^.),
    type (:&) (..),
  )
import Servant (ServerError, throwError)
import Types.DataField (PlayerName, Year)
import Types.Database
  ( EntityField (..),
    GameReport (..),
    Key (..),
    Player (..),
    PlayerId,
    PlayerStatsTotal (..),
    PlayerStatsYear (..),
    RatingDiff,
    Unique (..),
    currentYear,
    defaultPlayerStats,
  )
import Prelude hiding (get, on)

type DBAction m = ExceptT ServerError (SqlPersistT m)

runDb :: DBAction (LoggingT IO) a -> AppM a
runDb dbAction = do
  env <- ask
  result <- liftIO . runAppLogger env.logger . runSqlPool (runExceptT dbAction) $ env.dbPool
  case result of
    Left err -> throwError err
    Right a -> pure a

getPlayerByName :: (MonadIO m, MonadLogger m) => PlayerName -> DBAction m (Maybe (Entity Player))
getPlayerByName = lift . getBy . UniquePlayerName

insertPlayerIfNotExists :: (MonadIO m, MonadLogger m) => PlayerName -> DBAction m (Key Player)
insertPlayerIfNotExists name = do
  player <- getPlayerByName name
  case player of
    Just (Entity playerKey _) -> pure playerKey
    Nothing -> lift $ do
      logInfoN $ "Adding new player " <> name <> " to database."
      playerKey <- insert $ Player name Nothing
      year <- currentYear
      let (totalStats, yearStats) = defaultPlayerStats playerKey year
      insert_ totalStats
      insert_ yearStats
      pure playerKey

joinedPlayerStats :: Year -> From (SqlExpr (Entity Player) :& SqlExpr (Entity PlayerStatsTotal) :& SqlExpr (Entity PlayerStatsYear))
joinedPlayerStats year =
  table @Player
    `innerJoin` table @PlayerStatsTotal
      `on` (\(player :& totalStats) -> player ^. PlayerId ==. totalStats ^. PlayerStatsTotalPlayerId)
    `innerJoin` table @PlayerStatsYear
      `on` ( \(player :& _ :& yearStats) ->
               (player ^. PlayerId ==. yearStats ^. PlayerStatsYearPlayerId)
                 &&. (yearStats ^. PlayerStatsYearYear ==. val year)
           )

getPlayerStats :: (MonadIO m, MonadLogger m) => PlayerId -> DBAction m (Maybe (Entity PlayerStatsTotal, Entity PlayerStatsYear))
getPlayerStats pid = do
  year <- currentYear
  lift . selectOne $ do
    (player :& totalStats :& yearStats) <- from $ joinedPlayerStats year
    where_ (player ^. PlayerId ==. val pid)
    pure (totalStats, yearStats)

getAllStats :: (MonadIO m, MonadLogger m) => DBAction m [(Entity Player, Entity PlayerStatsTotal, Entity PlayerStatsYear)]
getAllStats = do
  year <- currentYear
  lift . select $ do
    (player :& totalStats :& yearStats) <- from $ joinedPlayerStats year
    pure (player, totalStats, yearStats)

replacePlayerStats :: (MonadIO m, MonadLogger m) => PlayerStatsTotal -> PlayerStatsYear -> DBAction m ()
replacePlayerStats totalStats@(PlayerStatsTotal {..}) yearStats@(PlayerStatsYear {..}) = lift $ do
  replace (PlayerStatsTotalKey playerStatsTotalPlayerId) totalStats
  replace (PlayerStatsYearKey playerStatsYearPlayerId playerStatsYearYear) yearStats

insertRatingChange :: (MonadIO m, MonadLogger m) => RatingDiff -> DBAction m ()
insertRatingChange = lift . insert_

insertGameReport :: (MonadIO m, MonadLogger m) => GameReport -> DBAction m (Key GameReport)
insertGameReport = lift . insert

getGameReports :: (MonadIO m, MonadLogger m) => DBAction m [(Entity GameReport, Entity Player, Entity Player)]
getGameReports = lift . select $ do
  (report :& winner :& loser) <-
    from $
      table @GameReport
        `innerJoin` table @Player
          `on` (\(report :& winner) -> report ^. GameReportWinnerId ==. winner ^. PlayerId)
        `innerJoin` table @Player
          `on` (\(report :& _ :& loser) -> report ^. GameReportLoserId ==. loser ^. PlayerId)
  orderBy [desc (report ^. GameReportTimestamp)]
  limit 500
  pure (report, winner, loser)
