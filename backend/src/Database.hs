module Database where

import Control.Monad.Logger (MonadLogger, logErrorN, logInfoN)
import Data.Time (Year)
import Database.Esqueleto.Experimental
  ( Entity (..),
    Key,
    SqlPersistT,
    desc,
    from,
    get,
    getBy,
    innerJoin,
    insert,
    insert_,
    on,
    orderBy,
    replace,
    select,
    selectOne,
    table,
    val,
    where_,
    (==.),
    (^.),
    type (:&) (..),
  )
import Logging ((<>:))
import Types.DataField (PlayerName)
import Types.Database
  ( EntityField (..),
    GameReport (..),
    Key (..),
    Player (..),
    PlayerId,
    PlayerStats (..),
    RatingDiff,
    Unique (..),
    currentYear,
    defaultPlayerStats,
    rolloverPlayerStats,
  )
import Prelude hiding (get, on)

getPlayerByName :: (MonadIO m, MonadLogger m) => PlayerName -> SqlPersistT m (Maybe (Entity Player))
getPlayerByName name = getBy $ UniquePlayerName name

insertPlayerIfNotExists :: (MonadIO m, MonadLogger m) => PlayerName -> SqlPersistT m (Key Player)
insertPlayerIfNotExists name =
  getPlayerByName name >>= \case
    Just (Entity playerKey _) -> pure playerKey
    Nothing -> do
      logInfoN $ "Adding new player " <> name <> " to database."
      playerKey <- insert $ Player name Nothing
      year <- currentYear
      insert_ $ defaultPlayerStats playerKey year
      pure playerKey

getStats :: (MonadIO m, MonadLogger m) => PlayerId -> Year -> SqlPersistT m PlayerStats
getStats pid year =
  get (PlayerStatsKey pid (fromIntegral year)) >>= \case
    Just stats -> pure stats
    Nothing -> do
      priorStats <- getMostRecentStats pid -- TODO Buggy sadness https://github.com/sirrus233/wotr-community-website/pull/31#discussion_r1897581825
      let priorYear = priorStats.playerStatsYear
      logInfoN $ "No stats for PID " <>: pid <> " in year " <>: year <> ". Rolling over from " <>: priorYear <> "."
      let stats = rolloverPlayerStats pid year priorStats
      insert_ stats
      pure stats

getMostRecentStats :: (MonadIO m, MonadLogger m) => PlayerId -> SqlPersistT m PlayerStats
getMostRecentStats pid =
  recentStats >>= \case
    Just (Entity _ stats) -> pure stats
    Nothing -> do
      logErrorN $ "Missing stats for PID " <>: pid <> ". Inserting defaults."
      year <- currentYear
      let stats = defaultPlayerStats pid year
      insert_ stats
      pure stats
  where
    recentStats = selectOne $ do
      stats <- from $ table @PlayerStats
      where_ (stats ^. PlayerStatsPlayerId ==. val pid)
      orderBy [desc (stats ^. PlayerStatsYear)]
      pure stats

replacePlayerStats :: (MonadIO m, MonadLogger m) => PlayerStats -> SqlPersistT m ()
replacePlayerStats stats@(PlayerStats {..}) = replace (PlayerStatsKey playerStatsPlayerId playerStatsYear) stats

insertRatingChange :: (MonadIO m, MonadLogger m) => RatingDiff -> SqlPersistT m ()
insertRatingChange = insert_

insertGameReport :: (MonadIO m, MonadLogger m) => GameReport -> SqlPersistT m (Key GameReport)
insertGameReport = insert

getGameReports :: (MonadIO m, MonadLogger m) => SqlPersistT m [(Entity GameReport, Entity Player, Entity Player)]
getGameReports =
  select $ do
    (report :& winner :& loser) <-
      from $
        table @GameReport
          `innerJoin` table @Player
            `on` (\(report :& winner) -> report ^. GameReportWinnerId ==. winner ^. PlayerId)
          `innerJoin` table @Player
            `on` (\(report :& _ :& loser) -> report ^. GameReportLoserId ==. loser ^. PlayerId)
    orderBy [desc (report ^. GameReportTimestamp)]
    pure (report, winner, loser)
