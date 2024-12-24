module Database where

import Data.Time (Year)
import Database.Esqueleto.Experimental
  ( Entity (..),
    Key,
    SqlPersistT,
    desc,
    from,
    orderBy,
    table,
    val,
    where_,
    (==.),
    (^.),
  )
import Database.Esqueleto.Experimental qualified as SQL
import Types.DataField (PlayerName)
import Types.Database
  ( EntityField (..),
    GameReport,
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

getPlayerByName :: (MonadIO m) => PlayerName -> SqlPersistT m (Maybe (Entity Player))
getPlayerByName name = SQL.getBy $ UniquePlayerName name

insertPlayerIfNotExists :: (MonadIO m) => PlayerName -> SqlPersistT m (Key Player)
insertPlayerIfNotExists name =
  getPlayerByName name >>= \case
    Just (Entity playerKey _) -> pure playerKey
    Nothing -> do
      playerKey <- SQL.insert $ Player name Nothing
      year <- currentYear
      SQL.insert_ $ defaultPlayerStats playerKey year
      pure playerKey

getStats :: (MonadIO m) => PlayerId -> Year -> SqlPersistT m PlayerStats
getStats pid year =
  SQL.get (PlayerStatsKey pid (fromIntegral year)) >>= \case
    Just stats -> pure stats
    Nothing -> do
      priorStats <- getMostRecentStats pid
      let stats = rolloverPlayerStats pid year priorStats
      SQL.insert_ stats
      pure stats

getMostRecentStats :: (MonadIO m) => PlayerId -> SqlPersistT m PlayerStats
getMostRecentStats pid =
  recentStats >>= \case
    Just (Entity _ stats) -> pure stats
    Nothing -> do
      year <- currentYear
      let stats = defaultPlayerStats pid year
      SQL.insert_ stats
      pure stats
  where
    recentStats = SQL.selectOne $ do
      stats <- from $ table @PlayerStats
      where_ (stats ^. PlayerStatsPlayerId ==. val pid)
      orderBy [desc (stats ^. PlayerStatsYear)]
      pure stats

replacePlayerStats :: (MonadIO m) => PlayerStats -> SqlPersistT m ()
replacePlayerStats stats@(PlayerStats {..}) = SQL.replace (PlayerStatsKey playerStatsPlayerId playerStatsYear) stats

insertRatingChange :: (MonadIO m) => RatingDiff -> SqlPersistT m ()
insertRatingChange = SQL.insert_

insertGameReport :: (MonadIO m) => GameReport -> SqlPersistT m (Key GameReport)
insertGameReport = SQL.insert
