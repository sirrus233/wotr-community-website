module Database where

import Data.Time (UTCTime (..), getCurrentTime, toGregorian)
import Database.Esqueleto.Experimental (Entity, Key, SqlPersistT, entityKey)
import Database.Esqueleto.Experimental qualified as SQL
import Types.DataField (PlayerName)
import Types.Database
  ( GameReport,
    GameReportId,
    Key (..),
    Player (..),
    PlayerId,
    PlayerStats (..),
    RatingDiff,
    Unique (..),
    defaultPlayerStats,
  )

getPlayerByName :: (MonadIO m) => PlayerName -> SqlPersistT m (Maybe (Entity Player))
getPlayerByName name = SQL.getBy $ UniquePlayerName name

insertPlayerIfNotExists :: (MonadIO m) => PlayerName -> SqlPersistT m (Key Player)
insertPlayerIfNotExists name =
  getPlayerByName name >>= \case
    Just player -> pure $ entityKey player
    Nothing -> do
      playerKey <- SQL.insert $ Player name Nothing
      (year, _, _) <- liftIO $ toGregorian . utctDay <$> getCurrentTime
      SQL.insert_ $ defaultPlayerStats playerKey year
      pure playerKey

getCurrentStats :: (MonadIO m) => PlayerId -> SqlPersistT m (Maybe PlayerStats)
getCurrentStats pid = do
  (year, _, _) <- liftIO $ toGregorian . utctDay <$> getCurrentTime
  SQL.get $ PlayerStatsKey pid (fromIntegral year)

insertRatingChange :: (MonadIO m) => RatingDiff -> SqlPersistT m ()
insertRatingChange = SQL.insert_

insertGameReport :: (MonadIO m) => GameReport -> SqlPersistT m GameReportId
insertGameReport = SQL.insert
