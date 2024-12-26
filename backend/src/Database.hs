module Database where

import Control.Monad.Logger (MonadLogger, logErrorN, logInfoN)
import Data.Time (Year)
import Database.Esqueleto.Experimental
  ( Entity (..),
    Key,
    SqlPersistT,
    desc,
    from,
    just,
    leftJoin,
    on,
    orderBy,
    select,
    table,
    val,
    where_,
    (==.),
    (?.),
    (^.),
    type (:&) (..),
  )
import Database.Esqueleto.Experimental qualified as SQL
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
import Prelude hiding (on)

getPlayerByName :: (MonadIO m, MonadLogger m) => PlayerName -> SqlPersistT m (Maybe (Entity Player))
getPlayerByName name = SQL.getBy $ UniquePlayerName name

insertPlayerIfNotExists :: (MonadIO m, MonadLogger m) => PlayerName -> SqlPersistT m (Key Player)
insertPlayerIfNotExists name =
  getPlayerByName name >>= \case
    Just (Entity playerKey _) -> pure playerKey
    Nothing -> do
      logInfoN $ "Adding new player " <> name <> " to database."
      playerKey <- SQL.insert $ Player name Nothing
      year <- currentYear
      SQL.insert_ $ defaultPlayerStats playerKey year
      pure playerKey

getStats :: (MonadIO m, MonadLogger m) => PlayerId -> Year -> SqlPersistT m PlayerStats
getStats pid year =
  SQL.get (PlayerStatsKey pid (fromIntegral year)) >>= \case
    Just stats -> pure stats
    Nothing -> do
      priorStats <- getMostRecentStats pid -- TODO Buggy sadness https://github.com/sirrus233/wotr-community-website/pull/31#discussion_r1897581825
      let priorYear = priorStats.playerStatsYear
      logInfoN $ "No stats for PID " <>: pid <> " in year " <>: year <> ". Rolling over from " <>: priorYear <> "."
      let stats = rolloverPlayerStats pid year priorStats
      SQL.insert_ stats
      pure stats

getMostRecentStats :: (MonadIO m, MonadLogger m) => PlayerId -> SqlPersistT m PlayerStats
getMostRecentStats pid =
  recentStats >>= \case
    Just (Entity _ stats) -> pure stats
    Nothing -> do
      logErrorN $ "Missing stats for PID " <>: pid <> ". Inserting defaults."
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

replacePlayerStats :: (MonadIO m, MonadLogger m) => PlayerStats -> SqlPersistT m ()
replacePlayerStats stats@(PlayerStats {..}) = SQL.replace (PlayerStatsKey playerStatsPlayerId playerStatsYear) stats

insertRatingChange :: (MonadIO m, MonadLogger m) => RatingDiff -> SqlPersistT m ()
insertRatingChange = SQL.insert_

insertGameReport :: (MonadIO m, MonadLogger m) => GameReport -> SqlPersistT m (Key GameReport)
insertGameReport = SQL.insert

-- getGameReports :: (MonadIO m, MonadLogger m) => GameReport -> SqlPersistT m [(GameReport, Player)]
getGameReports =
  select $ do
    (reports :& players) <-
      from $
        table @GameReport
          `leftJoin` table @Player
            `on` ( \(reports :& players) ->
                     just (reports ^. GameReportWinnerId) ==. players ?. PlayerId
                 )
    pure (reports, players)

-- (reports :& players) <- from joinedTable
-- pure (reports, players)
-- where
--   joinedTable =
--     ((table @GameReport) `innerJoin` (table @Player)) `on` (\(reports :& winners) -> reports ^. GameReportWinnerId ==. winners ^. PlayerId)

-- pure (reports, winners, losers)

-- SQL.select $
--   from (\(reports `SQL.LeftOuterJoin` winner `SQL.LeftOuterJoin` loser) -> do
--     on (just (reports ^. GameReportLoserId) ==. loser ?. PlayerId)
--     on (just (reports ^. GameReportWinnerId) ==. winner ?. PlayerId)
--     pure (reports, winner, loser))

-- select $ do
-- (people :& blogPosts) <-
--     from $ table @Person
--     `leftJoin` table @BlogPost
--     `on` (\(people :& blogPosts) ->
--             just (people ^. PersonId) ==. blogPosts ?. BlogPostAuthorId)
-- where_ (people ^. PersonAge >. just (val 18))
-- pure (people, blogPosts)
