module Migration.Database where

import AppServer (normalizeName)
import Data.Time (UTCTime (utctDay), toGregorian)
import Database.Esqueleto.Experimental (PersistEntity (..), PersistStoreWrite (..), SqlPersistT, from, selectOne, table, val, where_, (&&.), (==.), (^.))
import Migration.Types (ParsedGameReport (..), ParsedLadderEntry (..), ParsedLegacyLadderEntry (..))
import Types.DataField (PlayerName, Side (..))
import Types.Database
  ( EntityField (..),
    GameReport (..),
    Player (..),
    PlayerId,
    PlayerStatsInitial (..),
    PlayerStatsTotal (..),
    PlayerStatsYear,
  )

insertLegacyEntry :: (MonadIO m) => ParsedLegacyLadderEntry -> SqlPersistT m (PlayerName, Key Player)
insertLegacyEntry entry = do
  playerId <- insertMigratedPlayer entry.player
  insertInitialStats playerId entry
  pure (entry.player, playerId)

insertMigratedPlayer :: (MonadIO m) => Text -> SqlPersistT m (Key Player)
insertMigratedPlayer name = do
  insert $ Player (normalizeName name) name Nothing

insertInitialStats :: (MonadIO m) => Key Player -> ParsedLegacyLadderEntry -> SqlPersistT m ()
insertInitialStats playerId entry = do
  insert_ $ PlayerStatsInitial playerId entry.freeRating entry.shadowRating entry.gamesPlayedTotal
  insert_ $ PlayerStatsTotal playerId entry.freeRating entry.shadowRating entry.gamesPlayedTotal

insertGameReport :: (MonadIO m) => PlayerId -> PlayerId -> ParsedGameReport -> SqlPersistT m ()
insertGameReport gameReportWinnerId gameReportLoserId (ParsedGameReport {..}) = insert_ $ GameReport {..}

-- let year = (\(y, _, _) -> fromIntegral y) . toGregorian . utctDay $ gameReportTimestamp
-- let losingSide = case gameReportSide of
--       Free -> Shadow
--       Shadow -> Free
-- pass
-- where
--   getYearStats pid year = selectOne $ do
--     yearStats <- from $ table @PlayerStatsYear
--     where_ (yearStats ^. PlayerStatsYearPlayerId ==. val pid &&. yearStats ^. PlayerStatsYearYear ==. val year)
--     pure yearStats

-- insert_ $ RatingDiff gameReportTimestamp gameReportWinnerId rid gameReportSide winnerRatingBefore winnerRatingAfter
-- insert_ $ RatingDiff gameReportTimestamp gameReportLoserId rid losingSide loserRatingBefore loserRatingAfter
