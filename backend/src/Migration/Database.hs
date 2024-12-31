module Migration.Database where

import Database.Esqueleto.Experimental (PersistEntity (..), PersistStoreWrite (..), SqlPersistT)
import Migration.Types (LadderEntry (..), ParsedGameReport (..))
import Types.DataField (PlayerName, Side (..))
import Types.Database (GameReport (..), Player (..), PlayerStatsTotal (..), PlayerStatsYear (..), RatingDiff (..))

insertPlayer :: (MonadIO m) => Text -> SqlPersistT m (Key Player)
insertPlayer name = insert $ Player name Nothing

insertStats :: (MonadIO m) => Key Player -> LadderEntry -> SqlPersistT m ()
insertStats playerId entry = do
  insert_ $ PlayerStatsTotal playerId entry.freeRating entry.shadowRating entry.gamesPlayedTotal
  insert_ $ PlayerStatsYear playerId 2024 entry.fpWins entry.spWins entry.fpLoss entry.spLoss

insertEntry :: (MonadIO m) => LadderEntry -> SqlPersistT m (PlayerName, Key Player)
insertEntry entry = do
  playerId <- insertPlayer entry.player
  insertStats playerId entry
  pure (entry.player, playerId)

insertGameReport :: (MonadIO m) => ParsedGameReport -> SqlPersistT m ()
insertGameReport (ParsedGameReport {..}) = do
  rid <- insert $ GameReport {..}
  let losingSide = case gameReportSide of
        Free -> Shadow
        Shadow -> Free
  insert_ $ RatingDiff gameReportTimestamp gameReportWinnerId rid gameReportSide winnerRatingBefore winnerRatingAfter
  insert_ $ RatingDiff gameReportTimestamp gameReportLoserId rid losingSide loserRatingBefore loserRatingAfter
