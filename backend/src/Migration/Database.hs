module Migration.Database where

import AppConfig (AppM)
import AppServer (normalizeName)
import Database (insertPlayerIfNotExists, repsertPlayerStats, runDb)
import Migration.Types (ParsedLegacyLadderEntry (..))
import Types.Database
  ( PlayerStatsInitial (..),
    PlayerStatsTotal (..),
    defaultPlayerStatsYear,
  )

insertLegacyEntry :: ParsedLegacyLadderEntry -> AppM ()
insertLegacyEntry entry = runDb $ do
  playerId <- insertPlayerIfNotExists (normalizeName entry.player) entry.player
  let initialStats = PlayerStatsInitial playerId entry.freeRating entry.shadowRating entry.gamesPlayedTotal
  let totalStats = PlayerStatsTotal playerId entry.freeRating entry.shadowRating entry.gamesPlayedTotal
  let yearStats = defaultPlayerStatsYear playerId 2022
  _ <- repsertPlayerStats (totalStats, yearStats)
  -- TODO Insert initial stats
  pass
