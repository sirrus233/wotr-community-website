module AppServer where

import Api (Api)
import Data.Pool (withResource)
import Data.Time (UTCTime)
import Data.Time.Clock (getCurrentTime)
import Data.Validation (Validation (..))
import Database (insertGameReport, insertPlayerIfNotExists)
import Database.SQLite.Simple (withTransaction)
import Servant (ServerError (errBody), ServerT, throwError)
import Servant.Server (err422)
import Types.Api (GameReport (..))
import Types.App (AppM, Env (..))
import Types.DataField (PlayerId)
import Types.Database (ReadProcessedGameReport (..), WriteProcessedGameReport (..))
import Validation (validateReport)

processReport :: UTCTime -> PlayerId -> PlayerId -> GameReport -> WriteProcessedGameReport
processReport timestamp winner loser report =
  WriteProcessedGameReport
    { timestamp,
      winner,
      loser,
      side = report.side,
      victory = report.victory,
      match = report.match,
      competition = report.competition,
      league = report.league,
      expansions = report.expansions,
      treebeard = report.treebeard,
      actionTokens = report.actionTokens,
      dwarvenRings = report.dwarvenRings,
      turns = report.turns,
      corruption = report.corruption,
      mordor = report.mordor,
      initialEyes = report.initialEyes,
      aragornTurn = report.aragornTurn,
      strongholds = report.strongholds,
      interestRating = report.interestRating,
      comments = report.comments,
      winnerRatingAfter = 0, -- TODO
      loserRatingAfter = 0 -- TODO
    }

submitReportHandler :: GameReport -> AppM ReadProcessedGameReport
submitReportHandler r = case validateReport r of
  Failure errors -> throwError $ err422 {errBody = show errors}
  Success report -> do
    env <- ask
    liftIO . withResource env.dbPool $ \conn -> withTransaction conn $ do
      winnerId <- insertPlayerIfNotExists conn report.winner
      loserId <- insertPlayerIfNotExists conn report.loser
      now <- liftIO getCurrentTime
      insertGameReport conn $ processReport now winnerId loserId report

server :: ServerT Api AppM
server = submitReportHandler