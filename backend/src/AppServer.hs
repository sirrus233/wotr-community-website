module AppServer where

import Api (Api)
import Servant (ServerError (errBody), ServerT, throwError)
import Servant.Server (err422)
import Types.Api (GameReport)
import Types.App (AppM)
import Types.Database (ReadProcessedGameReport)

submitReportHandler :: GameReport -> AppM ReadProcessedGameReport
submitReportHandler report = throwError $ err422 {errBody = "I understood your request, but can't process it."}

server :: ServerT Api AppM
server = submitReportHandler