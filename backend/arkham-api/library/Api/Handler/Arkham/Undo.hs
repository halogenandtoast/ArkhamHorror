module Api.Handler.Arkham.Undo
  ( putApiV1ArkhamGameUndoR
  ) where

import Import hiding (delete, on, (==.))

import Api.Arkham.Helpers
import Arkham.Types.Card.CardCode
import Arkham.Types.Game
import Arkham.Types.Id
import Control.Lens (view)
import Data.Text qualified as T
import Json
import Safe (fromJustNote)

putApiV1ArkhamGameUndoR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameUndoR gameId = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  ArkhamGame {..} <- runDB $ get404 gameId
  Entity pid arkhamPlayer <- runDB $ getBy404 (UniquePlayer userId gameId)

  case arkhamGameChoices of
    [] -> pure ()
    [_] -> pure () -- can't undo the initial change
    choice : remaining -> do
      writeChannel <- getChannel gameId

      case patch arkhamGameCurrentData (choicePatchDown choice) of
        Error e -> error $ T.pack e
        Success ge -> do
          liftIO $ atomically $ writeTChan
            writeChannel
            (encode $ GameUpdate $ PublicGame
              gameId
              arkhamGameName
              arkhamGameLog
              ge
            )
          runDB $ do
            replace
              gameId
              (ArkhamGame
                arkhamGameName
                ge
                remaining
                arkhamGameLog
                arkhamGameMultiplayerVariant
              )

            replace pid $ arkhamPlayer
              { arkhamPlayerInvestigatorId = coerce
                (view activeInvestigatorIdL ge)
              }
