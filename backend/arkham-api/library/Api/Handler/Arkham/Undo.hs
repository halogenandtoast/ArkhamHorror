module Api.Handler.Arkham.Undo
  ( putApiV1ArkhamGameUndoR
  ) where

import Import hiding ( delete, on, (==.) )

import Data.Aeson.Patch
import Api.Arkham.Helpers
import Arkham.Card.CardCode
import Arkham.Game
import Arkham.Id
import Control.Lens ( view )
import Data.Text qualified as T
import Data.Time.Clock
import Database.Esqueleto.Experimental ( deleteKey )
import Entity.Arkham.Step
import Json
import Safe ( fromJustNote )

putApiV1ArkhamGameUndoR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameUndoR gameId = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  ArkhamGame {..} <- runDB $ get404 gameId
  Entity pid arkhamPlayer <- runDB $ getBy404 (UniquePlayer userId gameId)
  now <- liftIO getCurrentTime

  mstep <- runDB $ getBy (UniqueStep gameId arkhamGameStep)

  case mstep of
    Nothing -> pure ()
    Just (Entity stepId step) -> do
      -- never delete the initial step as it can not be redone
      -- NOTE: actually we never want to step back if the patchOperations are empty, the first condition is therefor redundant
      when (arkhamStepStep step > 0 && not (null $ patchOperations $ choicePatchDown $ arkhamStepChoice step)) $ do
        writeChannel <- getChannel gameId

        case patch arkhamGameCurrentData (choicePatchDown $ arkhamStepChoice step) of
          Error e -> error $ T.pack e
          Success ge -> do
            atomically
              $ writeTChan writeChannel
              $ encode
              $ GameUpdate
              $ PublicGame gameId arkhamGameName arkhamGameLog ge
            runDB $ do
              replace gameId $ ArkhamGame
                arkhamGameName
                ge
                (arkhamGameStep - 1)
                arkhamGameLog
                arkhamGameMultiplayerVariant
                arkhamGameCreatedAt
                now
              deleteKey stepId

              replace pid $ arkhamPlayer
                { arkhamPlayerInvestigatorId = coerce
                  (view activeInvestigatorIdL ge)
                }
