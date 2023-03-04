{-# LANGUAGE OverloadedRecordDot #-}
module Api.Handler.Arkham.Undo
  ( putApiV1ArkhamGameUndoR
  ) where

import Import hiding ( delete, on, (==.), (>=.), (<.) )

import Api.Arkham.Helpers
import Api.Arkham.Types.MultiplayerVariant
import Arkham.Card.CardCode
import Arkham.Game
import Arkham.Id
import Control.Lens ( view )
import Data.Aeson.Patch
import Data.Text qualified as T
import Data.Time.Clock
import Database.Esqueleto.Experimental
import Entity.Arkham.Step
import Entity.Arkham.LogEntry
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

            gameLog <- fmap (fmap unValue) . runDB $ select $ do
              entries <- from $ table @ArkhamLogEntry
              where_ $ entries.arkhamGameId ==. val gameId
              where_ $ entries.step <. val (arkhamGameStep - 1)
              orderBy [desc entries.createdAt]
              pure $ entries.body
            atomically
              $ writeTChan writeChannel
              $ encode
              $ GameUpdate
              $ PublicGame gameId arkhamGameName gameLog ge
            runDB $ do
              replace gameId $ ArkhamGame
                arkhamGameName
                ge
                (arkhamGameStep - 1)
                arkhamGameMultiplayerVariant
                arkhamGameCreatedAt
                now
              delete $ do
                entries <- from $ table @ArkhamLogEntry
                where_ $ entries.arkhamGameId ==. val gameId
                where_ $ entries.step >=. val (arkhamGameStep - 1)
              deleteKey stepId

              case arkhamGameMultiplayerVariant of
                Solo -> replace pid $ arkhamPlayer
                  { arkhamPlayerInvestigatorId = coerce
                    (view activeInvestigatorIdL ge)
                  }
                WithFriends -> pure ()
