module Api.Handler.Arkham.Undo
  ( putApiV1ArkhamGameUndoR
  ) where

import Import hiding (delete, on, (==.))

import Api.Arkham.Helpers
import Arkham.Game
import Arkham.Types.Game
import Control.Lens ((&), (.~))
import Control.Monad.Random (mkStdGen)
import Json
import Safe (fromJustNote)

putApiV1ArkhamGameUndoR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameUndoR gameId = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  ArkhamGame {..} <- runDB $ get404 gameId
  let gameJson@Game {..} = arkhamGameCurrentData

  _ <- runDB $ entityVal <$> getBy404 (UniquePlayer userId gameId)

  let undidChoices = drop 1 gameChoices

  gameRef <- newIORef (gameJson & choicesL .~ undidChoices)
  queueRef <- newIORef []
  genRef <- newIORef (mkStdGen gameSeed)
  writeChannel <- getChannel gameId

  runGameApp (GameApp gameRef queueRef genRef $ \_ -> pure ()) replayChoices

  ge <- readIORef gameRef
  updatedQueue <- readIORef queueRef
  liftIO $ atomically $ writeTChan
    writeChannel
    (encode $ GameUpdate $ PublicGame gameId arkhamGameName arkhamGameLog ge)
  runDB
    (replace
      gameId
      (ArkhamGame
        arkhamGameName
        ge
        updatedQueue
        arkhamGameLog
        arkhamGameMultiplayerVariant
      )
    )
