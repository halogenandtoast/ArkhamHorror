module Api.Handler.Arkham.Undo
  ( putApiV1ArkhamGameUndoR
  ) where

import Import hiding (delete, on, (==.))

import Api.Arkham.Helpers
import Arkham.Game
import Arkham.Types.Card.CardCode
import Arkham.Types.Game
import Arkham.Types.Id
import Control.Lens (view)
import Control.Monad.Random (mkStdGen)
import Data.Coerce
import Json
import Safe (fromJustNote)

putApiV1ArkhamGameUndoR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameUndoR gameId = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  ArkhamGame {..} <- runDB $ get404 gameId
  let gameJson@Game {..} = arkhamGameCurrentData

  Entity pid arkhamPlayer <- runDB $ getBy404 (UniquePlayer userId gameId)

  let undidChoices = drop 1 arkhamGameChoices

  gameRef <- newIORef gameJson
  queueRef <- newIORef []
  genRef <- newIORef (mkStdGen gameSeed)
  writeChannel <- getChannel gameId

  runGameApp
    (GameApp gameRef queueRef genRef $ \_ -> pure ())
    (replayChoices $ map choicePatch undidChoices)

  ge <- readIORef gameRef
  liftIO $ atomically $ writeTChan
    writeChannel
    (encode $ GameUpdate $ PublicGame gameId arkhamGameName arkhamGameLog ge)
  runDB $ do
    replace
      gameId
      (ArkhamGame
        arkhamGameName
        ge
        undidChoices
        arkhamGameLog
        arkhamGameMultiplayerVariant
      )

    replace pid $ arkhamPlayer
      { arkhamPlayerInvestigatorId = coerce (view activeInvestigatorIdL ge)
      }
