module Api.Handler.Arkham.Replay
  ( getApiV1ArkhamGameReplayR
  ) where

import Api.Arkham.Helpers
import Arkham.Game
import Arkham.Types.Game
import Control.Monad.Random (mkStdGen)
import Import hiding (delete, on, (==.))
import Safe (fromJustNote)

data GetReplayJson = GetReplayJson
  { totalSteps :: Int
  , game :: PublicGame ArkhamGameId
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

newtype ReplayId = ReplayId { id :: ArkhamGameId }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

getApiV1ArkhamGameReplayR :: ArkhamGameId -> Int -> Handler GetReplayJson
getApiV1ArkhamGameReplayR gameId step = do
  _ <- fromJustNote "Not authenticated" <$> getRequestUserId
  ge <- runDB $ get404 gameId
  let gameJson@Game {..} = arkhamGameCurrentData ge
  let choices = reverse (take step (reverse $ arkhamGameChoices ge))

  gameRef <- newIORef gameJson
  queueRef <- newIORef []
  genRef <- newIORef (mkStdGen gameSeed)

  runGameApp
    (GameApp gameRef queueRef genRef $ \_ -> pure ())
    (replayChoices $ map choicePatch choices)

  ge' <- readIORef gameRef
  let diffedGame = diff gameJson ge'
  updatedQueue <- readIORef queueRef
  pure $ GetReplayJson
    (length choices)
    (toPublicGame $ Entity
      gameId
      (ArkhamGame
        (arkhamGameName ge)
        ge'
        (Choice diffedGame updatedQueue : arkhamGameChoices ge)
        []
        (arkhamGameMultiplayerVariant ge)
      )
    )
