module Api.Handler.Arkham.Replay
  ( getApiV1ArkhamGameReplayR
  ) where

import Api.Arkham.Helpers
import Arkham.Game
import Import hiding ( delete, on, (==.) )
import Safe ( fromJustNote )

data GetReplayJson = GetReplayJson
  { totalSteps :: Int
  , game :: PublicGame ArkhamGameId
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

newtype ReplayId = ReplayId {id :: ArkhamGameId}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

getApiV1ArkhamGameReplayR :: ArkhamGameId -> Int -> Handler GetReplayJson
getApiV1ArkhamGameReplayR gameId step = do
  _ <- fromJustNote "Not authenticated" <$> getRequestUserId
  ge <- runDB $ get404 gameId
  let gameJson = arkhamGameCurrentData ge
  let choices = reverse $ drop step $ reverse $ arkhamGameChoices ge
  let gameJson' = replayChoices gameJson $ map choicePatchDown choices

  pure $ GetReplayJson
    (length choices)
    (toPublicGame $ Entity
      gameId
      (ArkhamGame
        (arkhamGameName ge)
        gameJson'
        (arkhamGameChoices ge)
        []
        (arkhamGameMultiplayerVariant ge)
        (arkhamGameCreatedAt ge)
        (arkhamGameUpdatedAt ge)
      )
    )
