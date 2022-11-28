module Api.Handler.Arkham.Replay
  ( getApiV1ArkhamGameReplayR
  ) where

import Api.Arkham.Helpers
import Arkham.Game
import Database.Esqueleto.Experimental
import Entity.Arkham.Step
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
  allChoices <- runDB $ select $ do
    steps <- from $ table @ArkhamStep
    where_ $ steps ^. ArkhamStepArkhamGameId ==. val gameId
    orderBy [asc $ steps ^. ArkhamStepStep]
    pure steps
  let gameJson = arkhamGameCurrentData ge
  let choices = map (arkhamStepChoice . entityVal) $ reverse $ drop step allChoices
  let gameJson' = replayChoices gameJson $ map choicePatchDown choices

  pure $ GetReplayJson
    (length choices)
    (toPublicGame $ Entity
      gameId
      (ArkhamGame
        (arkhamGameName ge)
        gameJson'
        (arkhamGameStep ge)
        []
        (arkhamGameMultiplayerVariant ge)
        (arkhamGameCreatedAt ge)
        (arkhamGameUpdatedAt ge)
      )
    )
