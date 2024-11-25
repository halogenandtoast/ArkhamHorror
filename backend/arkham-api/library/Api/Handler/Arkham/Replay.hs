module Api.Handler.Arkham.Replay (getApiV1ArkhamGameReplayR) where

import Api.Arkham.Helpers
import Arkham.Game
import Database.Esqueleto.Experimental
import Entity.Arkham.Step
import Import hiding (delete, on, (==.))

data GetReplayJson = GetReplayJson
  { totalSteps :: Int
  , game :: PublicGame ArkhamGameId
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

newtype ReplayId = ReplayId {id :: ArkhamGameId}
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

getApiV1ArkhamGameReplayR :: ArkhamGameId -> Int -> Handler GetReplayJson
getApiV1ArkhamGameReplayR gameId step = runDB do
  ge <- get404 gameId
  allChoices <- select do
    steps <- from $ table @ArkhamStep
    where_ $ steps.arkhamGameId ==. val gameId
    orderBy [asc steps.step]
    pure steps
  let gameJson = arkhamGameCurrentData ge
  let choices = map (arkhamStepChoice . entityVal) $ reverse $ drop step allChoices
  let gameJson' = replayChoices gameJson [mconcat $ map choicePatchDown choices]

  pure
    $ GetReplayJson (length allChoices)
    $ toPublicGame
      ( Entity gameId
          $ ArkhamGame
            (arkhamGameName ge)
            gameJson'
            (arkhamGameStep ge)
            (arkhamGameMultiplayerVariant ge)
            (arkhamGameCreatedAt ge)
            (arkhamGameUpdatedAt ge)
      )
      mempty
