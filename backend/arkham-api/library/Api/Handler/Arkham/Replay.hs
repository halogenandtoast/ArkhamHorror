module Api.Handler.Arkham.Replay (getApiV1ArkhamGameReplayR) where

import Api.Arkham.Helpers
import Arkham.Game
import Database.Esqueleto.Experimental
import Entity.Arkham.LogEntry
import Entity.Arkham.Step
import Import hiding (delete, on, (<.), (==.))

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

  -- Logs are stamped with the pre-action step. Only show actions already
  -- included in this replay snapshot, using stored step IDs rather than indices.
  let replayStep = maybe 0 (arkhamStepStep . entityVal) $ headMay $ reverse $ take step allChoices
  gameLog <-
    GameLog . map (toLogRow . entityVal) <$> select do
      entries <- from $ table @ArkhamLogEntry
      where_ $ entries.arkhamGameId ==. val gameId
      where_ $ entries.step <. val replayStep
      orderBy [asc entries.step, asc entries.id]
      pure entries

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
            (arkhamGameAllowSpectators ge)
      )
      gameLog
