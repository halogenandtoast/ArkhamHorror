module Arkham.Api.Handler.Choices
  ( postApiV1ArkhamGameMakeChoiceR
  )
where

import Arkham.Internal.Agenda
import Arkham.Types
import Arkham.Types.Game
import Arkham.Types.GameState
import Arkham.Util
import Base.Lock
import qualified Data.List.NonEmpty as NE
import Import
import Json
import Lens.Micro

newtype MakeChoicePost = MakeChoicePost { mcpIndex :: Int }
  deriving stock (Generic, Show)

instance FromJSON MakeChoicePost where
  parseJSON = genericParseJSON $ aesonOptions $ Just "mcp"

postApiV1ArkhamGameMakeChoiceR :: ArkhamGameId -> Handler ArkhamGameData
postApiV1ArkhamGameMakeChoiceR gameId = do
  g <- runDB $ get404 gameId
  choice <- mcpIndex <$> requireCheckJsonBody
  let
    ArkhamGameStateStepChooseOneStep ArkhamChooseOneStep {..} =
      g ^. gameStateStep
  case acosChoiceTarget of
    AgendaTarget agenda' -> do
      let a = toInternalAgenda agenda'
      (g & lock %~ (>>= NE.nonEmpty . NE.tail) & traverseOf
          (currentData . gameState)
          (agendaOnChoice a choice)
        )
        >>= runDB
        . updateGame gameId
    _ -> error "Other targets currently not implemented"
