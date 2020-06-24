
module Arkham.Api.Handler.Actions where

import Arkham.Types
import Import
import Lens.Micro

applyAction :: ArkhamAction -> ArkhamGameData -> IO ArkhamGameData
applyAction action@(InvestigateAction investigation) g =
  pure $ g & gameStateStep .~ newGameStateStep
 where
  newGameStateStep = ArkhamGameStateStepSkillCheckStep $ ArkhamSkillCheckStep
    { ascsType = ArkhamSkillIntellect
    , ascsAction = Just action
    , ascsTarget = LocationTarget <$> mlocation
    }
  mlocation = lookup targetLocationId $ g ^. locations
  targetLocationId = aiaLocationId investigation
applyAction (TakeResourceAction _) g = pure $ g & player . resources +~ 1
applyAction (DrawCardAction _) g = pure $ g & player . resources +~ 1
applyAction _ g = pure g

postApiV1ArkhamGameActionR :: ArkhamGameId -> Handler ArkhamGameData
postApiV1ArkhamGameActionR gameId = do
  game <- runDB $ get404 gameId
  action <- requireCheckJsonBody
  newGame <- liftIO $ traverseOf currentData (applyAction action) game
  runDB $ replace gameId newGame
  pure $ arkhamGameCurrentData newGame
