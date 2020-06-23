module Arkham.Api.Handler.SkillChecks
  ( postApiV1ArkhamGameSkillCheckR
  )
where

import Arkham.Types
import Import
import Lens.Micro

postApiV1ArkhamGameSkillCheckR :: ArkhamGameId -> Handler ArkhamGameData
postApiV1ArkhamGameSkillCheckR gameId = do
  game <- runDB $ get404 gameId
  let ArkhamGameStateStepSkillCheckStep step = game ^. gameStateStep

  case ascsAction step of
    Just (InvestigateAction _) -> do
      token <- liftIO $ drawChaosToken game
      let
        newStep = ArkhamGameStateStepRevealTokenStep token
        newGame = game & gameStateStep .~ newStep
      runDB $ replace gameId newGame
      pure $ arkhamGameCurrentData newGame
    _ -> error "fail"
