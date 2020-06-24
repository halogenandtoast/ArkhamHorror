module Arkham.Api.Handler.SkillChecks
  ( postApiV1ArkhamGameSkillCheckR
  , postApiV1ArkhamGameSkillCheckApplyResultR
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
    Just (InvestigateAction _) -> investigateAction (Entity gameId game)
    _ -> error "fail"

postApiV1ArkhamGameSkillCheckApplyResultR
  :: ArkhamGameId -> Handler ArkhamGameData
postApiV1ArkhamGameSkillCheckApplyResultR gameId = do
  game <- runDB $ get404 gameId
  let
    ArkhamGameStateStepRevealTokenStep ArkhamRevealTokenStep {..} =
      game ^. gameStateStep
    Just (LocationTarget location) = artsTarget

  tokenResult <- liftIO $ determineScenarioSpecificTokenResult game artsToken
  case tokenResult of
    Modifier n -> do
      difficulty <- shroudOf game location
      if skillValue (game ^. player . investigator) artsType + n >= difficulty
        then runDB $ updateGame gameId $ successfulInvestigation game location
        else runDB $ updateGame gameId $ failedInvestigation game location
    Failure -> runDB $ updateGame gameId $ failedInvestigation game location

data ArkhamChaosTokenResult = Modifier Int | Failure

shroudOf :: MonadIO m => ArkhamGame -> ArkhamLocation -> m Int
shroudOf _ (RevealedLocation location) = pure $ arlShroud location
shroudOf _ _ = throwString "Can not get shroud of unrevealed location"

skillValue :: ArkhamInvestigator -> ArkhamSkillType -> Int
skillValue i skillType = case skillType of
  ArkhamSkillWillpower -> unArkhamSkill $ aiWillpower i
  ArkhamSkillIntellect -> unArkhamSkill $ aiIntellect i
  ArkhamSkillCombat -> unArkhamSkill $ aiCombat i
  ArkhamSkillAgility -> unArkhamSkill $ aiAgility i


determineScenarioSpecificTokenResult
  :: MonadIO m => ArkhamGame -> ArkhamChaosToken -> m ArkhamChaosTokenResult
determineScenarioSpecificTokenResult _ PlusOne = pure $ Modifier 1
determineScenarioSpecificTokenResult _ Zero = pure $ Modifier 0
determineScenarioSpecificTokenResult _ MinusOne = pure $ Modifier (-1)
determineScenarioSpecificTokenResult _ MinusTwo = pure $ Modifier (-2)
determineScenarioSpecificTokenResult _ MinusThree = pure $ Modifier (-3)
determineScenarioSpecificTokenResult _ MinusFour = pure $ Modifier (-4)
determineScenarioSpecificTokenResult _ MinusFive = pure $ Modifier (-5)
determineScenarioSpecificTokenResult _ MinusSix = pure $ Modifier (-6)
determineScenarioSpecificTokenResult _ MinusSeven = pure $ Modifier (-7)
determineScenarioSpecificTokenResult _ MinusEight = pure $ Modifier (-8)
determineScenarioSpecificTokenResult _ Skull = pure $ Modifier 0
determineScenarioSpecificTokenResult _ Cultist = pure $ Modifier 0
determineScenarioSpecificTokenResult _ Tablet = pure $ Modifier 0
determineScenarioSpecificTokenResult _ ElderThing = pure $ Modifier 0
determineScenarioSpecificTokenResult _ AutoFail = pure Failure
determineScenarioSpecificTokenResult _ ElderSign = pure $ Modifier 0

updateGame
  :: (MonadIO m) => ArkhamGameId -> ArkhamGame -> SqlPersistT m ArkhamGameData
updateGame gameId game = replace gameId game $> arkhamGameCurrentData game

revealToken :: ArkhamChaosToken -> ArkhamGameStateStep -> ArkhamGameStateStep
revealToken token (ArkhamGameStateStepSkillCheckStep ArkhamSkillCheckStep {..})
  = ArkhamGameStateStepRevealTokenStep
  $ ArkhamRevealTokenStep ascsType ascsAction ascsTarget token
revealToken _ s = s

investigateAction :: Entity ArkhamGame -> Handler ArkhamGameData
investigateAction (Entity gameId game) = do
  token <- liftIO $ drawChaosToken game
  let newGame = game & gameStateStep %~ revealToken token
  runDB $ updateGame gameId newGame

failedInvestigation :: ArkhamGame -> ArkhamLocation -> ArkhamGame
failedInvestigation g _ = g

successfulInvestigation :: ArkhamGame -> ArkhamLocation -> ArkhamGame
successfulInvestigation g location = g
