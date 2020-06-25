module Arkham.Api.Handler.SkillChecks
  ( postApiV1ArkhamGameSkillCheckR
  , postApiV1ArkhamGameSkillCheckApplyResultR
  , successfulInvestigation
  )
where

import Arkham.Types
import Arkham.Types.Card.Internal
import Import
import Lens.Micro
import Lens.Micro.Platform ()

postApiV1ArkhamGameSkillCheckR :: ArkhamGameId -> Handler ArkhamGameData
postApiV1ArkhamGameSkillCheckR gameId = do
  game <- runDB $ get404 gameId
  cards <- requireCheckJsonBody
  let ArkhamGameStateStepSkillCheckStep step = game ^. gameStateStep

  case ascsAction step of
    Just (InvestigateAction _) -> investigateAction (Entity gameId game) cards
    _ -> error "fail"

postApiV1ArkhamGameSkillCheckApplyResultR
  :: ArkhamGameId -> Handler ArkhamGameData
postApiV1ArkhamGameSkillCheckApplyResultR gameId = do
  game <- runDB $ get404 gameId
  let
    ArkhamGameStateStepRevealTokenStep ArkhamRevealTokenStep {..} =
      game ^. gameStateStep
    Just (LocationTarget location) = artsTarget
    cardContributions = length $ filter (== artsType) $ concatMap
      (maybe [] aciTestIcons . toInternalCard)
      artsCards

  tokenResult <- liftIO $ determineScenarioSpecificTokenResult game artsToken
  case tokenResult of
    Modifier n -> do
      difficulty <- shroudOf game location
      if skillValue (game ^. player . investigator) artsType
          + cardContributions
          + n
          >= difficulty
        then runDB $ updateGame gameId $ successfulInvestigation
          game
          (location ^. locationId)
          1
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
  ArkhamSkillWild -> error "Not a possible skill"

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

revealToken
  :: ArkhamChaosToken
  -> [ArkhamCard]
  -> ArkhamGameStateStep
  -> ArkhamGameStateStep
revealToken token cards (ArkhamGameStateStepSkillCheckStep ArkhamSkillCheckStep {..})
  = ArkhamGameStateStepRevealTokenStep
    $ ArkhamRevealTokenStep ascsType ascsAction ascsTarget token cards
revealToken _ _ s = s

investigateAction :: Entity ArkhamGame -> [Int] -> Handler ArkhamGameData
investigateAction (Entity gameId game) cardIndexes = do
  token <- liftIO $ drawChaosToken game
  let
    hand' = game ^. player . hand
    (spentCards, remainingCards) =
      over both (map snd) $ partition (\(i, _) -> i `elem` cardIndexes) $ zip
        [0 ..]
        hand'
  let
    newGame =
      game
        & gameStateStep
        %~ revealToken token spentCards
        & player
        . hand
        .~ remainingCards
  runDB $ updateGame gameId newGame

failedInvestigation :: ArkhamGame -> ArkhamLocation -> ArkhamGame
failedInvestigation g _ = g

-- brittany-disable-next-binding
successfulInvestigation :: ArkhamGame -> LocationId -> Int -> ArkhamGame
successfulInvestigation g !lId clueCount = g
    & locations . at lId . _Just . clues -~ clueCount
    & player . clues +~ clueCount
    & gameStateStep .~ investigatorStep

investigatorStep :: ArkhamGameStateStep
investigatorStep = ArkhamGameStateStepInvestigatorActionStep
