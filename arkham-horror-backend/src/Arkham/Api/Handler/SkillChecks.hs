module Arkham.Api.Handler.SkillChecks
  ( postApiV1ArkhamGameSkillCheckR
  , postApiV1ArkhamGameSkillCheckApplyResultR
  , successfulInvestigation
  )
where

import Arkham.Internal.Types
import Arkham.Types
import Arkham.Types.Card.Internal
import qualified Data.HashMap.Strict as HashMap
import Import
import Lens.Micro
import Lens.Micro.Platform ()

postApiV1ArkhamGameSkillCheckR :: ArkhamGameId -> Handler ArkhamGameData
postApiV1ArkhamGameSkillCheckR gameId = do
  game <- runDB $ get404 gameId
  cards <- requireCheckJsonBody
  let ArkhamGameStateStepSkillCheckStep step = game ^. gameStateStep

  case ascsAction step of
    Just (InvestigateAction action) -> do
      let
        Just location =
          HashMap.lookup (aiaLocationId action) (game ^. locations)
      investigateAction (Entity gameId game) ArkhamSkillIntellect location cards
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
      let
        modifiedSkillValue = determineModifiedSkillValue
          artsType
          (game ^. player . investigator)
          artsCards
          n
      if modifiedSkillValue >= difficulty
        then runDB $ updateGame gameId $ successfulInvestigation
          game
          (location ^. locationId)
          1
        else runDB $ updateGame gameId $ failedInvestigation game location
    Failure -> runDB $ updateGame gameId $ failedInvestigation game location

shroudOf :: MonadIO m => ArkhamGame -> ArkhamLocation -> m Int
shroudOf _ (RevealedLocation location) = pure $ arlShroud location
shroudOf _ _ = throwString "Can not get shroud of unrevealed location"

determineModifiedSkillValue
  :: ArkhamSkillType -> ArkhamInvestigator -> [ArkhamCard] -> Int -> Int
determineModifiedSkillValue skillType investigator' commitedCards tokenModifier
  = skillValue investigator' skillType + cardContributions + tokenModifier
 where
  cardContributions = length $ filter (== skillType) $ concatMap
    (maybe [] aciTestIcons . toInternalCard)
    commitedCards

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

determineScenarioSpecificTokenModifier
  :: MonadIO m => ArkhamGame -> ArkhamChaosToken -> m Int
determineScenarioSpecificTokenModifier game token = do
  result <- determineScenarioSpecificTokenResult game token
  case result of
    Modifier n -> pure n
    Failure -> pure 0

updateGame
  :: (MonadIO m) => ArkhamGameId -> ArkhamGame -> SqlPersistT m ArkhamGameData
updateGame gameId game = replace gameId game $> arkhamGameCurrentData game

revealToken
  :: ArkhamChaosToken
  -> Int
  -> Int
  -> [ArkhamCard]
  -> ArkhamGameStateStep
  -> ArkhamGameStateStep
revealToken token difficulty modifiedSkillValue cards (ArkhamGameStateStepSkillCheckStep ArkhamSkillCheckStep {..})
  = ArkhamGameStateStepRevealTokenStep $ ArkhamRevealTokenStep
    ascsType
    ascsAction
    ascsTarget
    token
    difficulty
    modifiedSkillValue
    cards
revealToken _ _ _ _ s = s

investigateAction
  :: Entity ArkhamGame
  -> ArkhamSkillType
  -> ArkhamLocation
  -> [Int]
  -> Handler ArkhamGameData
investigateAction (Entity gameId game) skillType location cardIndexes = do
  token <- liftIO $ drawChaosToken game
  difficulty <- shroudOf game location
  tokenModifier <- liftIO $ determineScenarioSpecificTokenModifier game token

  let
    hand' = game ^. player . hand
    (commitedCards, remainingCards) =
      over both (map snd) $ partition (\(i, _) -> i `elem` cardIndexes) $ zip
        [0 ..]
        hand'
    modifiedSkillValue = determineModifiedSkillValue
      skillType
      (game ^. player . investigator)
      commitedCards
      tokenModifier
    newGame =
      game
        & gameStateStep
        %~ revealToken token difficulty modifiedSkillValue commitedCards
        & player
        . hand
        .~ remainingCards
        & player
        . discard
        %~ (commitedCards ++)
  runDB $ updateGame gameId newGame

failedInvestigation :: ArkhamGame -> ArkhamLocation -> ArkhamGame
failedInvestigation g _ = g

-- brittany-disable-next-binding
successfulInvestigation :: ArkhamGame -> LocationId -> Int -> ArkhamGame
successfulInvestigation g lId clueCount = g
    & locations . at lId . _Just . clues -~ clueCount
    & player . clues +~ clueCount
    & gameStateStep .~ investigatorStep

investigatorStep :: ArkhamGameStateStep
investigatorStep = ArkhamGameStateStepInvestigatorActionStep
