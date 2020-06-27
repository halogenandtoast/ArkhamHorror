module Arkham.Api.Handler.SkillChecks
  ( postApiV1ArkhamGameSkillCheckR
  , postApiV1ArkhamGameSkillCheckApplyResultR
  , successfulInvestigation
  )
where

import Arkham.Conversion
import Arkham.Internal.Card
import Arkham.Internal.Types
import Arkham.Types
import Arkham.Util
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
    tokenInternal = toInternalToken game artsToken

  case
      tokenToResult
        tokenInternal
        (game ^. currentData . gameState)
        (game ^. player . investigator)
    of
      Modifier n -> do
        checkDifficulty <- shroudOf game location
        let
          modifiedSkillValue = determineModifiedSkillValue
            artsType
            (game ^. player . investigator)
            artsCards
            n
        if modifiedSkillValue >= checkDifficulty
          then runDB $ updateGame gameId $ successfulInvestigation
            game
            location
            1
          else runDB $ updateGame gameId $ failedInvestigation game location
      Failure -> runDB $ updateGame gameId $ failedInvestigation game location

shroudOf :: MonadIO m => ArkhamGame -> ArkhamLocation -> m Int
shroudOf _ location = pure $ alShroud location

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

revealToken
  :: ArkhamChaosToken
  -> Int
  -> Int
  -> [ArkhamCard]
  -> ArkhamGameStateStep
  -> ArkhamGameStateStep
revealToken token' checkDifficulty modifiedSkillValue cards (ArkhamGameStateStepSkillCheckStep ArkhamSkillCheckStep {..})
  = ArkhamGameStateStepRevealTokenStep $ ArkhamRevealTokenStep
    ascsType
    ascsAction
    ascsTarget
    token'
    checkDifficulty
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
  token' <- liftIO $ drawChaosToken game
  checkDifficulty <- shroudOf game location
  let
    tokenModifier = tokenToModifier game (game ^. player . investigator) token'

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
        %~ revealToken token' checkDifficulty modifiedSkillValue commitedCards
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
successfulInvestigation :: ArkhamGame -> ArkhamLocation -> Int -> ArkhamGame
successfulInvestigation g l clueCount = g
    & locations . at (alCardCode l) . _Just . clues -~ clueCount
    & player . clues +~ clueCount
    & gameStateStep .~ investigatorStep

investigatorStep :: ArkhamGameStateStep
investigatorStep = ArkhamGameStateStepInvestigatorActionStep
