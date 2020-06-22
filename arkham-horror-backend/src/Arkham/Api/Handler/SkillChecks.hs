module Arkham.Api.Handler.SkillChecks where

import Arkham.Fixtures
import Arkham.Types
import Import
import Lens.Micro

applyTokenResult :: ArkhamGameData -> ArkhamChaosToken -> ArkhamGameData
applyTokenResult game _token = game

currentInvestigator :: Handler ArkhamInvestigator
currentInvestigator = pure rolandBanks

baseSkillValue :: ArkhamInvestigator -> ArkhamSkillType -> Int
baseSkillValue investigator = \case
  ArkhamSkillWillpower -> unArkhamSkill $ aiWillpower investigator
  ArkhamSkillIntellect -> unArkhamSkill $ aiIntellect investigator
  ArkhamSkillCombat -> unArkhamSkill $ aiCombat investigator
  ArkhamSkillAgility -> unArkhamSkill $ aiAgility investigator

tokenToValue :: ArkhamChaosToken -> ArkhamGameData -> Int
tokenToValue PlusOne _ = 1
tokenToValue Zero _ = 0
tokenToValue MinusOne _ = -1
tokenToValue MinusTwo _ = -2
tokenToValue MinusThree _ = -3
tokenToValue MinusFour _ = -4
tokenToValue MinusFive _ = -5
tokenToValue MinusSix _ = -6
tokenToValue MinusSeven _ = -7
tokenToValue MinusEight _ = -8
tokenToValue Skull _ = 0
tokenToValue Cultist _ = 0
tokenToValue Tablet _ = 0
tokenToValue ElderThing _ = 0
tokenToValue AutoFail _ = 0
tokenToValue ElderSign _ = 0

postApiV1ArkhamGameSkillCheckR :: Int -> Handler ArkhamGame
postApiV1ArkhamGameSkillCheckR _gameId = do
  game <- requireCheckJsonBody
  let ArkhamGameStateStepSkillCheckStep step = game ^. gameStateStep
  -- investigator <- currentInvestigator

  case ascsAction step of
    Just (InvestigateAction _) -> do
      token <- liftIO $ drawChaosToken game
      let newStep = ArkhamGameStateStepRevealTokenStep token
      pure $ game & gameStateStep .~ newStep
      -- case token of
      --   AutoFail -> pure game
      --   other -> do
      --     let
      --       Just (LocationTarget (RevealedLocation location)) = ascsTarget step
      --       difficulty = arlShroud location
      --       totalDifficulty = difficulty + tokenToValue other game
      --       skill = baseSkillValue investigator (ascsType step)
      --     if skill > totalDifficulty
      --       then pure game
      --       else error "failed skill check"

    _ -> error "fail"
