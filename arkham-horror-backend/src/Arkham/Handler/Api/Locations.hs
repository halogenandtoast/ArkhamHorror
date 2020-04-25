module Arkham.Handler.Api.Locations where

import Arkham.Fixtures
import Arkham.Types
import qualified Data.List.NonEmpty as NE
import Import
import Prelude ((!!))
import System.Random

sample :: NE.NonEmpty a -> IO a
sample l = (toList l !!) <$> randomRIO (0, length l - 1)

getRandomToken :: NE.NonEmpty ArkhamChaosToken -> IO ArkhamChaosToken
getRandomToken = sample

tokenPool :: NE.NonEmpty ArkhamChaosToken
tokenPool = NE.fromList [ArkhamChaosTokenNumber (-1)]

tokenToModifier :: ArkhamScenario -> ArkhamChaosToken -> Int
tokenToModifier _ _ = -1

investigatorSkillValue :: ArkhamSkill -> ArkhamInvestigator -> Int
investigatorSkillValue ArkhamSkillWillpower = arkhamInvestigatorWillpower
investigatorSkillValue ArkhamSkillIntellect = arkhamInvestigatorIntellect
investigatorSkillValue ArkhamSkillCombat = arkhamInvestigatorCombat
investigatorSkillValue ArkhamSkillAgility = arkhamInvestigatorAgility

shroudValue :: ArkhamLocation -> Maybe Int
shroudValue (ArkhamLocationUnrevealed _) = Nothing
shroudValue (ArkhamLocationRevealed l) =
  pure $ arkhamLocationRevealedDataShroud l

postApiV1ArkhamGameLocationsRevealR
  :: ArkhamHorrorGameId -> Handler ArkhamLocation
postApiV1ArkhamGameLocationsRevealR _ = pure study

postApiV1ArkhamGameLocationsInvestigateR
  :: ArkhamHorrorGameId -> Int -> Handler ArkhamSkillTestResult
postApiV1ArkhamGameLocationsInvestigateR _ _ = case shroudValue study of
  Nothing -> error "Could not investgate"
  Just difficulty -> do
    token <- liftIO $ getRandomToken tokenPool
    let skillValue = investigatorSkillValue ArkhamSkillIntellect rolandBanks
    -- TODO: commit skill cards
    let finalValue = skillValue - difficulty - tokenToModifier (error "unused") token
    let
      result = if finalValue >= 0
        then ArkhamSkillTestResultTypeSuccess
        else ArkhamSkillTestResultTypeFailure
    pure ArkhamSkillTestResult
      { arkhamSkillTestResultToken = token
      , arkhamSkillTestResultBase = skillValue
      , arkhamSkillTestResultSkill = ArkhamSkillIntellect
      , arkhamSkillTestResultAction = ArkhamActionInvestigate 0
      , arkhamSkillTestResultType = result
      }
