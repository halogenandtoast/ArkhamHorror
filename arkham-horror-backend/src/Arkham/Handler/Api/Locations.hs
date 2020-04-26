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
tokenPool = NE.fromList [ArkhamChaosTokenMinusOne]

tokenToModifier :: ArkhamGame -> ArkhamChaosToken -> Int
tokenToModifier _ ArkhamChaosTokenPlusOne = 1
tokenToModifier _ ArkhamChaosTokenZero = 0
tokenToModifier _ ArkhamChaosTokenMinusOne = -1
tokenToModifier _ ArkhamChaosTokenMinusTwo = -2
tokenToModifier _ ArkhamChaosTokenMinusThree = -3
tokenToModifier _ ArkhamChaosTokenMinusOne = -4
tokenToModifier _ ArkhamChaosTokenMinusOne = -5
tokenToModifier _ ArkhamChaosTokenMinusOne = -6
tokenToModifier _ ArkhamChaosTokenMinusOne = -7
tokenToModifier _ ArkhamChaosTokenMinusOne = -8
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

postApiV1ArkhamGameLocationsRevealR :: ArkhamGameId -> Handler ArkhamLocation
postApiV1ArkhamGameLocationsRevealR _ = pure study

postApiV1ArkhamGameLocationsInvestigateR
  :: ArkhamGameId -> Int -> Handler ArkhamSkillTestResult
postApiV1ArkhamGameLocationsInvestigateR gameId _ = do
  game <- runDB $ get404 gameId
  case shroudValue study of
    Nothing -> error "Could not investgate"
    Just difficulty -> do
      token <- liftIO $ getRandomToken tokenPool
      let skillValue = investigatorSkillValue ArkhamSkillIntellect rolandBanks
      -- TODO: commit skill cards
      let finalValue = skillValue - difficulty - tokenToModifier game token
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
