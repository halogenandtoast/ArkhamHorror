module Arkham.Campaigns.TheFeastOfHemlockVale.TokenHelpers where

import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (dayNumber, getCampaignDay)
import Arkham.ChaosToken (pattern NegativeModifier, pattern PositiveModifier)
import Arkham.ChaosToken.Types (
  ChaosToken,
  ChaosTokenFace (..),
  ChaosTokenModifier (..),
  ChaosTokenValue (..),
 )
import Arkham.Classes.HasChaosTokenValue
import Arkham.Classes.HasGame
import Arkham.Helpers.Scenario
import Arkham.Helpers.SkillTest (isParley, withSkillTest)
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Message.Lifted
import Arkham.Modifier
import Arkham.Prelude hiding (Day)
import Arkham.Projection
import Arkham.Scenario.Runner ()
import Arkham.Scenario.Types (ScenarioAttrs)
import Arkham.Tracing

hemlockPreludeChaosTokenValue
  :: (HasCallStack, HasGame m, Tracing m)
  => InvestigatorId -> ChaosTokenFace -> ScenarioAttrs -> m ChaosTokenValue
hemlockPreludeChaosTokenValue iid tokenFace attrs = case tokenFace of
  Skull -> do
    currentDay <- dayNumber <$> getCampaignDay
    pure $ toChaosTokenValue attrs Skull currentDay (currentDay + 1)
  Cultist -> do
    parley <- isParley
    pure
      $ ChaosTokenValue Cultist
      $ if isEasyStandard attrs || parley then PositiveModifier 1 else NegativeModifier 1
  Tablet -> do
    cardsInHand <- fieldMap InvestigatorHand length iid
    pure
      $ ChaosTokenValue Tablet
      $ if isEasyStandard attrs
        then if cardsInHand < 3 then NoModifier else NegativeModifier 2
        else if cardsInHand < 2 then NegativeModifier 2 else NegativeModifier 4
  ElderThing -> pure $ toChaosTokenValue attrs ElderThing 1 3
  otherFace -> getChaosTokenValue iid otherFace attrs

hemlockPreludeResolveChaosToken
  :: ReverseQueue m
  => ScenarioAttrs -> ChaosToken -> ChaosTokenFace -> InvestigatorId -> m ()
hemlockPreludeResolveChaosToken attrs token tokenFace iid = case tokenFace of
  Cultist | isEasyStandard attrs -> do
    parley <- isParley
    when parley $ withSkillTest \sid -> skillTestModifier sid Cultist token SkillTestAutomaticallySucceeds
  ElderThing -> do
    parley <- isParley
    when parley $ drawAnotherChaosToken iid
  _ -> pure ()
