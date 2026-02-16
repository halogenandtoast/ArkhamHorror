module Arkham.Skill.Cards.InspiringPresence (inspiringPresence) where

import {-# SOURCE #-} Arkham.GameEnv (getSkillTest)
import Arkham.Helpers.Healing
import Arkham.Matcher hiding (AssetExhausted)
import Arkham.Message.Lifted.Choose
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillTestResult

newtype InspiringPresence = InspiringPresence SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inspiringPresence :: SkillCard InspiringPresence
inspiringPresence = skill InspiringPresence Cards.inspiringPresence

instance RunMessage InspiringPresence where
  runMessage msg s@(InspiringPresence attrs) = runQueueT $ case msg of
    CheckSkillTestResultOptions skillTestId exclusions -> do
      mst <- getSkillTest
      for_ mst \st -> do
        when (st.id == skillTestId && isTarget attrs st.target) do
          case st.result of
            SucceededBy {} -> do
              assets <-
                select
                  $ at_ attrs.owner.location
                  <> #ally
                  <> oneOf [AssetCanReady, healableAsset attrs AnyAsset]
              provideSkillTestResultOption attrs exclusions "Inspiring Presence" do
                chooseTargetM attrs.owner assets \a -> do
                  whenMatch a AssetCanReady $ readyThis a
                  assetChooseHealDamageOrHorror attrs attrs.owner a
            _ -> pure ()
      pure s
    _ -> InspiringPresence <$> liftRunMessage msg attrs
