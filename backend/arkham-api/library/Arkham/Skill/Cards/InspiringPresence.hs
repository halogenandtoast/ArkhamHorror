module Arkham.Skill.Cards.InspiringPresence (inspiringPresence) where

import Arkham.Helpers.Healing
import Arkham.Matcher hiding (AssetExhausted)
import Arkham.Message.Lifted.Choose
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype InspiringPresence = InspiringPresence SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inspiringPresence :: SkillCard InspiringPresence
inspiringPresence = skill InspiringPresence Cards.inspiringPresence

instance RunMessage InspiringPresence where
  runMessage msg s@(InspiringPresence attrs) = runQueueT $ case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      assets <-
        select
          $ at_ attrs.owner.location
          <> #ally
          <> oneOf [AssetCanReady, healableAsset attrs AnyAsset]
      skillTestResultOption "Inspiring Presence" do
        chooseTargetM attrs.owner assets \a -> do
          whenMatch a AssetCanReady $ readyThis a
          assetChooseHealDamageOrHorror attrs attrs.owner a
      pure s
    _ -> InspiringPresence <$> liftRunMessage msg attrs
