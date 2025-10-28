module Arkham.Skill.Cards.InspiringPresence2 (inspiringPresence2) where

import Arkham.Helpers.Asset
import Arkham.Helpers.Healing
import Arkham.Matcher hiding (AssetExhausted)
import Arkham.Message.Lifted.Choose
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype InspiringPresence2 = InspiringPresence2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inspiringPresence2 :: SkillCard InspiringPresence2
inspiringPresence2 = skill InspiringPresence2 Cards.inspiringPresence2

instance RunMessage InspiringPresence2 where
  runMessage msg s@(InspiringPresence2 attrs) = runQueueT $ case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      assets <-
        select
          $ at_ attrs.owner.location
          <> #ally
          <> oneOf [AssetCanReady, healableAsset attrs AnyAsset]
      skillTestResultOption "Inspiring Presence (2)" do
        chooseTargetM attrs.owner assets \a -> do
          whenMatch a AssetCanReady $ readyThis a
          whenM (assetCanHaveDamageHealed attrs a) $ healDamage a attrs 1
          whenM (assetCanHaveHorrorHealed attrs a) $ healHorror a attrs 1
      pure s
    _ -> InspiringPresence2 <$> liftRunMessage msg attrs
