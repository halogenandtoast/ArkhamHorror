module Arkham.Skill.Cards.Leadership2 (leadership2, Leadership2 (..)) where

import Arkham.Capability
import Arkham.Game.Helpers
import Arkham.Helpers.SkillTest
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Leadership2 = Leadership2 SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leadership2 :: SkillCard Leadership2
leadership2 = skill Leadership2 Cards.leadership2

instance HasModifiersFor Leadership2 where
  getModifiersFor (Leadership2 attrs) = do
    maybeModified_ attrs attrs.cardId do
      iid <- MaybeT getSkillTestInvestigator
      guard $ attrs.owner /= iid
      pure [AddSkillIcons [#willpower, #wild]]

instance RunMessage Leadership2 where
  runMessage msg s@(Leadership2 attrs) = runQueueT $ case msg of
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ _ -> do
      whenM (can.gain.resources attrs.owner) do
        gainResourcesIfCan (skillOwner attrs) attrs 2

      otherCanGainResources <- can.gain.resources iid
      when (otherCanGainResources && iid /= attrs.owner) do
        gainResourcesIfCan iid attrs 2
      pure s
    _ -> Leadership2 <$> liftRunMessage msg attrs
