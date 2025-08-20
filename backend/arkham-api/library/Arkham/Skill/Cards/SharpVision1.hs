module Arkham.Skill.Cards.SharpVision1 (sharpVision1) where

import Arkham.Action qualified as Action
import Arkham.Card
import Arkham.Constants
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest (
  getSkillTestAbilitySource,
  getSkillTestAction,
  getSkillTestInvestigator,
  withSkillTest,
 )
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype SharpVision1 = SharpVision1 SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sharpVision1 :: SkillCard SharpVision1
sharpVision1 = skill SharpVision1 Cards.sharpVision1

instance HasModifiersFor SharpVision1 where
  getModifiersFor (SharpVision1 a) = maybeModified_ a (CardIdTarget $ toCardId a) do
    Action.Investigate <- MaybeT getSkillTestAction
    AbilitySource (LocationSource _) AbilityInvestigate <- MaybeT getSkillTestAbilitySource
    pure [AddSkillIcons [#intellect, #intellect]]

instance RunMessage SharpVision1 where
  runMessage msg s@(SharpVision1 attrs) = runQueueT $ case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      void $ runMaybeT do
        Action.Investigate <- MaybeT getSkillTestAction
        AbilitySource (LocationSource _) AbilityInvestigate <- MaybeT getSkillTestAbilitySource
        iid <- MaybeT getSkillTestInvestigator
        lift $ withSkillTest \sid ->
          skillTestModifier sid attrs iid (DiscoveredClues 1)
      pure s
    _ -> SharpVision1 <$> liftRunMessage msg attrs
