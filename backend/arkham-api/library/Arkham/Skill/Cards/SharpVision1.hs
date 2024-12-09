module Arkham.Skill.Cards.SharpVision1 (sharpVision1, SharpVision1 (..)) where

import Arkham.Action qualified as Action
import Arkham.Card
import Arkham.Classes
import Arkham.Constants
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest
import Arkham.Prelude
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype SharpVision1 = SharpVision1 SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sharpVision1 :: SkillCard SharpVision1
sharpVision1 = skill SharpVision1 Cards.sharpVision1

instance HasModifiersFor SharpVision1 where
  getModifiersFor (SharpVision1 a) = maybeModified_ a (CardIdTarget $ toCardId a) do
    Action.Investigate <- MaybeT getSkillTestAction
    AbilitySource (LocationSource _) AbilityInvestigate <- MaybeT getSkillTestSource
    pure [AddSkillIcons [#intellect, #intellect]]

instance RunMessage SharpVision1 where
  runMessage msg s@(SharpVision1 attrs) = case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      mAction <- getSkillTestAction
      mSource <- getSkillTestSource
      mInvestigator <- getSkillTestInvestigator
      case (mAction, mSource, mInvestigator) of
        (Just Action.Investigate, Just (AbilitySource (LocationSource _) AbilityInvestigate), Just iid) -> do
          withSkillTest \sid ->
            pushM $ skillTestModifier sid (toSource attrs) iid (DiscoveredClues 1)
        _ -> pure ()
      pure s
    _ -> SharpVision1 <$> runMessage msg attrs
