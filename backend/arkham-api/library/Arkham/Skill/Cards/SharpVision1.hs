module Arkham.Skill.Cards.SharpVision1 (sharpVision1, SharpVision1 (..)) where

import Arkham.Action qualified as Action
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
  getModifiersFor target (SharpVision1 a) | a `is` target = do
    mAction <- getSkillTestAction
    mSource <- getSkillTestSource
    case (mAction, mSource) of
      (Just Action.Investigate, Just (AbilitySource (LocationSource _) AbilityInvestigate)) -> pure $ toModifiers a [AddSkillIcons [#intellect, #intellect]]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage SharpVision1 where
  runMessage msg s@(SharpVision1 attrs) = case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      mAction <- getSkillTestAction
      mSource <- getSkillTestSource
      mInvestigator <- getSkillTestInvestigator
      case (mAction, mSource, mInvestigator) of
        (Just Action.Investigate, Just (AbilitySource (LocationSource _) AbilityInvestigate), Just iid) -> do
          withSkillTest \sid ->
            push $ skillTestModifier sid (toSource attrs) iid (DiscoveredClues 1)
        _ -> pure ()
      pure s
    _ -> SharpVision1 <$> runMessage msg attrs
