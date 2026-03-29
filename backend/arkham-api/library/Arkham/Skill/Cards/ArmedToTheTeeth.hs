module Arkham.Skill.Cards.ArmedToTheTeeth (armedToTheTeeth) where

import Arkham.Helpers.SkillTest (getSkillTestSource)
import Arkham.Matcher
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype ArmedToTheTeeth = ArmedToTheTeeth SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

armedToTheTeeth :: SkillCard ArmedToTheTeeth
armedToTheTeeth = skill ArmedToTheTeeth Cards.armedToTheTeeth

instance HasModifiersFor ArmedToTheTeeth where
  getModifiersFor (ArmedToTheTeeth a) = do
    onItem <- isJust <$> runMaybeT do
      src <- MaybeT getSkillTestSource
      aid <- hoistMaybe src.asset
      guardM $ lift $ aid <=~> (#item <> assetControlledBy a.owner)
    addSkillIconsWhen a onItem [#wild, #wild]

instance RunMessage ArmedToTheTeeth where
  runMessage msg (ArmedToTheTeeth attrs) = ArmedToTheTeeth <$> runMessage msg attrs
