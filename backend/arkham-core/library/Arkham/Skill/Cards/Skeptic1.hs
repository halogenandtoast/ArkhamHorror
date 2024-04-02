module Arkham.Skill.Cards.Skeptic1 (skeptic1, Skeptic1 (..)) where

import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Prelude
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype Skeptic1 = Skeptic1 SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

skeptic1 :: SkillCard Skeptic1
skeptic1 = skill Skeptic1 Cards.skeptic1

instance HasModifiersFor Skeptic1 where
  getModifiersFor (ChaosTokenTarget token) (Skeptic1 attrs) | token.face `elem` [#bless, #curse] = do
    pure $ toModifiers attrs [ChangeChaosTokenModifier (PositiveModifier 1)]
  getModifiersFor _ _ = pure []

instance RunMessage Skeptic1 where
  runMessage msg (Skeptic1 attrs) = Skeptic1 <$> runMessage msg attrs
