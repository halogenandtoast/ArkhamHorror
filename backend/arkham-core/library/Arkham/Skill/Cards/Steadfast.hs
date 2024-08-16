module Arkham.Skill.Cards.Steadfast (
  steadfast,
  Steadfast (..),
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner
import Arkham.SkillType

newtype Steadfast = Steadfast SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

steadfast :: SkillCard Steadfast
steadfast = skill Steadfast Cards.steadfast

instance HasModifiersFor Steadfast where
  getModifiersFor (CardIdTarget cid) (Steadfast attrs) | toCardId attrs == cid =
    do
      remainingHealth <- field InvestigatorRemainingHealth (skillOwner attrs)
      remainingSanity <- field InvestigatorRemainingSanity (skillOwner attrs)
      let total = remainingHealth + remainingSanity
      pure
        $ toModifiers
          attrs
          [ AddSkillIcons
            $ if total >= 10
              then [#willpower, #willpower, #combat, #combat]
              else [#willpower, #combat]
          | total >= 5
          ]
  getModifiersFor _ _ = pure []

instance RunMessage Steadfast where
  runMessage msg (Steadfast attrs) = Steadfast <$> runMessage msg attrs
