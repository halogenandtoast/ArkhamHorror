module Arkham.Skill.Cards.Curiosity (
  curiosity,
  Curiosity (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner
import Arkham.SkillType

newtype Curiosity = Curiosity SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

curiosity :: SkillCard Curiosity
curiosity =
  skill Curiosity Cards.curiosity

instance HasModifiersFor Curiosity where
  getModifiersFor (CardIdTarget cid) (Curiosity attrs) | toCardId attrs == cid =
    do
      cardsInHand <- fieldMap InvestigatorHand length (skillOwner attrs)
      pure
        $ toModifiers
          attrs
          [ AddSkillIcons
            $ if cardsInHand >= 7
              then
                [ SkillIcon SkillWillpower
                , SkillIcon SkillWillpower
                , SkillIcon SkillIntellect
                , SkillIcon SkillIntellect
                ]
              else [SkillIcon SkillWillpower, SkillIcon SkillIntellect]
          | cardsInHand >= 4
          ]
  getModifiersFor _ _ = pure []

instance RunMessage Curiosity where
  runMessage msg (Curiosity attrs) = Curiosity <$> runMessage msg attrs
