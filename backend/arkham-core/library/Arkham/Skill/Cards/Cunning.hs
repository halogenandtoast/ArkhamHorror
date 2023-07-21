module Arkham.Skill.Cards.Cunning (
  cunning,
  Cunning (..),
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

newtype Cunning = Cunning SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cunning :: SkillCard Cunning
cunning =
  skill Cunning Cards.cunning

instance HasModifiersFor Cunning where
  getModifiersFor (CardIdTarget cid) (Cunning attrs) | toCardId attrs == cid =
    do
      resources <- field InvestigatorResources (skillOwner attrs)
      pure $
        toModifiers
          attrs
          [ AddSkillIcons $
            if resources >= 10
              then
                [ SkillIcon SkillIntellect
                , SkillIcon SkillIntellect
                , SkillIcon SkillAgility
                , SkillIcon SkillAgility
                ]
              else [SkillIcon SkillIntellect, SkillIcon SkillAgility]
          | resources >= 5
          ]
  getModifiersFor _ _ = pure []

instance RunMessage Cunning where
  runMessage msg (Cunning attrs) = Cunning <$> runMessage msg attrs
