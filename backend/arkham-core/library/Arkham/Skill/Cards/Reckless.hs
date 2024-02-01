module Arkham.Skill.Cards.Reckless (
  reckless,
  Reckless (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype Reckless = Reckless SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

reckless :: SkillCard Reckless
reckless = skill Reckless Cards.reckless

instance RunMessage Reckless where
  runMessage msg s@(Reckless attrs) = case msg of
    InHand iid' (UseCardAbility iid (isSource attrs -> True) 1 _ _) | iid' == iid -> do
      pushAll
        [ RevealCard $ toCardId attrs
        , LoseResources iid (CardSource $ toCard attrs) 2
        ]
      pure s
    FailedSkillTest _ _ _ SkillTestInitiatorTarget {} _ _ -> do
      push $ ReturnToHand (skillOwner attrs) (toTarget attrs)
      pure s
    _ -> Reckless <$> runMessage msg attrs
