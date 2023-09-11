module Arkham.Treachery.Cards.GraspingHands (
  GraspingHands (..),
  graspingHands,
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype GraspingHands = GraspingHands TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

graspingHands :: TreacheryCard GraspingHands
graspingHands = treachery GraspingHands Cards.graspingHands

instance RunMessage GraspingHands where
  runMessage msg t@(GraspingHands attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ revelationSkillTest iid attrs #agility 3
      pure t
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ n -> do
      push $ assignDamage iid attrs n
      pure t
    _ -> GraspingHands <$> runMessage msg attrs
