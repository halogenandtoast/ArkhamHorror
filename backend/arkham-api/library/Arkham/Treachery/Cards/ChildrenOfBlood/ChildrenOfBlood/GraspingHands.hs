module Arkham.Treachery.Cards.ChildrenOfBlood.ChildrenOfBlood.GraspingHands (graspingHands) where

import Arkham.Treachery.CardDefs.ChildrenOfBlood.ChildrenOfBlood qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype GraspingHands = GraspingHands TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

graspingHands :: TreacheryCard GraspingHands
graspingHands = treachery GraspingHands Cards.graspingHands

instance RunMessage GraspingHands where
  runMessage msg t@(GraspingHands attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> GraspingHands <$> liftRunMessage msg attrs
