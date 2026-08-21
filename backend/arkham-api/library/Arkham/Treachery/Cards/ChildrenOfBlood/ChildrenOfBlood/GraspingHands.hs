module Arkham.Treachery.Cards.ChildrenOfBlood.ChildrenOfBlood.GraspingHands (graspingHands) where

import Arkham.Treachery.CardDefs.ChildrenOfBlood.ChildrenOfBlood qualified as Cards
import Arkham.Treachery.Cards.NightOfTheZealot.Ghouls.GraspingHands qualified as Base
import Arkham.Treachery.Import.Lifted

newtype GraspingHands = GraspingHands Base.GraspingHands
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor, HasAbilities)

graspingHands :: TreacheryCard GraspingHands
graspingHands = treachery (GraspingHands . Base.GraspingHands) Cards.graspingHands

instance RunMessage GraspingHands where
  runMessage msg (GraspingHands inner) = GraspingHands <$> runMessage msg inner
