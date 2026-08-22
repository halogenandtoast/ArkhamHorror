module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.LockedChamberDarkestDepths (lockedChamberDarkestDepths) where

import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted

newtype LockedChamberDarkestDepths = LockedChamberDarkestDepths LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lockedChamberDarkestDepths :: LocationCard LockedChamberDarkestDepths
lockedChamberDarkestDepths =
  symbolLabel $ location LockedChamberDarkestDepths Cards.lockedChamberDarkestDepths 4 (PerPlayer 2)

instance HasAbilities LockedChamberDarkestDepths where
  getAbilities (LockedChamberDarkestDepths a) = extendRevealed a []

instance RunMessage LockedChamberDarkestDepths where
  runMessage msg (LockedChamberDarkestDepths attrs) = runQueueT $ LockedChamberDarkestDepths <$> liftRunMessage msg attrs
