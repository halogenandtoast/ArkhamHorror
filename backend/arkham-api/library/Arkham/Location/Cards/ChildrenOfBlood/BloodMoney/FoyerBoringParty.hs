module Arkham.Location.Cards.ChildrenOfBlood.BloodMoney.FoyerBoringParty (foyerBoringParty) where

import Arkham.Location.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Location.Import.Lifted

newtype FoyerBoringParty = FoyerBoringParty LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foyerBoringParty :: LocationCard FoyerBoringParty
foyerBoringParty = symbolLabel $ location FoyerBoringParty Cards.foyerBoringParty 2 (Static 0)

instance HasAbilities FoyerBoringParty where
  getAbilities (FoyerBoringParty a) = extendRevealed a []

instance RunMessage FoyerBoringParty where
  runMessage msg (FoyerBoringParty attrs) = runQueueT $ FoyerBoringParty <$> liftRunMessage msg attrs
