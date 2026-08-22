module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.CavernEntranceDarkestDepths (cavernEntranceDarkestDepths) where

import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted

newtype CavernEntranceDarkestDepths = CavernEntranceDarkestDepths LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cavernEntranceDarkestDepths :: LocationCard CavernEntranceDarkestDepths
cavernEntranceDarkestDepths =
  symbolLabel $ location CavernEntranceDarkestDepths Cards.cavernEntranceDarkestDepths 2 (PerPlayer 1)

instance HasAbilities CavernEntranceDarkestDepths where
  getAbilities (CavernEntranceDarkestDepths a) = extendRevealed a []

instance RunMessage CavernEntranceDarkestDepths where
  runMessage msg (CavernEntranceDarkestDepths attrs) = runQueueT $ CavernEntranceDarkestDepths <$> liftRunMessage msg attrs
