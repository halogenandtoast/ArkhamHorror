module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.DescendingTunnel (descendingTunnel) where

import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted

newtype DescendingTunnel = DescendingTunnel LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

descendingTunnel :: LocationCard DescendingTunnel
descendingTunnel = symbolLabel $ location DescendingTunnel Cards.descendingTunnel 3 (PerPlayer 1)

instance HasAbilities DescendingTunnel where
  getAbilities (DescendingTunnel a) = extendRevealed a []

instance RunMessage DescendingTunnel where
  runMessage msg (DescendingTunnel attrs) = runQueueT $ DescendingTunnel <$> liftRunMessage msg attrs
