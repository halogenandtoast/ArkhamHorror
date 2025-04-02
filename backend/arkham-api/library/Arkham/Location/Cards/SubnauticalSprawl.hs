module Arkham.Location.Cards.SubnauticalSprawl (subnauticalSprawl) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SubnauticalSprawl = SubnauticalSprawl LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

subnauticalSprawl :: LocationCard SubnauticalSprawl
subnauticalSprawl = location SubnauticalSprawl Cards.subnauticalSprawl 2 (PerPlayer 2)

instance HasAbilities SubnauticalSprawl where
  getAbilities (SubnauticalSprawl attrs) =
    extendRevealed attrs []

instance RunMessage SubnauticalSprawl where
  runMessage msg (SubnauticalSprawl attrs) = runQueueT $ case msg of
    _ -> SubnauticalSprawl <$> liftRunMessage msg attrs
