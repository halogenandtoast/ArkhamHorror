module Arkham.Location.Cards.WeedChokedBeach (weedChokedBeach) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype WeedChokedBeach = WeedChokedBeach LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

weedChokedBeach :: LocationCard WeedChokedBeach
weedChokedBeach = locationWith WeedChokedBeach Cards.weedChokedBeach 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities WeedChokedBeach where
  getAbilities (WeedChokedBeach a) =
    extendRevealed a []

instance RunMessage WeedChokedBeach where
  runMessage msg (WeedChokedBeach attrs) = runQueueT $ case msg of
    _ -> WeedChokedBeach <$> liftRunMessage msg attrs
