module Arkham.Location.Cards.FishStreetBridge (fishStreetBridge, FishStreetBridge (..)) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype FishStreetBridge = FishStreetBridge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fishStreetBridge :: LocationCard FishStreetBridge
fishStreetBridge = location FishStreetBridge Cards.fishStreetBridge 2 (PerPlayer 1)

instance HasAbilities FishStreetBridge where
  getAbilities (FishStreetBridge a) =
    extendRevealed1 a
      $ playerLimit PerGame
      $ restricted a 1 (Here <> can.gain.resources You) doubleActionAbility

instance RunMessage FishStreetBridge where
  runMessage msg l@(FishStreetBridge attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResourcesIfCan iid (attrs.ability 1) 4
      pure l
    _ -> FishStreetBridge <$> liftRunMessage msg attrs
