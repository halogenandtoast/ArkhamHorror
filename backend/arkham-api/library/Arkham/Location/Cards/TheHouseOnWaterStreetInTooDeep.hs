module Arkham.Location.Cards.TheHouseOnWaterStreetInTooDeep (
  theHouseOnWaterStreetInTooDeep,
  TheHouseOnWaterStreetInTooDeep (..),
)
where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (connectsToAdjacent)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.InTooDeep.Helpers

newtype TheHouseOnWaterStreetInTooDeep = TheHouseOnWaterStreetInTooDeep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHouseOnWaterStreetInTooDeep :: LocationCard TheHouseOnWaterStreetInTooDeep
theHouseOnWaterStreetInTooDeep =
  locationWith
    TheHouseOnWaterStreetInTooDeep
    Cards.theHouseOnWaterStreetInTooDeep
    2
    (PerPlayer 1)
    connectsToAdjacent

instance HasAbilities TheHouseOnWaterStreetInTooDeep where
  getAbilities (TheHouseOnWaterStreetInTooDeep a) =
    extendRevealed
      a
      [ restricted a 1 UnrevealedKeyIsSetAside $ forced $ RevealLocation #after Anyone (be a)
      , restricted a 2 (Here <> thisIs a LocationWithAdjacentBarrier)
          $ FastAbility' (HandDiscardCost 1 #any) [#parley]
      ]

instance RunMessage TheHouseOnWaterStreetInTooDeep where
  runMessage msg l@(TheHouseOnWaterStreetInTooDeep attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeUnrevealedKeyOn attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      removeBarrierBetweenConnected iid attrs.id
      pure l
    _ -> TheHouseOnWaterStreetInTooDeep <$> liftRunMessage msg attrs
