module Arkham.Location.Cards.ShorewardSlumsInTooDeep (
  shorewardSlumsInTooDeep,
  ShorewardSlumsInTooDeep (..),
)
where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (connectsToAdjacent)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.InTooDeep.Helpers

newtype ShorewardSlumsInTooDeep = ShorewardSlumsInTooDeep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shorewardSlumsInTooDeep :: LocationCard ShorewardSlumsInTooDeep
shorewardSlumsInTooDeep =
  locationWith
    ShorewardSlumsInTooDeep
    Cards.shorewardSlumsInTooDeep
    2
    (PerPlayer 1)
    connectsToAdjacent

instance HasAbilities ShorewardSlumsInTooDeep where
  getAbilities (ShorewardSlumsInTooDeep a) =
    extendRevealed
      a
      [ restricted a 1 UnrevealedKeyIsSetAside $ forced $ RevealLocation #after Anyone (be a)
      , restricted a 2 (Here <> thisIs a LocationWithAdjacentBarrier)
          $ FastAbility' (DamageCost (a.ability 2) YouTarget 1) [#parley]
      ]

instance RunMessage ShorewardSlumsInTooDeep where
  runMessage msg l@(ShorewardSlumsInTooDeep attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeUnrevealedKeyOn attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      removeBarrierBetweenConnected iid attrs.id
      pure l
    _ -> ShorewardSlumsInTooDeep <$> liftRunMessage msg attrs
