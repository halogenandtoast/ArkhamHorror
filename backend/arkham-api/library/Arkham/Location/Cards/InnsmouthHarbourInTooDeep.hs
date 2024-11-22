module Arkham.Location.Cards.InnsmouthHarbourInTooDeep (
  innsmouthHarbourInTooDeep,
  InnsmouthHarbourInTooDeep (..),
)
where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (connectsToAdjacent)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Scenarios.InTooDeep.Helpers

newtype InnsmouthHarbourInTooDeep = InnsmouthHarbourInTooDeep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

innsmouthHarbourInTooDeep :: LocationCard InnsmouthHarbourInTooDeep
innsmouthHarbourInTooDeep =
  locationWith
    InnsmouthHarbourInTooDeep
    Cards.innsmouthHarbourInTooDeep
    1
    (PerPlayer 1)
    connectsToAdjacent

instance HasAbilities InnsmouthHarbourInTooDeep where
  getAbilities (InnsmouthHarbourInTooDeep a) =
    extendRevealed
      a
      [ restricted a 1 (exists $ locationIs Cards.theHouseOnWaterStreetInTooDeep <> CanEnterLocation You)
          $ freeReaction
          $ Enters #after You (be a)
      , restricted a 2 Here $ ActionAbility [#parley] (ActionCost 2)
      ]

instance RunMessage InnsmouthHarbourInTooDeep where
  runMessage msg l@(InnsmouthHarbourInTooDeep attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      theHouseOnWaterStreetInTooDeep <- selectJust $ locationIs Cards.theHouseOnWaterStreetInTooDeep
      moveTo (attrs.ability 1) iid theHouseOnWaterStreetInTooDeep
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      removeBarrierBetweenConnected iid attrs.id
      pure l
    _ -> InnsmouthHarbourInTooDeep <$> liftRunMessage msg attrs
