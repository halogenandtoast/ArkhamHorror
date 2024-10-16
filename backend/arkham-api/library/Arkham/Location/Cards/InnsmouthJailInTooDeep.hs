module Arkham.Location.Cards.InnsmouthJailInTooDeep (
  innsmouthJailInTooDeep,
  InnsmouthJailInTooDeep (..),
)
where

import Arkham.Ability
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (connectsToAdjacent)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.InTooDeep.Helpers

newtype InnsmouthJailInTooDeep = InnsmouthJailInTooDeep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

innsmouthJailInTooDeep :: LocationCard InnsmouthJailInTooDeep
innsmouthJailInTooDeep =
  locationWith InnsmouthJailInTooDeep Cards.innsmouthJailInTooDeep 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities InnsmouthJailInTooDeep where
  getAbilities (InnsmouthJailInTooDeep attrs) =
    extendRevealed
      attrs
      [ restricted attrs 1 (Here <> thisIs attrs LocationWithAdjacentBarrier)
          $ parleyAction
          $ ResourceCost 3
      , groupLimit PerGame
          $ restricted attrs 2 (Here <> youExist (InvestigatorWithKey BlackKey))
          $ FastAbility' Free [#parley]
      ]

instance RunMessage InnsmouthJailInTooDeep where
  runMessage msg l@(InnsmouthJailInTooDeep attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      removeBarrierBetweenConnected iid attrs.id
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flashback iid Flashback8
      pure l
    _ -> InnsmouthJailInTooDeep <$> liftRunMessage msg attrs
