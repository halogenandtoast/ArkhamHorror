module Arkham.Location.Cards.TheLittleBookshopInTooDeep (
  theLittleBookshopInTooDeep,
  TheLittleBookshopInTooDeep (..),
)
where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (connectsToAdjacent)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.InTooDeep.Helpers

newtype TheLittleBookshopInTooDeep = TheLittleBookshopInTooDeep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theLittleBookshopInTooDeep :: LocationCard TheLittleBookshopInTooDeep
theLittleBookshopInTooDeep =
  locationWith
    TheLittleBookshopInTooDeep
    Cards.theLittleBookshopInTooDeep
    3
    (Static 1)
    connectsToAdjacent

instance HasAbilities TheLittleBookshopInTooDeep where
  getAbilities (TheLittleBookshopInTooDeep a) =
    extendRevealed
      a
      [ restricted a 1 (Here <> thisIs a LocationWithAdjacentBarrier)
          $ ActionAbility [#parley] (ActionCost 2 <> DrawEncounterCardsCost 1)
      , groupLimit PerGame
          $ restricted a 2 (Here <> HasCalculation (InvestigatorKeyCountCalculation Anyone) (atLeast 5))
          $ FastAbility' Free [#parley]
      ]

instance RunMessage TheLittleBookshopInTooDeep where
  runMessage msg l@(TheLittleBookshopInTooDeep attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      removeBarrierBetweenConnected iid attrs.id
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flashback iid Flashback6
      pure l
    _ -> TheLittleBookshopInTooDeep <$> liftRunMessage msg attrs
