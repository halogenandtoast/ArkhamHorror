module Arkham.Location.Cards.TheInnsmouthConspiracy.InTooDeep.TheLittleBookshop (
  theLittleBookshop,
  TheLittleBookshop (..),
)
where

import Arkham.Ability
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.InTooDeep qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.InTooDeep.Helpers

newtype TheLittleBookshop = TheLittleBookshop LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theLittleBookshop :: LocationCard TheLittleBookshop
theLittleBookshop =
  locationWith
    TheLittleBookshop
    Cards.theLittleBookshop
    3
    (Static 1)
    connectsToAdjacent

instance HasAbilities TheLittleBookshop where
  getAbilities (TheLittleBookshop a) =
    extendRevealed
      a
      [ restricted a 1 (Here <> thisIs a LocationWithAdjacentBarrier)
          $ ActionAbility #parley Nothing (ActionCost 2 <> DrawEncounterCardsCost 1)
      , groupLimit PerGame
          $ restricted a 2 (Here <> HasCalculation (InvestigatorKeyCountCalculation Anyone) (atLeast 5))
          $ FastAbility' Free #parley
      ]

instance RunMessage TheLittleBookshop where
  runMessage msg l@(TheLittleBookshop attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      removeBarrierBetweenConnected iid attrs.id
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flashback iid Flashback6
      pure l
    _ -> TheLittleBookshop <$> liftRunMessage msg attrs
