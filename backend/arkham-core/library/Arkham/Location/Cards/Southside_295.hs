module Arkham.Location.Cards.Southside_295 (
  southside_295,
  Southside_295 (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers

newtype Southside_295 = Southside_295 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

southside_295 :: LocationCard Southside_295
southside_295 = location Southside_295 Cards.southside_295 2 (Static 0)

instance HasAbilities Southside_295 where
  getAbilities (Southside_295 attrs) =
    withRevealedAbilities attrs
      $ [fastAbility attrs 1 (HandDiscardAnyNumberCost AnyCard) $ withBreaches attrs Here]

countCards :: Payment -> Int
countCards (DiscardCardPayment cards) = length cards
countCards (Payments ps) = sum $ map countCards ps
countCards _ = 0

instance RunMessage Southside_295 where
  runMessage msg l@(Southside_295 attrs) = case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 _ (countCards -> n) -> do
      act <- selectJust AnyAct
      pushAll $ [RemoveBreaches (toTarget attrs) n, PlaceBreaches (toTarget act) n]
      pure l
    _ -> Southside_295 <$> runMessage msg attrs
