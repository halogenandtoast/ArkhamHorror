module Arkham.Location.Cards.Southside_295 (southside_295) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers

newtype Southside_295 = Southside_295 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

southside_295 :: LocationCard Southside_295
southside_295 = location Southside_295 Cards.southside_295 2 (Static 0)

instance HasAbilities Southside_295 where
  getAbilities (Southside_295 a) =
    extendRevealed1 a $ fastAbility a 1 (HandDiscardAnyNumberCost #any) $ withBreaches a Here

instance RunMessage Southside_295 where
  runMessage msg l@(Southside_295 attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 _ (totalDiscardCardPayments -> n) -> do
      act <- selectJust AnyAct
      removeBreaches attrs n
      placeBreaches act n
      pure l
    _ -> Southside_295 <$> liftRunMessage msg attrs
