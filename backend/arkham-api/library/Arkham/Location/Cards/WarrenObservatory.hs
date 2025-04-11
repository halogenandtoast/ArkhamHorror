module Arkham.Location.Cards.WarrenObservatory (warrenObservatory) where

import Arkham.Ability
import Arkham.Helpers.Window (discoveredClues)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype WarrenObservatory = WarrenObservatory LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

warrenObservatory :: LocationCard WarrenObservatory
warrenObservatory = symbolLabel $ location WarrenObservatory Cards.warrenObservatory 4 (PerPlayer 1)

instance HasAbilities WarrenObservatory where
  getAbilities (WarrenObservatory attrs) =
    extendRevealed1 attrs
      $ restricted attrs 1 Here
      $ forced
      $ DiscoverClues #after You (be attrs) AnyValue

instance RunMessage WarrenObservatory where
  runMessage msg l@(WarrenObservatory attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (discoveredClues -> n) _ -> do
      discardTopOfDeck iid (attrs.ability 1) (n * 3)
      pure l
    _ -> WarrenObservatory <$> liftRunMessage msg attrs
