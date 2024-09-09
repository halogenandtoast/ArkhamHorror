module Arkham.Location.Cards.WarrenObservatory (
  warrenObservatory,
  WarrenObservatory (..),
)
where

import Arkham.Ability
import Arkham.Helpers.Window
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype WarrenObservatory = WarrenObservatory LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

warrenObservatory :: LocationCard WarrenObservatory
warrenObservatory = locationWith WarrenObservatory Cards.warrenObservatory 4 (PerPlayer 1) (labelL .~ "orneLibrary")

instance HasAbilities WarrenObservatory where
  getAbilities (WarrenObservatory attrs) =
    extendRevealed
      attrs
      [mkAbility attrs 1 $ forced $ DiscoverClues #after You (be attrs) AnyValue]

instance RunMessage WarrenObservatory where
  runMessage msg l@(WarrenObservatory attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (discoveredClues -> n) _ -> do
      push $ DiscardTopOfDeck iid (3 * n) (attrs.ability 1) Nothing
      pure l
    _ -> WarrenObservatory <$> liftRunMessage msg attrs
