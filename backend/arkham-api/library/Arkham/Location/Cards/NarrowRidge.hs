module Arkham.Location.Cards.NarrowRidge (narrowRidge) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.ToTheForbiddenPeaks.Helpers

newtype NarrowRidge = NarrowRidge LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

narrowRidge :: LocationCard NarrowRidge
narrowRidge = locationWith NarrowRidge Cards.narrowRidge 2 (PerPlayer 2) (connectsToL .~ adjacentLocations)

instance HasAbilities NarrowRidge where
  getAbilities (NarrowRidge a) =
    extendRevealed1 a
      $ mkAbility a 1
      $ forced
      $ Moves #after You AnySource (below a) (be a)

instance HasModifiersFor NarrowRidge where
  getModifiersFor (NarrowRidge l) =
    whenUnrevealed l $ blockedWhenAny l $ leftOf l <> LocationWithAnyClues

instance RunMessage NarrowRidge where
  runMessage msg l@(NarrowRidge attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure l
    _ -> NarrowRidge <$> liftRunMessage msg attrs
