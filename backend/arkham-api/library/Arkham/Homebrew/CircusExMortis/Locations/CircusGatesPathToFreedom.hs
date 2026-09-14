module Arkham.Homebrew.CircusExMortis.Locations.CircusGatesPathToFreedom (circusGatesPathToFreedom) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (hasSealedMoonToken)
import Arkham.Location.Import.Lifted

newtype CircusGatesPathToFreedom = CircusGatesPathToFreedom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

circusGatesPathToFreedom :: LocationCard CircusGatesPathToFreedom
circusGatesPathToFreedom = location CircusGatesPathToFreedom Cards.circusGatesPathToFreedom 1 (Static 0)

instance HasAbilities CircusGatesPathToFreedom where
  getAbilities (CircusGatesPathToFreedom x) =
    extendRevealed1 x $ restricted x 1 (Here <> youExist (not_ hasSealedMoonToken)) resignAction_

instance RunMessage CircusGatesPathToFreedom where
  runMessage msg l@(CircusGatesPathToFreedom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resign iid
      pure l
    _ -> CircusGatesPathToFreedom <$> liftRunMessage msg attrs
