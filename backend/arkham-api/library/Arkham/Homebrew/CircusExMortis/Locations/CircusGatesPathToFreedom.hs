module Arkham.Homebrew.CircusExMortis.Locations.CircusGatesPathToFreedom (
  circusGatesPathToFreedom,
) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.Helpers (getSealedMoonTokens)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype CircusGatesPathToFreedom = CircusGatesPathToFreedom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

circusGatesPathToFreedom :: LocationCard CircusGatesPathToFreedom
circusGatesPathToFreedom =
  location CircusGatesPathToFreedom Cards.circusGatesPathToFreedom 1 (Static 0)

instance HasAbilities CircusGatesPathToFreedom where
  getAbilities (CircusGatesPathToFreedom x) =
    extendRevealed x [restricted x 1 Here actionAbility]

instance RunMessage CircusGatesPathToFreedom where
  runMessage msg l@(CircusGatesPathToFreedom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- "If there are no moon tokens sealed on your investigator card: Resign."
      moons <- getSealedMoonTokens iid
      when (null moons) $ resign iid
      pure l
    _ -> CircusGatesPathToFreedom <$> liftRunMessage msg attrs
