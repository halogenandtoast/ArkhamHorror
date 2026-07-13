module Arkham.Location.Cards.CircusGatesPathToFreedomCircusExMortis (
  circusGatesPathToFreedomCircusExMortis,
) where

import Arkham.Ability
import Arkham.Campaigns.CircusExMortis.Helpers (getSealedMoonTokens)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CircusGatesPathToFreedomCircusExMortis = CircusGatesPathToFreedomCircusExMortis LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

circusGatesPathToFreedomCircusExMortis :: LocationCard CircusGatesPathToFreedomCircusExMortis
circusGatesPathToFreedomCircusExMortis =
  location CircusGatesPathToFreedomCircusExMortis Cards.circusGatesPathToFreedomCircusExMortis 1 (Static 0)

instance HasAbilities CircusGatesPathToFreedomCircusExMortis where
  getAbilities (CircusGatesPathToFreedomCircusExMortis x) =
    extendRevealed x [restricted x 1 Here actionAbility]

instance RunMessage CircusGatesPathToFreedomCircusExMortis where
  runMessage msg l@(CircusGatesPathToFreedomCircusExMortis attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- "If there are no moon tokens sealed on your investigator card: Resign."
      moons <- getSealedMoonTokens iid
      when (null moons) $ resign iid
      pure l
    _ -> CircusGatesPathToFreedomCircusExMortis <$> liftRunMessage msg attrs
