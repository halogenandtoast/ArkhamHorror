module Arkham.Homebrew.CircusExMortis.Locations.CircusGatesDoorwayToDoom (circusGatesDoorwayToDoom) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype CircusGatesDoorwayToDoom = CircusGatesDoorwayToDoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

circusGatesDoorwayToDoom :: LocationCard CircusGatesDoorwayToDoom
circusGatesDoorwayToDoom = location CircusGatesDoorwayToDoom Cards.circusGatesDoorwayToDoom 2 (Static 0)

instance HasAbilities CircusGatesDoorwayToDoom where
  getAbilities (CircusGatesDoorwayToDoom x) =
    extendRevealed1 x $ restricted x 1 Here resignAction_

instance RunMessage CircusGatesDoorwayToDoom where
  runMessage msg l@(CircusGatesDoorwayToDoom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resign iid
      pure l
    _ -> CircusGatesDoorwayToDoom <$> liftRunMessage msg attrs
