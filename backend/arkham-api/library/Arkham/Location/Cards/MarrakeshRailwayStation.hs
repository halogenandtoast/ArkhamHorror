module Arkham.Location.Cards.MarrakeshRailwayStation (marrakeshRailwayStation) where

import Arkham.Ability
import Arkham.ForMovement
import Arkham.Helpers.Location
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenarios.DeadHeat.Helpers

newtype MarrakeshRailwayStation = MarrakeshRailwayStation LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

marrakeshRailwayStation :: LocationCard MarrakeshRailwayStation
marrakeshRailwayStation = symbolLabel $ location MarrakeshRailwayStation Cards.marrakeshRailwayStation 3 (Static 0)

instance HasAbilities MarrakeshRailwayStation where
  getAbilities (MarrakeshRailwayStation a) =
    if a.revealed
      then
        extendRevealed
          a
          [ groupLimit PerTurn $ restricted a 1 (DuringTurn Anyone <> Here) $ FastAbility' Free [#move]
          , scenarioI18n $ withI18nTooltip "marrakeshRailwayStation.resign" (locationResignAction a)
          , becomeAbandonedAbility a 3
          ]
      else extendUnrevealed1 a $ becomeAbandonedAbility a 1

instance RunMessage MarrakeshRailwayStation where
  runMessage msg l@(MarrakeshRailwayStation attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 | attrs.unrevealed -> do
      swapLocation attrs =<< fetchCard Cards.marrakeshRailwayStationAbandoned
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      connectedLocations <- select $ accessibleFrom ForMovement attrs
      chooseOrRunOneM iid $ targets connectedLocations (moveTo attrs iid)
      pure l
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      swapLocation attrs =<< fetchCard Cards.marrakeshRailwayStationAbandoned
      pure l
    _ -> MarrakeshRailwayStation <$> liftRunMessage msg attrs
