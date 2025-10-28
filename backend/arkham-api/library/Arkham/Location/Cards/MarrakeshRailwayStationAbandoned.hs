module Arkham.Location.Cards.MarrakeshRailwayStationAbandoned (marrakeshRailwayStationAbandoned) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.DeadHeat.Helpers

newtype MarrakeshRailwayStationAbandoned = MarrakeshRailwayStationAbandoned LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

marrakeshRailwayStationAbandoned :: LocationCard MarrakeshRailwayStationAbandoned
marrakeshRailwayStationAbandoned =
  symbolLabel
    $ location MarrakeshRailwayStationAbandoned Cards.marrakeshRailwayStationAbandoned 2 (Static 0)

instance HasModifiersFor MarrakeshRailwayStationAbandoned where
  getModifiersFor (MarrakeshRailwayStationAbandoned a) =
    modifySelf a [AdditionalCostToLeave $ ResourceCost 1]

instance HasAbilities MarrakeshRailwayStationAbandoned where
  getAbilities (MarrakeshRailwayStationAbandoned a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "marrakeshRailwayStationAbandoned.resign" (locationResignAction a)

instance RunMessage MarrakeshRailwayStationAbandoned where
  runMessage msg (MarrakeshRailwayStationAbandoned attrs) =
    MarrakeshRailwayStationAbandoned <$> runMessage msg attrs
