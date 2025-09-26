module Arkham.Location.Cards.LandlordsQuarters (landlordsQuarters) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.TheSecretName.Helpers

newtype LandlordsQuarters = LandlordsQuarters LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

landlordsQuarters :: LocationCard LandlordsQuarters
landlordsQuarters = location LandlordsQuarters Cards.landlordsQuarters 2 (PerPlayer 1)

instance HasAbilities LandlordsQuarters where
  getAbilities (LandlordsQuarters a) =
    extendRevealed
      a
      [ restricted a 1 Here $ forced $ RevealLocation #after You (be a)
      , scenarioI18n $ hauntedI "landlordsQuarters.haunted" a 2
      ]

instance RunMessage LandlordsQuarters where
  runMessage msg l@(LandlordsQuarters attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      findEncounterCard iid attrs (cardIs Enemies.swarmOfRats)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      findEncounterCard iid attrs (cardIs Enemies.swarmOfRats)
      pure l
    FoundEncounterCard _ (isTarget attrs -> True) (toCard -> card) -> do
      createEnemyAtLocationMatching_ card (locationIs Locations.moldyHalls)
      pure l
    _ -> LandlordsQuarters <$> liftRunMessage msg attrs
