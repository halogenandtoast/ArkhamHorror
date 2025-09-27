module Arkham.Location.Cards.HereticsGraves_172 (hereticsGraves_172) where

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers
import Arkham.Helpers.Location
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Types (Field (..))
import Arkham.Scenarios.TheWagesOfSin.Helpers
import Arkham.Trait (Trait (Witch))

newtype HereticsGraves_172 = HereticsGraves_172 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hereticsGraves_172 :: LocationCard HereticsGraves_172
hereticsGraves_172 = location HereticsGraves_172 Cards.hereticsGraves_172 4 (Static 0)

instance HasAbilities HereticsGraves_172 where
  getAbilities (HereticsGraves_172 a) =
    extendRevealed
      a
      [ mkAbility a 1
          $ forced
          $ EnemyDefeated #after Anyone ByAny
          $ enemyAt (toId a)
          <> EnemyWithTrait Witch
      , scenarioI18n $ hauntedI "hereticsGraves_172.haunted" a 2
      ]

instance RunMessage HereticsGraves_172 where
  runMessage msg l@(HereticsGraves_172 attrs) = runQueueT $ case msg of
    FlipThis (isTarget attrs -> True) -> do
      swapLocation attrs =<< genCard Locations.hereticsGravesSpectral_172
      pure l
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- We normally want to avoid this, but we need to target the correct deck
      cards <- scenarioFieldMap ScenarioEncounterDeck (take 2 . unDeck)
      addToEncounterDiscard cards
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      enemies <- select $ EnemyWithTrait Witch
      pushAll [HealDamage (toTarget enemy) (toSource attrs) 1 | enemy <- enemies]
      pure l
    _ -> HereticsGraves_172 <$> liftRunMessage msg attrs
