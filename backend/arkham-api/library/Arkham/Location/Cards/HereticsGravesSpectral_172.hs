module Arkham.Location.Cards.HereticsGravesSpectral_172 (hereticsGravesSpectral_172) where

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
import Arkham.Trait (Trait (Witch))

newtype HereticsGravesSpectral_172 = HereticsGravesSpectral_172 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hereticsGravesSpectral_172 :: LocationCard HereticsGravesSpectral_172
hereticsGravesSpectral_172 = location HereticsGravesSpectral_172 Cards.hereticsGravesSpectral_172 4 (Static 0)

instance HasAbilities HereticsGravesSpectral_172 where
  getAbilities (HereticsGravesSpectral_172 a) =
    extendRevealed1 a
      $ mkAbility a 1
      $ forced
      $ EnemyDefeated #after Anyone ByAny
      $ enemyAt (toId a)
      <> EnemyWithTrait Witch

instance RunMessage HereticsGravesSpectral_172 where
  runMessage msg l@(HereticsGravesSpectral_172 attrs) = runQueueT $ case msg of
    FlipThis (isTarget attrs -> True) -> do
      swapLocation attrs =<< genCard Locations.hereticsGravesSpectral_172
      pure l
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- We normally want to avoid this, but we need to target the correct deck
      cards <- scenarioFieldMap ScenarioEncounterDeck (take 2 . unDeck)
      addToEncounterDiscard cards
      pure l
    _ -> HereticsGravesSpectral_172 <$> liftRunMessage msg attrs
