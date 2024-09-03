module Arkham.Location.Cards.HereticsGravesSpectral_172 (
  hereticsGravesSpectral_172,
  HereticsGravesSpectral_172 (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Scenario.Types (Field (..))
import Arkham.Timing qualified as Timing
import Arkham.Trait (Trait (Witch))

newtype HereticsGravesSpectral_172 = HereticsGravesSpectral_172 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hereticsGravesSpectral_172 :: LocationCard HereticsGravesSpectral_172
hereticsGravesSpectral_172 = location HereticsGravesSpectral_172 Cards.hereticsGravesSpectral_172 4 (Static 0)

instance HasAbilities HereticsGravesSpectral_172 where
  getAbilities (HereticsGravesSpectral_172 a) =
    withRevealedAbilities
      a
      [ mkAbility a 1
          $ ForcedAbility
          $ EnemyDefeated Timing.After Anyone ByAny
          $ enemyAt (toId a)
          <> EnemyWithTrait Witch
      ]

instance RunMessage HereticsGravesSpectral_172 where
  runMessage msg l@(HereticsGravesSpectral_172 attrs) = case msg of
    Flip _ _ target | isTarget attrs target -> do
      spectral <- genCard Locations.hereticsGravesSpectral_172
      push $ ReplaceLocation (toId attrs) spectral Swap
      pure l
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      -- We normally want to avoid this, but we need to target the correct deck
      cards <- scenarioFieldMap ScenarioEncounterDeck (take 2 . unDeck)
      pushAll $ map AddToEncounterDiscard cards
      pure l
    _ -> HereticsGravesSpectral_172 <$> runMessage msg attrs
