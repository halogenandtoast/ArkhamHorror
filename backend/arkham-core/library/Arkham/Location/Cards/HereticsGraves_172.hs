module Arkham.Location.Cards.HereticsGraves_172 (
  hereticsGraves_172,
  HereticsGraves_172 (..),
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

newtype HereticsGraves_172 = HereticsGraves_172 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

hereticsGraves_172 :: LocationCard HereticsGraves_172
hereticsGraves_172 = location HereticsGraves_172 Cards.hereticsGraves_172 4 (Static 0)

instance HasAbilities HereticsGraves_172 where
  getAbilities (HereticsGraves_172 a) =
    withRevealedAbilities
      a
      [ mkAbility a 1
          $ ForcedAbility
          $ EnemyDefeated Timing.After Anyone ByAny
          $ enemyAt (toId a)
          <> EnemyWithTrait Witch
      , haunted "Heal 1 damage from each Heretic and each Witch enemy in play." a 2
      ]

instance RunMessage HereticsGraves_172 where
  runMessage msg l@(HereticsGraves_172 attrs) = case msg of
    Flip _ _ target | isTarget attrs target -> do
      spectral <- genCard Locations.hereticsGravesSpectral_172
      push $ ReplaceLocation (toId attrs) spectral Swap
      pure l
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      -- We normally want to avoid this, but we need to target the correct deck
      cards <- scenarioFieldMap ScenarioEncounterDeck (take 2 . unDeck)
      pushAll $ map AddToEncounterDiscard cards
      pure l
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      enemies <- selectList $ EnemyWithTrait Witch
      pushAll [HealDamage (toTarget enemy) (toSource attrs) 1 | enemy <- enemies]
      pure l
    _ -> HereticsGraves_172 <$> runMessage msg attrs
