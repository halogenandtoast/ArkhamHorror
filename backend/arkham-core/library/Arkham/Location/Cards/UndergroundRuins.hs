module Arkham.Location.Cards.UndergroundRuins (
  undergroundRuins,
  UndergroundRuins (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Direction
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype UndergroundRuins = UndergroundRuins LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

undergroundRuins :: LocationCard UndergroundRuins
undergroundRuins =
  locationWith
    UndergroundRuins
    Cards.undergroundRuins
    2
    (PerPlayer 1)
    (connectsToL .~ setFromList [LeftOf, RightOf])

instance HasModifiersFor UndergroundRuins where
  getModifiersFor target (UndergroundRuins a) | isTarget a target = do
    hasEnemies <- selectAny $ enemyAt $ toId a
    pure $ toModifiers a [InVictoryDisplayForCountingVengeance | hasEnemies]
  getModifiersFor _ _ = pure []

instance HasAbilities UndergroundRuins where
  getAbilities (UndergroundRuins attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1
          $ ForcedAbility
          $ EnemyLeavesPlay Timing.After
          $ enemyAt
          $ toId attrs
      ]

instance RunMessage UndergroundRuins where
  runMessage msg l@(UndergroundRuins attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ PlaceDoom (toAbilitySource attrs 1) (toTarget attrs) 1
      pure l
    _ -> UndergroundRuins <$> runMessage msg attrs
