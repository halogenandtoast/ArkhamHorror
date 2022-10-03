module Arkham.Location.Cards.SerpentsHaven
  ( serpentsHaven
  , SerpentsHaven(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Criteria
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Treacheries

newtype SerpentsHaven = SerpentsHaven LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

serpentsHaven :: LocationCard SerpentsHaven
serpentsHaven = location SerpentsHaven Cards.serpentsHaven 2 (PerPlayer 2)

instance HasModifiersFor SerpentsHaven where
  getModifiersFor (EnemyTarget eid) (SerpentsHaven a) = do
    modified <- eid <=~> (enemyAt (toId a) <> EnemyWithTrait Serpent)
    pure $ toModifiers a [ EnemyFight 1 | modified ]
  getModifiersFor _ _ = pure []

instance HasAbilities SerpentsHaven where
  getAbilities (SerpentsHaven attrs) = withBaseAbilities
    attrs
    [ restrictedAbility
        attrs
        1
        (Here <> TreacheryExists
          (treacheryIs Treacheries.poisoned <> TreacheryInThreatAreaOf You)
        )
      $ ForcedAbility
      $ PerformAction
          Timing.After
          You
          (ActionOneOf [ActionIs Action.Investigate, ActionIs Action.Explore])
    ]

instance RunMessage SerpentsHaven where
  runMessage msg l@(SerpentsHaven attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ InvestigatorAssignDamage iid source DamageAny 1 0
      pure l
    _ -> SerpentsHaven <$> runMessage msg attrs
