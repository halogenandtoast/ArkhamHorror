module Arkham.Types.Enemy.Cards.TheThingThatFollows
  ( theThingThatFollows
  , TheThingThatFollows(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (EnemyDefeated)
import Arkham.Types.Prey
import Arkham.Types.Timing qualified as Timing

newtype TheThingThatFollows = TheThingThatFollows EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theThingThatFollows :: EnemyCard TheThingThatFollows
theThingThatFollows = enemyWith
  TheThingThatFollows
  Cards.theThingThatFollows
  (3, Static 2, 3)
  (1, 1)
  ((preyL .~ SetToBearer) . (spawnAtL ?~ FarthestLocationFromYou Anywhere))

instance HasAbilities TheThingThatFollows where
  getAbilities (TheThingThatFollows x) = withBaseAbilities
    x
    [ mkAbility x 1
      $ ForcedAbility
      $ EnemyWouldBeDefeated Timing.When
      $ EnemyWithId
      $ toId x
    ]

instance EnemyRunner env => RunMessage env TheThingThatFollows where
  runMessage msg e@(TheThingThatFollows attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> e <$ pushAll
      [CancelNext EnemyDefeatedMessage, ShuffleIntoDeck iid $ toTarget attrs]
    _ -> TheThingThatFollows <$> runMessage msg attrs
