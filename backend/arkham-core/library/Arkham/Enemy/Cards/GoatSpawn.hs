module Arkham.Enemy.Cards.GoatSpawn
  ( goatSpawn
  , GoatSpawn(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message hiding (EnemyDefeated)
import Arkham.Timing qualified as Timing

newtype GoatSpawn = GoatSpawn EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

goatSpawn :: EnemyCard GoatSpawn
goatSpawn = enemy GoatSpawn Cards.goatSpawn (3, Static 3, 2) (1, 0)

instance HasAbilities GoatSpawn where
  getAbilities (GoatSpawn a) = withBaseAbilities
    a
    [ mkAbility a 1
      $ ForcedAbility
      $ EnemyDefeated Timing.When Anyone
      $ EnemyWithId
      $ toId a
    ]

instance RunMessage GoatSpawn where
  runMessage msg e@(GoatSpawn attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      investigatorIds <- getInvestigatorsAtSameLocation attrs
      e <$ pushAll
        [ InvestigatorAssignDamage iid source DamageAny 0 1
        | iid <- investigatorIds
        ]
    _ -> GoatSpawn <$> runMessage msg attrs
