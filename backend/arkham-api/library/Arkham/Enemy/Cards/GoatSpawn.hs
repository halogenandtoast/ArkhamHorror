module Arkham.Enemy.Cards.GoatSpawn (
  goatSpawn,
  GoatSpawn (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype GoatSpawn = GoatSpawn EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

goatSpawn :: EnemyCard GoatSpawn
goatSpawn = enemy GoatSpawn Cards.goatSpawn (3, Static 3, 2) (1, 0)

instance HasAbilities GoatSpawn where
  getAbilities (GoatSpawn a) =
    withBaseAbilities a
      $ [ forcedAbility a 1
            $ EnemyDefeated Timing.When Anyone ByAny
            $ EnemyWithId (toId a)
        ]

instance RunMessage GoatSpawn where
  runMessage msg e@(GoatSpawn attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      investigators <- getInvestigatorsAtSameLocation attrs
      pushAll [assignHorror investigator attrs 1 | investigator <- investigators]
      pure e
    _ -> GoatSpawn <$> runMessage msg attrs
