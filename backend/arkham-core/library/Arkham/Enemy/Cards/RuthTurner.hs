module Arkham.Enemy.Cards.RuthTurner (
  ruthTurner,
  RuthTurner (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype RuthTurner = RuthTurner EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

ruthTurner :: EnemyCard RuthTurner
ruthTurner = enemyWith RuthTurner Cards.ruthTurner (2, Static 4, 5) (1, 0) (spawnAtL ?~ "St. Mary's Hospital")

instance HasAbilities RuthTurner where
  getAbilities (RuthTurner a) =
    withBaseAbilities a
      $ [ mkAbility a 1
            $ ForcedAbility
            $ EnemyEvaded Timing.After Anyone (EnemyWithId $ toId a)
        ]

instance RunMessage RuthTurner where
  runMessage msg e@(RuthTurner attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ AddToVictory $ toTarget attrs
      pure e
    _ -> RuthTurner <$> runMessage msg attrs
