module Arkham.Enemy.Cards.MrTrombly (
  mrTrombly,
  MrTrombly (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Trait (Trait (Staff))

newtype MrTrombly = MrTrombly EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mrTrombly :: EnemyCard MrTrombly
mrTrombly = enemyWith MrTrombly Cards.mrTrombly (4, Static 5, 4) (2, 1) (spawnAtL ?~ "Foyer")

instance HasAbilities MrTrombly where
  getAbilities (MrTrombly a) =
    withBaseAbilities
      a
      [ mkAbility a 1
          $ ForcedAbility
          $ EnemySpawns #after Anywhere
          $ EnemyWithId (toId a)
      ]

instance RunMessage MrTrombly where
  runMessage msg e@(MrTrombly attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      enemies <-
        selectList
          $ EnemyWithTrait Staff
          <> NotEnemy (EnemyWithId $ toId attrs)
          <> oneOf [HunterEnemy, PatrolEnemy]
      pushAll $ map ((`SendMessage` HuntersMove) . toTarget) enemies
      pure e
    _ -> MrTrombly <$> runMessage msg attrs
