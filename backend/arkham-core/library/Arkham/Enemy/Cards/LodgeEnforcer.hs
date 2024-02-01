module Arkham.Enemy.Cards.LodgeEnforcer (
  lodgeEnforcer,
  LodgeEnforcer (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher

newtype LodgeEnforcer = LodgeEnforcer EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

lodgeEnforcer :: EnemyCard LodgeEnforcer
lodgeEnforcer =
  enemyWith
    LodgeEnforcer
    Cards.lodgeEnforcer
    (3, Static 4, 3)
    (1, 1)
    ( spawnAtL
        ?~ SpawnAtFirst
          [SpawnAt $ MostBreaches $ LocationWithBreaches (AtLeast $ Static 1), "Silver Twilight Lodge"]
    )

instance HasModifiersFor LodgeEnforcer where
  getModifiersFor (LocationTarget lid) (LodgeEnforcer attrs) = do
    shouldBlank <- member lid <$> select (locationWithEnemy $ toId attrs)
    pure $ toModifiers attrs [Blank | shouldBlank]
  getModifiersFor _ _ = pure []

instance RunMessage LodgeEnforcer where
  runMessage msg (LodgeEnforcer attrs) =
    LodgeEnforcer <$> runMessage msg attrs
