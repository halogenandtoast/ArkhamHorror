module Arkham.Enemy.Cards.YithianStarseeker
  ( yithianStarseeker
  , YithianStarseeker(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message hiding (EnemyAttacks)
import Arkham.Timing qualified as Timing

newtype YithianStarseeker = YithianStarseeker EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yithianStarseeker :: EnemyCard YithianStarseeker
yithianStarseeker = enemyWith
  YithianStarseeker
  Cards.yithianStarseeker
  (3, Static 4, 5)
  (2, 1)
  (spawnAtL ?~ LocationWithTitle "Another Dimension")

instance HasAbilities YithianStarseeker where
  getAbilities (YithianStarseeker attrs) = withBaseAbilities
    attrs
    [ mkAbility attrs 1
      $ ForcedAbility
      $ EnemyAttacks
          Timing.When
          (DiscardWith $ LengthIs $ GreaterThan $ Static 10)
          AnyEnemyAttack
      $ EnemyWithId
      $ toId attrs
    ]

instance RunMessage YithianStarseeker where
  runMessage msg e@(YithianStarseeker attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      e <$ push (PlaceDoom (toTarget attrs) 1)
    _ -> YithianStarseeker <$> runMessage msg attrs
