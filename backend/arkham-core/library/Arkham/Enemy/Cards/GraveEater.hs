module Arkham.Enemy.Cards.GraveEater
  ( graveEater
  , GraveEater(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message hiding (EnemyAttacks)
import Arkham.Timing qualified as Timing

newtype GraveEater = GraveEater EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

graveEater :: EnemyCard GraveEater
graveEater = enemy GraveEater Cards.graveEater (2, Static 2, 2) (1, 1)

instance HasAbilities GraveEater where
  getAbilities (GraveEater x) = withBaseAbilities
    x
    [ mkAbility x 1
      $ ForcedAbility
      $ EnemyAttacks Timing.After You AnyEnemyAttack
      $ EnemyWithId
      $ toId x
    ]

instance RunMessage GraveEater where
  runMessage msg e@(GraveEater attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      e <$ push (RandomDiscard iid)
    _ -> GraveEater <$> runMessage msg attrs
