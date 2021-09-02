module Arkham.Types.Enemy.Cards.GraveEater
  ( graveEater
  , GraveEater(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (EnemyAttacks)
import qualified Arkham.Types.Timing as Timing

newtype GraveEater = GraveEater EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

graveEater :: EnemyCard GraveEater
graveEater = enemy GraveEater Cards.graveEater (2, Static 2, 2) (1, 1)

instance HasAbilities GraveEater where
  getAbilities (GraveEater x) = withBaseAbilities
    x
    [ mkAbility x 1
      $ ForcedAbility
      $ EnemyAttacks Timing.After You
      $ EnemyWithId
      $ toId x
    ]

instance EnemyRunner env => RunMessage env GraveEater where
  runMessage msg e@(GraveEater attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      e <$ push (RandomDiscard iid)
    _ -> GraveEater <$> runMessage msg attrs
