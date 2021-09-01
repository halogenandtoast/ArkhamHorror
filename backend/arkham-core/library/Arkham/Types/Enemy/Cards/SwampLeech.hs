module Arkham.Types.Enemy.Cards.SwampLeech
  ( SwampLeech(..)
  , swampLeech
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Matcher
import Arkham.Types.Message
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Trait

newtype SwampLeech = SwampLeech EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

swampLeech :: EnemyCard SwampLeech
swampLeech = enemyWith
  SwampLeech
  Cards.swampLeech
  (4, Static 1, 0)
  (1, 0)
  (spawnAtL ?~ LocationWithTrait Bayou)

instance HasAbilities env SwampLeech where
  getAbilities i window (SwampLeech attrs) = do
    actions' <- getAbilities i window attrs
    let base = filter (not . (`abilityIs` Action.Evade)) actions'
    pure
      $ base
      <> [ mkAbility attrs 1
           $ ForcedAbility
           $ EnemyEnters Timing.When (LocationWithoutTrait Bayou)
           $ EnemyWithId
           $ toId attrs
         ]

instance EnemyRunner env => RunMessage env SwampLeech where
  runMessage msg e@(SwampLeech attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      e <$ push (Discard $ toTarget attrs)
    _ -> SwampLeech <$> runMessage msg attrs
