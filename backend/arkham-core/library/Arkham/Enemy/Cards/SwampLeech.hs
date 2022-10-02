module Arkham.Enemy.Cards.SwampLeech
  ( SwampLeech(..)
  , swampLeech
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype SwampLeech = SwampLeech EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

swampLeech :: EnemyCard SwampLeech
swampLeech = enemyWith
  SwampLeech
  Cards.swampLeech
  (4, Static 1, 0)
  (1, 0)
  ((spawnAtL ?~ LocationWithTrait Bayou) . (evadeL .~ Nothing))

instance HasAbilities SwampLeech where
  getAbilities (SwampLeech attrs) = do
    let actions' = getAbilities attrs
    let base = filter (not . (`abilityIs` Action.Evade)) actions'
    base
      <> [ mkAbility attrs 1
           $ ForcedAbility
           $ EnemyEnters Timing.When (LocationWithoutTrait Bayou)
           $ EnemyWithId
           $ toId attrs
         ]

instance RunMessage SwampLeech where
  runMessage msg e@(SwampLeech attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      e <$ push (Discard $ toTarget attrs)
    _ -> SwampLeech <$> runMessage msg attrs
