module Arkham.Enemy.Cards.RelentlessDarkYoung
  ( relentlessDarkYoung
  , RelentlessDarkYoung(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Prey
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype RelentlessDarkYoung = RelentlessDarkYoung EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

relentlessDarkYoung :: EnemyCard RelentlessDarkYoung
relentlessDarkYoung = enemyWith
  RelentlessDarkYoung
  Cards.relentlessDarkYoung
  (4, Static 5, 2)
  (2, 1)
  (preyL .~ LowestSkill SkillAgility)

instance HasAbilities RelentlessDarkYoung where
  getAbilities (RelentlessDarkYoung attrs) = withBaseAbilities
    attrs
    [mkAbility attrs 1 $ ForcedAbility $ RoundEnds Timing.When]

instance EnemyRunner env => RunMessage env RelentlessDarkYoung where
  runMessage msg e@(RelentlessDarkYoung attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      e <$ push (HealDamage (toTarget attrs) 2)
    _ -> RelentlessDarkYoung <$> runMessage msg attrs
