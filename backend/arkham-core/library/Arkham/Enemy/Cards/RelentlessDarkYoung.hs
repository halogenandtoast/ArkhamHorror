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
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype RelentlessDarkYoung = RelentlessDarkYoung EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

relentlessDarkYoung :: EnemyCard RelentlessDarkYoung
relentlessDarkYoung = enemyWith
  RelentlessDarkYoung
  Cards.relentlessDarkYoung
  (4, Static 5, 2)
  (2, 1)
  (preyL .~ Prey (InvestigatorWithLowestSkill SkillAgility))

instance HasAbilities RelentlessDarkYoung where
  getAbilities (RelentlessDarkYoung attrs) = withBaseAbilities
    attrs
    [mkAbility attrs 1 $ ForcedAbility $ RoundEnds Timing.When]

instance RunMessage RelentlessDarkYoung where
  runMessage msg e@(RelentlessDarkYoung attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ HealDamage (toTarget attrs) (toSource attrs) 2
      pure e
    _ -> RelentlessDarkYoung <$> runMessage msg attrs
