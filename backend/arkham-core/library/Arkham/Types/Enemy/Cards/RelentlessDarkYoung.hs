module Arkham.Types.Enemy.Cards.RelentlessDarkYoung
  ( relentlessDarkYoung
  , RelentlessDarkYoung(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Message
import Arkham.Types.Prey
import Arkham.Types.SkillType

newtype RelentlessDarkYoung = RelentlessDarkYoung EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities env)

relentlessDarkYoung :: EnemyCard RelentlessDarkYoung
relentlessDarkYoung = enemyWith
  RelentlessDarkYoung
  Cards.relentlessDarkYoung
  (4, Static 5, 2)
  (2, 1)
  (preyL .~ LowestSkill SkillAgility)

instance EnemyRunner env => RunMessage env RelentlessDarkYoung where
  runMessage msg (RelentlessDarkYoung attrs) = case msg of
    EndRound ->
      pure $ RelentlessDarkYoung $ attrs & damageL %~ max 0 . subtract 2
    _ -> RelentlessDarkYoung <$> runMessage msg attrs
