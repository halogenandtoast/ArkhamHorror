module Arkham.Types.Enemy.Cards.GhoulPriest
  ( ghoulPriest
  , GhoulPriest(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Prey
import Arkham.Types.SkillType

newtype GhoulPriest = GhoulPriest EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ghoulPriest :: EnemyCard GhoulPriest
ghoulPriest = enemyWith
  GhoulPriest
  Cards.ghoulPriest
  (4, PerPlayer 5, 4)
  (2, 2)
  (preyL .~ HighestSkill SkillCombat)

instance HasModifiersFor env GhoulPriest

instance ActionRunner env => HasActions env GhoulPriest where
  getActions i window (GhoulPriest attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env GhoulPriest where
  runMessage msg (GhoulPriest attrs) = GhoulPriest <$> runMessage msg attrs
