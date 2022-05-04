module Arkham.Enemy.Cards.ScreechingByakhee
  ( screechingByakhee
  , ScreechingByakhee(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Id
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Query

newtype ScreechingByakhee = ScreechingByakhee EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

screechingByakhee :: EnemyCard ScreechingByakhee
screechingByakhee = enemyWith
  ScreechingByakhee
  Cards.screechingByakhee
  (3, Static 4, 3)
  (1, 2)
  (preyL .~ Prey LowestRemainingSanity)

instance HasCount RemainingSanity env InvestigatorId => HasModifiersFor env ScreechingByakhee where
  getModifiersFor _ target (ScreechingByakhee attrs) | isTarget attrs target =
    do
      sanities <- map unRemainingSanity
        <$> traverse getCount (setToList $ enemyEngagedInvestigators attrs)
      pure $ toModifiers attrs $ if any (<= 4) sanities
        then [EnemyFight 1, EnemyEvade 1]
        else []
  getModifiersFor _ _ _ = pure []

instance EnemyRunner env => RunMessage env ScreechingByakhee where
  runMessage msg (ScreechingByakhee attrs) =
    ScreechingByakhee <$> runMessage msg attrs
