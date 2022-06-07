module Arkham.Enemy.Cards.ScreechingByakhee
  ( screechingByakhee
  , ScreechingByakhee(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner hiding ( EnemyEvade )
import Arkham.Investigator.Attrs ( Field (..) )
import Arkham.Matcher
import Arkham.Modifier qualified as Modifier
import Arkham.Projection

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

instance HasModifiersFor ScreechingByakhee where
  getModifiersFor _ target (ScreechingByakhee attrs) | isTarget attrs target =
    do
      sanities <- traverse
        (field InvestigatorRemainingSanity)
        (setToList $ enemyEngagedInvestigators attrs)
      pure $ toModifiers attrs $ if any (<= 4) sanities
        then [Modifier.EnemyFight 1, Modifier.EnemyEvade 1]
        else []
  getModifiersFor _ _ _ = pure []

instance RunMessage ScreechingByakhee where
  runMessage msg (ScreechingByakhee attrs) =
    ScreechingByakhee <$> runMessage msg attrs
