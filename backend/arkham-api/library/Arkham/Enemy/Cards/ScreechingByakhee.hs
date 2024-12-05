module Arkham.Enemy.Cards.ScreechingByakhee (screechingByakhee, ScreechingByakhee (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner hiding (EnemyEvade)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Modifier qualified as Modifier
import Arkham.Prelude

newtype ScreechingByakhee = ScreechingByakhee EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

screechingByakhee :: EnemyCard ScreechingByakhee
screechingByakhee =
  enemyWith ScreechingByakhee Cards.screechingByakhee (3, Static 4, 3) (1, 2)
    $ preyL
    .~ Prey LowestRemainingSanity

instance HasModifiersFor ScreechingByakhee where
  getModifiersFor (ScreechingByakhee attrs) = do
    minSanity <- selectAgg Min InvestigatorRemainingSanity $ investigatorEngagedWith (toId attrs)
    modifySelfWhen attrs (minSanity <= 4) [Modifier.EnemyFight 1, Modifier.EnemyEvade 1]

instance RunMessage ScreechingByakhee where
  runMessage msg (ScreechingByakhee attrs) = ScreechingByakhee <$> runMessage msg attrs
