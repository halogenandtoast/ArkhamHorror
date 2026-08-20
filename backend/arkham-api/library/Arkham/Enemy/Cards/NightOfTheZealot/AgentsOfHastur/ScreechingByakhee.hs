module Arkham.Enemy.Cards.NightOfTheZealot.AgentsOfHastur.ScreechingByakhee (screechingByakhee) where

import Arkham.Enemy.CardDefs.NightOfTheZealot.AgentsOfHastur qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Modifier qualified as Modifier

newtype ScreechingByakhee = ScreechingByakhee EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

screechingByakhee :: EnemyCard ScreechingByakhee
screechingByakhee =
  enemy ScreechingByakhee Cards.screechingByakhee
    & setPrey LowestRemainingSanity

instance HasModifiersFor ScreechingByakhee where
  getModifiersFor (ScreechingByakhee attrs) = do
    minSanity <- selectAgg Min InvestigatorRemainingSanity $ investigatorEngagedWith (toId attrs)
    modifySelfWhen attrs (minSanity <= 4) [Modifier.EnemyFight 1, Modifier.EnemyEvade 1]

instance RunMessage ScreechingByakhee where
  runMessage msg (ScreechingByakhee attrs) = ScreechingByakhee <$> runMessage msg attrs
