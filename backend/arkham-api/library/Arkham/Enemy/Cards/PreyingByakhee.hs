module Arkham.Enemy.Cards.PreyingByakhee (preyingByakhee) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types qualified as Field
import Arkham.Field
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Matcher

newtype PreyingByakhee = PreyingByakhee EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

preyingByakhee :: EnemyCard PreyingByakhee
preyingByakhee =
  enemyWith PreyingByakhee Cards.preyingByakhee (2, Static 3, 5) (2, 1)
    $ preyL
    .~ OnlyPrey LowestRemainingSanity

instance HasModifiersFor PreyingByakhee where
  getModifiersFor (PreyingByakhee a) = do
    ok <-
      selectAny
        $ investigatorEngagedWith a
        <> InvestigatorWithRemainingSanity (LessThanOrEqualTo $ Static 4)
    modifySelfWhen
      a
      ok
      [AlternateFightField (SomeField Field.EnemyEvade), AlternateEvadeField (SomeField Field.EnemyFight)]

instance RunMessage PreyingByakhee where
  runMessage msg (PreyingByakhee attrs) = PreyingByakhee <$> runMessage msg attrs
