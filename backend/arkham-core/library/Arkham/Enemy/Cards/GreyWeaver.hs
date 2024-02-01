module Arkham.Enemy.Cards.GreyWeaver (greyWeaver, GreyWeaver (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype GreyWeaver = GreyWeaver EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

greyWeaver :: EnemyCard GreyWeaver
greyWeaver =
  enemyWith
    GreyWeaver
    Cards.greyWeaver
    (4, Static 5, 3)
    (1, 2)
    (preyL .~ Prey (InvestigatorWithLowestSkill #agility))

instance HasModifiersFor GreyWeaver where
  getModifiersFor (InvestigatorTarget iid) (GreyWeaver attrs) = do
    isReady <- toId attrs <=~> ReadyEnemy
    sameLocation <- iid <=~> InvestigatorAt (locationWithEnemy attrs)
    pure $ toModifiers attrs [CannotTakeAction #move | isReady && sameLocation]
  getModifiersFor _ _ = pure []

instance RunMessage GreyWeaver where
  runMessage msg (GreyWeaver attrs) =
    GreyWeaver <$> runMessage msg attrs
