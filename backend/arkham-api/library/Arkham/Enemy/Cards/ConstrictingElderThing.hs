module Arkham.Enemy.Cards.ConstrictingElderThing (constrictingElderThing) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype ConstrictingElderThing = ConstrictingElderThing EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

constrictingElderThing :: EnemyCard ConstrictingElderThing
constrictingElderThing = enemy ConstrictingElderThing Cards.constrictingElderThing (3, Static 1, 2) (1, 2)

instance HasModifiersFor ConstrictingElderThing where
  getModifiersFor (ConstrictingElderThing a) = do
    x <-
      fromMaybe 0 <$> runMaybeT do
        loc <- MaybeT $ selectOne $ LocationWithInvestigator ActiveInvestigator
        pos <- MaybeT $ field LocationPosition loc
        pure pos.row
    modifySelf a [HealthModifier (x - 1)]
    modifySelect a (investigatorEngagedWith a) [CannotMoveTo (LocationHigherThan YourLocation)]

instance RunMessage ConstrictingElderThing where
  runMessage msg (ConstrictingElderThing attrs) = runQueueT $ case msg of
    _ -> ConstrictingElderThing <$> liftRunMessage msg attrs
