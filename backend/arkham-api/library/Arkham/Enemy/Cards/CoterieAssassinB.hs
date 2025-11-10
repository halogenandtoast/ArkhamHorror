module Arkham.Enemy.Cards.CoterieAssassinB (coterieAssassinB) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype CoterieAssassinB = CoterieAssassinB EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coterieAssassinB :: EnemyCard CoterieAssassinB
coterieAssassinB = enemy CoterieAssassinB Cards.coterieAssassinB (2, Static 2, 4) (1, 0)

instance HasAbilities CoterieAssassinB where
  getAbilities (CoterieAssassinB a) =
    extend1 a
      $ restricted
        a
        1
        (thisExists a (EnemyWithPlacement InTheShadows) <> exists (InvestigatorAt LocationWithConcealedCard))
      $ forced
      $ PhaseEnds #when #enemy

instance RunMessage CoterieAssassinB where
  runMessage msg e@(CoterieAssassinB attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      investigators <- select $ InvestigatorAt LocationWithConcealedCard
      leadChooseOneAtATimeM $ targets investigators $ initiateEnemyAttack attrs (attrs.ability 1)
      pure e
    _ -> CoterieAssassinB <$> liftRunMessage msg attrs
