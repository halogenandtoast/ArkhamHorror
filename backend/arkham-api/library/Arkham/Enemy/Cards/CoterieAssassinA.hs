module Arkham.Enemy.Cards.CoterieAssassinA (coterieAssassinA) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype CoterieAssassinA = CoterieAssassinA EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coterieAssassinA :: EnemyCard CoterieAssassinA
coterieAssassinA = enemy CoterieAssassinA Cards.coterieAssassinA (2, Static 2, 4) (1, 0)

instance HasAbilities CoterieAssassinA where
  getAbilities (CoterieAssassinA a) =
    extend1 a
      $ restricted
        a
        1
        (thisExists a (EnemyWithPlacement InTheShadows) <> exists (InvestigatorAt LocationWithConcealedCard))
      $ forced
      $ PhaseEnds #when #enemy

instance RunMessage CoterieAssassinA where
  runMessage msg e@(CoterieAssassinA attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      investigators <- select $ InvestigatorAt LocationWithConcealedCard
      leadChooseOneAtATimeM $ targets investigators $ initiateEnemyAttack attrs (attrs.ability 1)
      pure e
    _ -> CoterieAssassinA <$> liftRunMessage msg attrs
