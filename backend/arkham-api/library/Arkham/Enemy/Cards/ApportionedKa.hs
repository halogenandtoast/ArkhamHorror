module Arkham.Enemy.Cards.ApportionedKa (apportionedKa) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Placement

newtype ApportionedKa = ApportionedKa EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

apportionedKa :: EnemyCard ApportionedKa
apportionedKa = enemyWith ApportionedKa Cards.apportionedKa (0, Static 1, 0) (1, 1) \e ->
  e {enemyFight = Nothing, enemyHealth = Nothing, enemyEvade = Nothing}

instance HasAbilities ApportionedKa where
  getAbilities (ApportionedKa a) = extend1 a $ mkAbility a 1 $ forced $ afterExposed a

instance RunMessage ApportionedKa where
  runMessage msg e@(ApportionedKa attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectForMaybeM (enemyIs Cards.theSanguineWatcherWithTheRubySpectacles)
        $ nonAttackEnemyDamage (Just iid) (attrs.ability 1) 5
      place attrs InTheShadows
      resolveConcealed iid attrs.id
      pure e
    _ -> ApportionedKa <$> liftRunMessage msg attrs
