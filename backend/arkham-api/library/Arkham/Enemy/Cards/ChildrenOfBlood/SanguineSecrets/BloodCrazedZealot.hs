module Arkham.Enemy.Cards.ChildrenOfBlood.SanguineSecrets.BloodCrazedZealot (bloodCrazedZealot) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.ChildrenOfBlood.SanguineSecrets qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype BloodCrazedZealot = BloodCrazedZealot EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodCrazedZealot :: EnemyCard BloodCrazedZealot
bloodCrazedZealot =
  enemyWith BloodCrazedZealot Cards.bloodCrazedZealot (spawnAtL ?~ SpawnAt EmptyLocation)

instance HasAbilities BloodCrazedZealot where
  getAbilities (BloodCrazedZealot a) =
    extend1 a $ restricted a 1 CanPlaceDoomOnThis $ forced $ ChaosTokenSealedOn #after Anyone #blood

instance RunMessage BloodCrazedZealot where
  runMessage msg e@(BloodCrazedZealot attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure e
    _ -> BloodCrazedZealot <$> liftRunMessage msg attrs
