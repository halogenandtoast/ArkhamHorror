module Arkham.Enemy.Cards.TracesToNowhere.ThoseWhichGrasp (thoseWhichGrasp) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TracesToNowhere.Helpers

newtype ThoseWhichGrasp = ThoseWhichGrasp EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thoseWhichGrasp :: EnemyCard ThoseWhichGrasp
thoseWhichGrasp = enemy ThoseWhichGrasp Cards.thoseWhichGrasp

instance HasAbilities ThoseWhichGrasp where
  getAbilities (ThoseWhichGrasp a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyAttacks #after You AnyEnemyAttack (be a)

instance RunMessage ThoseWhichGrasp where
  runMessage msg e@(ThoseWhichGrasp attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <- select $ assetControlledBy iid
      chooseTargetM iid assets abductAsset
      pure e
    _ -> ThoseWhichGrasp <$> liftRunMessage msg attrs
