module Arkham.Enemy.Cards.DianneDevine (dianneDevine) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Trait

newtype DianneDevine = DianneDevine EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dianneDevine :: EnemyCard DianneDevine
dianneDevine = enemy DianneDevine Cards.dianneDevine (2, Static 3, 2) (0, 0)

instance HasModifiersFor DianneDevine where
  getModifiersFor (DianneDevine a) = do
    modifySelect
      a
      (InvestigatorAt $ locationWithEnemy a)
      [CannotDiscoverClues, CannotTakeControlOfClues]

instance HasAbilities DianneDevine where
  getAbilities (DianneDevine a) = extend1 a $ mkAbility a 1 $ forced $ PhaseBegins #when #enemy

instance RunMessage DianneDevine where
  runMessage msg e@(DianneDevine attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      locations <- select $ LocationWithAsset $ AssetWithFewestClues $ AssetWithTrait Bystander
      leadChooseOneM $ targets locations $ enemyMoveTo attrs
      pure e
    _ -> DianneDevine <$> liftRunMessage msg attrs
