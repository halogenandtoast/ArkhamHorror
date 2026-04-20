module Arkham.Enemy.Cards.MutatedExperiment (mutatedExperiment) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Matcher

newtype MutatedExperiment = MutatedExperiment EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mutatedExperiment :: EnemyCard MutatedExperiment
mutatedExperiment =
  enemy MutatedExperiment Cards.mutatedExperiment (3, Static 3, 3) (1, 1)

instance HasAbilities MutatedExperiment where
    getAbilities (MutatedExperiment a) = 
        extend1 a $ restricted a 1 OnSameLocation $ forced $ EnemyDefeated #when Anyone ByAny (be a)

instance RunMessage MutatedExperiment where
  runMessage msg e@(MutatedExperiment attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
        selectEach (InvestigatorAt $ locationWithEnemy attrs) (assignHorrorTo (attrs.ability 1) 1)
        pure e
    _ -> MutatedExperiment <$> liftRunMessage msg attrs
