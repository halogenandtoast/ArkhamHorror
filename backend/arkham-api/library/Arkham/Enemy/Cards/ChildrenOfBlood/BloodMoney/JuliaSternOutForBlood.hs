module Arkham.Enemy.Cards.ChildrenOfBlood.BloodMoney.JuliaSternOutForBlood (juliaSternOutForBlood) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Matcher

newtype JuliaSternOutForBlood = JuliaSternOutForBlood EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

juliaSternOutForBlood :: EnemyCard JuliaSternOutForBlood
juliaSternOutForBlood = enemy JuliaSternOutForBlood Cards.juliaSternOutForBlood

instance HasModifiersFor JuliaSternOutForBlood where
  getModifiersFor (JuliaSternOutForBlood a) = do
    n <- getPlayerCount
    modifySelf a [HealthModifier n]

wilkesWith :: EnemyAttrs -> EnemyMatcher
wilkesWith a = EnemyWithTitle "Howard Wilkes" <> EnemyAt (locationWithEnemy a.id)

instance HasAbilities JuliaSternOutForBlood where
  getAbilities (JuliaSternOutForBlood a) =
    extend1 a $ restricted a 1 (exists $ wilkesWith a) $ forced $ PhaseEnds #when #enemy

instance RunMessage JuliaSternOutForBlood where
  runMessage msg e@(JuliaSternOutForBlood attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      whenJustM (selectOne $ wilkesWith attrs) \wilkes -> do
        nonAttackEnemyDamage Nothing attrs 2 wilkes
        doStep 1 msg
      pure e
    -- deferred: Wilkes only strikes back if he survived the damage above
    DoStep 1 (UseThisAbility _ (isSource attrs -> True) 1) -> do
      whenJustM (selectOne $ wilkesWith attrs) \wilkes ->
        nonAttackEnemyDamage Nothing wilkes 2 attrs.id
      pure e
    _ -> JuliaSternOutForBlood <$> liftRunMessage msg attrs
