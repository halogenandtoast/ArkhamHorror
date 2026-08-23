module Arkham.Enemy.Cards.ChildrenOfBlood.BloodMoney.JuliaSternChildOfVengeance (juliaSternChildOfVengeance) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Matcher

newtype JuliaSternChildOfVengeance = JuliaSternChildOfVengeance EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

juliaSternChildOfVengeance :: EnemyCard JuliaSternChildOfVengeance
juliaSternChildOfVengeance = enemy JuliaSternChildOfVengeance Cards.juliaSternChildOfVengeance

instance HasModifiersFor JuliaSternChildOfVengeance where
  getModifiersFor (JuliaSternChildOfVengeance a) = do
    n <- getPlayerCount
    modifySelf a [HealthModifier n]

wilkesWith :: EnemyAttrs -> EnemyMatcher
wilkesWith a = EnemyWithTitle "Howard Wilkes" <> EnemyAt (locationWithEnemy a.id)

instance HasAbilities JuliaSternChildOfVengeance where
  getAbilities (JuliaSternChildOfVengeance a) =
    extend1 a $ restricted a 1 (exists $ wilkesWith a) $ forced $ PhaseEnds #when #enemy

instance RunMessage JuliaSternChildOfVengeance where
  runMessage msg e@(JuliaSternChildOfVengeance attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      whenJustM (selectOne $ wilkesWith attrs) \wilkes -> do
        nonAttackEnemyDamage Nothing attrs 1 wilkes
        doStep 1 msg
      pure e
    -- deferred: Wilkes only strikes back if he survived the damage above
    DoStep 1 (UseThisAbility _ (isSource attrs -> True) 1) -> do
      whenJustM (selectOne $ wilkesWith attrs) \wilkes ->
        nonAttackEnemyDamage Nothing wilkes 2 attrs.id
      pure e
    _ -> JuliaSternChildOfVengeance <$> liftRunMessage msg attrs
