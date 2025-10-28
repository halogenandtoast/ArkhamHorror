module Arkham.Enemy.Cards.TheContessaEnraged (theContessaEnraged) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.DamageEffect
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Keyword (Keyword (Hunter, Retaliate))
import Arkham.Matcher

newtype TheContessaEnraged = TheContessaEnraged EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theContessaEnraged :: EnemyCard TheContessaEnraged
theContessaEnraged = enemy TheContessaEnraged Cards.theContessaEnraged (4, PerPlayer 4, 4) (1, 1)

instance HasModifiersFor TheContessaEnraged where
  getModifiersFor (TheContessaEnraged a) = do
    hasCape <- selectAny $ AssetAttachedTo (targetIs a) <> assetIs Assets.accursedCapeShroudOfChaos
    modifySelfWhen a hasCape [DamageDealt 1, AddKeyword Hunter, AddKeyword Retaliate]

instance RunMessage TheContessaEnraged where
  runMessage msg (TheContessaEnraged attrs) = runQueueT $ case msg of
    EnemyDamaged eid damageAssignment | eid == attrs.id -> do
      mcloak <- selectOne $ assetIs Assets.accursedCapeShroudOfChaos <> AssetAttachedTo (targetIs attrs)
      TheContessaEnraged <$> case mcloak of
        Nothing -> liftRunMessage msg attrs
        Just cloak -> case damageAssignmentAmount damageAssignment of
          0 -> pure attrs
          1 -> do
            dealAssetDamage cloak (damageAssignmentSource damageAssignment) 1
            pure attrs
          n -> do
            result <-
              liftRunMessage (EnemyDamaged eid damageAssignment {damageAssignmentAmount = n - 1}) attrs
            dealAssetDamage cloak (damageAssignmentSource damageAssignment) 1
            pure result
    _ -> TheContessaEnraged <$> liftRunMessage msg attrs
