module Arkham.Enemy.Cards.AmaranthScarletScorn (amaranthScarletScorn) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Window (damagedAsset)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype AmaranthScarletScorn = AmaranthScarletScorn EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

amaranthScarletScorn :: EnemyCard AmaranthScarletScorn
amaranthScarletScorn = enemy AmaranthScarletScorn Cards.amaranthScarletScorn (4, Static 3, 4) (2, 1)

instance HasAbilities AmaranthScarletScorn where
  getAbilities (AmaranthScarletScorn a) =
    extend
      a
      [ restricted a 1 (enemyWithScarletKey a) $ forced $ EnemyAttacks #when Anyone AnyEnemyAttack (be a)
      , mkAbility a 2 $ SilentForcedAbility $ AssetDealtDamageOrHorror #when (SourceIsEnemy $ be a) AnyAsset
      ]

instance RunMessage AmaranthScarletScorn where
  runMessage msg e@(AmaranthScarletScorn attrs) = runQueueT $ case msg of
    InvestigatorDrawEnemy _ eid | eid == attrs.id -> do
      keysFor attrs >>= traverse_ (`createScarletKeyAt_` AttachedToEnemy attrs.id)
      AmaranthScarletScorn <$> liftRunMessage msg attrs
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      skeys <- select $ scarletKeyWithEnemy attrs.id
      chooseOneAtATimeM iid $ targets skeys shift
      pure e
    UseCardAbility _ (isSource attrs -> True) 2 (damagedAsset -> asset) _ -> do
      assetDefeated (attrs.ability 2) asset
      pure e
    _ -> AmaranthScarletScorn <$> liftRunMessage msg attrs
