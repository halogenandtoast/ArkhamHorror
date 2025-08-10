module Arkham.Asset.Assets.ArchiveOfConduitsGatewayToTindalos4 (archiveOfConduitsGatewayToTindalos4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Token qualified as Token

newtype ArchiveOfConduitsGatewayToTindalos4 = ArchiveOfConduitsGatewayToTindalos4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

archiveOfConduitsGatewayToTindalos4 :: AssetCard ArchiveOfConduitsGatewayToTindalos4
archiveOfConduitsGatewayToTindalos4 = asset ArchiveOfConduitsGatewayToTindalos4 Cards.archiveOfConduitsGatewayToTindalos4

instance HasAbilities ArchiveOfConduitsGatewayToTindalos4 where
  getAbilities (ArchiveOfConduitsGatewayToTindalos4 attrs) =
    [ controlled attrs 1 (exists NonEliteEnemy <> exists (be attrs <> AssetWithUses Leyline))
        $ FastAbility Free
    , controlled
        attrs
        2
        (exists $ LocationWithEnemy (EnemyWithToken Token.Leyline) <> CanEnterLocation (affectsOthers Anyone))
        actionAbility
    ]

instance RunMessage ArchiveOfConduitsGatewayToTindalos4 where
  runMessage msg a@(ArchiveOfConduitsGatewayToTindalos4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select NonEliteEnemy
      chooseTargetM iid enemies $ moveTokensTo (attrs.ability 1) attrs Leyline 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      iids <-
        select
          $ affectsOthers
          $ InvestigatorCanMoveTo (attrs.ability 2) (LocationWithEnemy $ EnemyWithToken Token.Leyline)
      chooseOrRunOneM iid $ targets iids $ handleTarget iid (attrs.ability 2)
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 2 -> True) (InvestigatorTarget iid') -> do
      locations <-
        select
          $ LocationWithEnemy (EnemyWithToken Token.Leyline)
          <> CanEnterLocation (InvestigatorWithId iid')

      chooseOrRunOneM iid do
        targets locations \location -> do
          enemies <- select $ enemyAt location <> EnemyWithToken Token.Leyline
          chooseTargetM iid enemies \enemy -> do
            moveTo (attrs.ability 2) iid' location
            chooseOneM iid do
              labeled "Do not remove Leyline" nothing
              labeled "Remove Leyline" do
                removeTokens (attrs.ability 2) (toTarget enemy) Token.Leyline 1
                nonAttackEnemyDamage (Just iid) (attrs.ability 2) 1 enemy
      pure a
    _ -> ArchiveOfConduitsGatewayToTindalos4 <$> liftRunMessage msg attrs
