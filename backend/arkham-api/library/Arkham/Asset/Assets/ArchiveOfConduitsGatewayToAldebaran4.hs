module Arkham.Asset.Assets.ArchiveOfConduitsGatewayToAldebaran4 (archiveOfConduitsGatewayToAldebaran4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Token qualified as Token

newtype ArchiveOfConduitsGatewayToAldebaran4 = ArchiveOfConduitsGatewayToAldebaran4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

archiveOfConduitsGatewayToAldebaran4 :: AssetCard ArchiveOfConduitsGatewayToAldebaran4
archiveOfConduitsGatewayToAldebaran4 = asset ArchiveOfConduitsGatewayToAldebaran4 Cards.archiveOfConduitsGatewayToAldebaran4

instance HasAbilities ArchiveOfConduitsGatewayToAldebaran4 where
  getAbilities (ArchiveOfConduitsGatewayToAldebaran4 attrs) =
    [ controlled attrs 1 (exists NonEliteEnemy <> exists (be attrs <> AssetWithUses Leyline))
        $ FastAbility Free
    , doesNotProvokeAttacksOfOpportunity
        $ controlled
          attrs
          2
          (exists $ EnemyIsEngagedWith (affectsOthers Anyone) <> EnemyWithToken Token.Leyline)
          actionAbility
    ]

instance RunMessage ArchiveOfConduitsGatewayToAldebaran4 where
  runMessage msg a@(ArchiveOfConduitsGatewayToAldebaran4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select NonEliteEnemy
      chooseTargetM iid enemies $ moveTokensTo (attrs.ability 1) attrs Leyline 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      iids <- select $ affectsOthers $ InvestigatorEngagedWith (EnemyWithToken Token.Leyline)
      chooseOrRunOneM iid $ targets iids $ handleTarget iid (attrs.ability 2)
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 2 -> True) (InvestigatorTarget iid') -> do
      enemies <- select $ EnemyWithToken Token.Leyline
      connectedLocations <-
        select $ ConnectedFrom (locationWithInvestigator iid') <> CanEnterLocation (InvestigatorWithId iid')

      chooseOrRunOneM iid do
        targets enemies \enemy -> do
          disengageEnemy iid' enemy
          chooseOrRunOneM iid $ targets connectedLocations $ moveTo (attrs.ability 2) iid'
          chooseOneM iid do
            labeled "Do not remove Leyline" nothing
            labeled "Remove Leyline" $ automaticallyEvadeEnemy iid enemy

      pure a
    _ -> ArchiveOfConduitsGatewayToAldebaran4 <$> liftRunMessage msg attrs
