module Arkham.Asset.Assets.ArchiveOfConduitsGatewayToAcheron4 (archiveOfConduitsGatewayToAcheron4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Token qualified as Token

newtype ArchiveOfConduitsGatewayToAcheron4 = ArchiveOfConduitsGatewayToAcheron4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

archiveOfConduitsGatewayToAcheron4 :: AssetCard ArchiveOfConduitsGatewayToAcheron4
archiveOfConduitsGatewayToAcheron4 = asset ArchiveOfConduitsGatewayToAcheron4 Cards.archiveOfConduitsGatewayToAcheron4

instance HasAbilities ArchiveOfConduitsGatewayToAcheron4 where
  getAbilities (ArchiveOfConduitsGatewayToAcheron4 a) =
    [ controlled a 1 (exists RevealedLocation <> exists (be a <> AssetWithUses Leyline))
        $ FastAbility Free
    , controlled
        a
        2
        (exists $ LocationWithToken Token.Leyline <> CanEnterLocation (affectsOthers Anyone))
        actionAbility
    ]

instance RunMessage ArchiveOfConduitsGatewayToAcheron4 where
  runMessage msg a@(ArchiveOfConduitsGatewayToAcheron4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select RevealedLocation
      chooseTargetM iid locations $ moveTokensTo (attrs.ability 1) attrs Leyline 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      iids <-
        select
          $ affectsOthers
          $ InvestigatorCanMoveTo (attrs.ability 2) (LocationWithToken Token.Leyline)
      chooseOrRunOneM iid $ targets iids $ handleTarget iid (attrs.ability 2)
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 2 -> True) (InvestigatorTarget iid') -> do
      locations <- select $ LocationWithToken Token.Leyline <> CanEnterLocation (InvestigatorWithId iid')
      sid <- getRandom
      chooseOrRunOneM iid do
        targets locations \location -> do
          moveTo (attrs.ability 2) iid' location
          chooseOneM iid do
            labeled "Do not remove Leyline" nothing
            labeled "Remove Leyline" do
              removeTokens (attrs.ability 2) (toTarget location) Token.Leyline 1
              investigateLocation_ sid iid' (attrs.ability 2) location

      pure a
    _ -> ArchiveOfConduitsGatewayToAcheron4 <$> liftRunMessage msg attrs
