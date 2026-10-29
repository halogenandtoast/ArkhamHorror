module Arkham.Homebrew.NightOfVespers.Assets.TenebrarumsFavor (tenebrarumsFavor) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.ChaosToken (ChaosTokenFace (Cultist))
import Arkham.Homebrew.NightOfVespers.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.NightOfVespers.Jinx (triggerJinx)
import Arkham.Matcher

newtype TenebrarumsFavor = TenebrarumsFavor AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tenebrarumsFavor :: AssetCard TenebrarumsFavor
tenebrarumsFavor = asset TenebrarumsFavor Cards.tenebrarumsFavor

instance HasAbilities TenebrarumsFavor where
  getAbilities (TenebrarumsFavor a) =
    [controlled_ a 1 $ forced $ RevealChaosToken #when You (ChaosTokenFaceIs Cultist)]

instance RunMessage TenebrarumsFavor where
  runMessage msg a@(TenebrarumsFavor attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      putCardIntoPlay iid attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      triggerJinx iid
      toDiscardBy iid (attrs.ability 1) attrs
      pure a
    _ -> TenebrarumsFavor <$> liftRunMessage msg attrs
