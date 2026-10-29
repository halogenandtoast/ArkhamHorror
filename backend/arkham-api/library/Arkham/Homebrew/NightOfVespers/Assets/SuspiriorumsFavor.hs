module Arkham.Homebrew.NightOfVespers.Assets.SuspiriorumsFavor (suspiriorumsFavor) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.ChaosToken (ChaosTokenFace (Tablet))
import Arkham.Homebrew.NightOfVespers.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.NightOfVespers.Jinx (triggerJinx)
import Arkham.Matcher

newtype SuspiriorumsFavor = SuspiriorumsFavor AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

suspiriorumsFavor :: AssetCard SuspiriorumsFavor
suspiriorumsFavor = asset SuspiriorumsFavor Cards.suspiriorumsFavor

instance HasAbilities SuspiriorumsFavor where
  getAbilities (SuspiriorumsFavor a) =
    [controlled_ a 1 $ forced $ RevealChaosToken #when You (ChaosTokenFaceIs Tablet)]

instance RunMessage SuspiriorumsFavor where
  runMessage msg a@(SuspiriorumsFavor attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      putCardIntoPlay iid attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      triggerJinx iid
      toDiscardBy iid (attrs.ability 1) attrs
      pure a
    _ -> SuspiriorumsFavor <$> liftRunMessage msg attrs
