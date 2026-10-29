module Arkham.Homebrew.NightOfVespers.Assets.LachrymarumsFavor (lachrymarumsFavor) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.ChaosToken (ChaosTokenFace (ElderThing))
import Arkham.Homebrew.NightOfVespers.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.NightOfVespers.Jinx (triggerJinx)
import Arkham.Matcher

newtype LachrymarumsFavor = LachrymarumsFavor AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lachrymarumsFavor :: AssetCard LachrymarumsFavor
lachrymarumsFavor = asset LachrymarumsFavor Cards.lachrymarumsFavor

instance HasAbilities LachrymarumsFavor where
  getAbilities (LachrymarumsFavor a) =
    [controlled_ a 1 $ forced $ RevealChaosToken #when You (ChaosTokenFaceIs ElderThing)]

instance RunMessage LachrymarumsFavor where
  runMessage msg a@(LachrymarumsFavor attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      putCardIntoPlay iid attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      triggerJinx iid
      toDiscardBy iid (attrs.ability 1) attrs
      pure a
    _ -> LachrymarumsFavor <$> liftRunMessage msg attrs
