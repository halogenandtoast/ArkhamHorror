module Arkham.Asset.Assets.CherishedKeepsake1 (cherishedKeepsake1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Matcher

newtype CherishedKeepsake1 = CherishedKeepsake1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cherishedKeepsake1 :: AssetCard CherishedKeepsake1
cherishedKeepsake1 = assetWith CherishedKeepsake1 Cards.cherishedKeepsake1 (sanityL ?~ 4)

instance HasAbilities CherishedKeepsake1 where
  getAbilities (CherishedKeepsake1 a) =
    [restricted a 1 ControlsThis $ forced $ AssetDefeated #when ByHorror (be a)]

instance RunMessage CherishedKeepsake1 where
  runMessage msg a@(CherishedKeepsake1 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      exile attrs
      pure a
    _ -> CherishedKeepsake1 <$> liftRunMessage msg attrs
