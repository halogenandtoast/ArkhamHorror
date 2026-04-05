module Arkham.Asset.Assets.Canteen2 (canteen2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher

newtype Canteen2 = Canteen2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

canteen2 :: AssetCard Canteen2
canteen2 = assetWith Canteen2 Cards.canteen2 discardWhenNoUses

instance HasAbilities Canteen2 where
  getAbilities (Canteen2 a) =
    [ controlled a 1 (exists $ HealableInvestigator (a.ability 1) #horror You)
        $ FastAbility
        $ assetUseCost a Supply 1
    ]

instance RunMessage Canteen2 where
  runMessage msg a@(Canteen2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      healHorror iid (attrs.ability 1) 2
      pure a
    _ -> Canteen2 <$> liftRunMessage msg attrs
