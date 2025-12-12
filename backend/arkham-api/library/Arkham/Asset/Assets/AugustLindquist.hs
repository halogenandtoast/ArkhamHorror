module Arkham.Asset.Assets.AugustLindquist (augustLindquist) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.GameValue
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Data.Map.Strict qualified as Map

newtype AugustLindquist = AugustLindquist AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

augustLindquist :: AssetCard AugustLindquist
augustLindquist = asset AugustLindquist Cards.augustLindquist

instance HasAbilities AugustLindquist where
  getAbilities (AugustLindquist a) =
    [restricted a 1 OnSameLocation $ parleyAction $ GroupClueCost (PerPlayer 2) (locationWithAsset a)]

instance RunMessage AugustLindquist where
  runMessage msg a@(AugustLindquist attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (totalCluePaymentPerInvestigator -> spentClues) -> do
      for_ (Map.assocs spentClues) \(iid', _) -> do
        chooseOneM iid' $ withI18n do
          countVar 1 $ labeled' "takeDamage" $ assignDamage iid' (attrs.ability 1) 1
          countVar 1 $ labeled' "takeHorror" $ assignHorror iid' (attrs.ability 1) 1
      removeFromGame attrs
      for_ (toList (assetKeys attrs)) (placeKey iid)

      pure a
    _ -> AugustLindquist <$> liftRunMessage msg attrs
