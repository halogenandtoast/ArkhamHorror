module Arkham.Asset.Assets.LaChicaRojaYourWatchfulShadow (laChicaRojaYourWatchfulShadow) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype LaChicaRojaYourWatchfulShadow = LaChicaRojaYourWatchfulShadow AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

laChicaRojaYourWatchfulShadow :: AssetCard LaChicaRojaYourWatchfulShadow
laChicaRojaYourWatchfulShadow = asset LaChicaRojaYourWatchfulShadow Cards.laChicaRojaYourWatchfulShadow

instance RunMessage LaChicaRojaYourWatchfulShadow where
  runMessage msg (LaChicaRojaYourWatchfulShadow attrs) = runQueueT $ case msg of
    _ -> LaChicaRojaYourWatchfulShadow <$> liftRunMessage msg attrs
