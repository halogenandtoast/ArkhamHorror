module Arkham.Asset.Cards.Dendromorphosis (
  dendromorphosis,
  Dendromorphosis (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Window (defaultWindows)

newtype Dendromorphosis = Dendromorphosis AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dendromorphosis :: AssetCard Dendromorphosis
dendromorphosis =
  assetWith Dendromorphosis Cards.dendromorphosis
    $ (canLeavePlayByNormalMeansL .~ False)
    . (healthL ?~ 1)

instance RunMessage Dendromorphosis where
  runMessage msg a@(Dendromorphosis attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ PutCardIntoPlay iid (toCard attrs) Nothing (defaultWindows iid)
      pure a
    _ -> Dendromorphosis <$> runMessage msg attrs
