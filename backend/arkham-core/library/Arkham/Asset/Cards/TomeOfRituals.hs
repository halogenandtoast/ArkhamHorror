module Arkham.Asset.Cards.TomeOfRituals (
  tomeOfRituals,
  TomeOfRituals (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype TomeOfRituals = TomeOfRituals AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tomeOfRituals :: AssetCard TomeOfRituals
tomeOfRituals = asset TomeOfRituals Cards.tomeOfRituals

instance RunMessage TomeOfRituals where
  runMessage msg a@(TomeOfRituals attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ putCardIntoPlay iid attrs
      pure a
    _ -> TomeOfRituals <$> runMessage msg attrs
