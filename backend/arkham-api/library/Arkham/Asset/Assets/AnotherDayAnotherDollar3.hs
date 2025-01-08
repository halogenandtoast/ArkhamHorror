module Arkham.Asset.Assets.AnotherDayAnotherDollar3 (
  anotherDayAnotherDollar3,
  AnotherDayAnotherDollar3 (..),
)
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Prelude

newtype AnotherDayAnotherDollar3 = AnotherDayAnotherDollar3 AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anotherDayAnotherDollar3 :: AssetCard AnotherDayAnotherDollar3
anotherDayAnotherDollar3 =
  asset AnotherDayAnotherDollar3 Cards.anotherDayAnotherDollar3

instance HasModifiersFor AnotherDayAnotherDollar3 where
  getModifiersFor (AnotherDayAnotherDollar3 a) = case a.controller of
    Just iid -> modifiedWith_ a iid setActiveDuringSetup [StartingResources 2]
    Nothing -> pure mempty

instance RunMessage AnotherDayAnotherDollar3 where
  runMessage msg (AnotherDayAnotherDollar3 attrs) = AnotherDayAnotherDollar3 <$> runMessage msg attrs
