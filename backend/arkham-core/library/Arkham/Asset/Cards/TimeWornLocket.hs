module Arkham.Asset.Cards.TimeWornLocket (
  timeWornLocket,
  TimeWornLocket (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype TimeWornLocket = TimeWornLocket AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timeWornLocket :: AssetCard TimeWornLocket
timeWornLocket = asset TimeWornLocket Cards.timeWornLocket

instance RunMessage TimeWornLocket where
  runMessage msg a@(TimeWornLocket attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ putCardIntoPlay iid attrs
      pure a
    _ -> TimeWornLocket <$> runMessage msg attrs
