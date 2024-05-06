module Arkham.Asset.Cards.AstronomicalAtlas3
  ( astronomicalAtlas3
  , AstronomicalAtlas3(..)
  )
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype AstronomicalAtlas3 = AstronomicalAtlas3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

astronomicalAtlas3 :: AssetCard AstronomicalAtlas3
astronomicalAtlas3 = asset AstronomicalAtlas3 Cards.astronomicalAtlas3

instance RunMessage AstronomicalAtlas3 where
  runMessage msg (AstronomicalAtlas3 attrs) = runQueueT $ case msg of
    _ -> AstronomicalAtlas3 <$> lift (runMessage msg attrs)
