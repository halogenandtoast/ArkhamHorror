module Arkham.Asset.Assets.HorrorInClaySculptureOfDreams (horrorInClay) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype HorrorInClaySculptureOfDreams = HorrorInClaySculptureOfDreams AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- TODO: abilities
horrorInClay :: AssetCard HorrorInClaySculptureOfDreams
horrorInClay = asset HorrorInClaySculptureOfDreams Cards.horrorInClay

instance RunMessage HorrorInClaySculptureOfDreams where
  runMessage msg (HorrorInClaySculptureOfDreams attrs) =
    runQueueT $ HorrorInClaySculptureOfDreams <$> liftRunMessage msg attrs
