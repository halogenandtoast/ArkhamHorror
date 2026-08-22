module Arkham.Asset.Assets.ForgedPermit (forgedPermit) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype ForgedPermit = ForgedPermit AssetAttrs
  deriving anyclass (IsAsset, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forgedPermit :: AssetCard ForgedPermit
forgedPermit = asset ForgedPermit Cards.forgedPermit

instance RunMessage ForgedPermit where
  runMessage msg (ForgedPermit attrs) = ForgedPermit <$> runMessage msg attrs
