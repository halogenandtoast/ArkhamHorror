module Arkham.Asset.Assets.UntilTheEndOfTime (untilTheEndOfTime, UntilTheEndOfTime (..)) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Prelude

newtype UntilTheEndOfTime = UntilTheEndOfTime AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

untilTheEndOfTime :: AssetCard UntilTheEndOfTime
untilTheEndOfTime =
  assetWith UntilTheEndOfTime Cards.untilTheEndOfTime
    $ (healthL ?~ 2)
    . (sanityL ?~ 2)

instance HasModifiersFor UntilTheEndOfTime where
  getModifiersFor (UntilTheEndOfTime a) = modifySelf a [CanBeAssignedDirectDamage]

instance RunMessage UntilTheEndOfTime where
  runMessage msg (UntilTheEndOfTime attrs) =
    UntilTheEndOfTime <$> runMessage msg attrs
