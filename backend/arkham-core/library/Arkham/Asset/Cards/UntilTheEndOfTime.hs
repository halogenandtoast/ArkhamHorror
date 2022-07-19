module Arkham.Asset.Cards.UntilTheEndOfTime
  ( untilTheEndOfTime
  , UntilTheEndOfTime(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Target

newtype UntilTheEndOfTime = UntilTheEndOfTime AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

untilTheEndOfTime :: AssetCard UntilTheEndOfTime
untilTheEndOfTime = assetWith
  UntilTheEndOfTime
  Cards.untilTheEndOfTime
  ( (healthL ?~ 2)
  . (sanityL ?~ 2)
  )

instance HasModifiersFor UntilTheEndOfTime where
  getModifiersFor _ (AssetTarget aid) (UntilTheEndOfTime a) | aid == toId a =
    pure $ toModifiers a [CanBeAssignedDirectDamage]
  getModifiersFor _ _ _ = pure []

instance RunMessage UntilTheEndOfTime where
  runMessage msg (UntilTheEndOfTime attrs) =
    UntilTheEndOfTime <$> runMessage msg attrs
