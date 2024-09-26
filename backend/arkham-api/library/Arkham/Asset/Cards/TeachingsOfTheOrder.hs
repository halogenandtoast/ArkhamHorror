module Arkham.Asset.Cards.TeachingsOfTheOrder
  ( teachingsOfTheOrder
  , TeachingsOfTheOrder(..)
  )
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype TeachingsOfTheOrder = TeachingsOfTheOrder AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

teachingsOfTheOrder :: AssetCard TeachingsOfTheOrder
teachingsOfTheOrder = asset TeachingsOfTheOrder Cards.teachingsOfTheOrder

instance RunMessage TeachingsOfTheOrder where
  runMessage msg (TeachingsOfTheOrder attrs) = runQueueT $ case msg of
    _ -> TeachingsOfTheOrder <$> liftRunMessage msg attrs
