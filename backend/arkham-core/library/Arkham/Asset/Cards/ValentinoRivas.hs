module Arkham.Asset.Cards.ValentinoRivas
  ( valentinoRivas
  , ValentinoRivas(..)
  )
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype ValentinoRivas = ValentinoRivas AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

valentinoRivas :: AssetCard ValentinoRivas
valentinoRivas =
  allyWith ValentinoRivas Cards.valentinoRivas (2, 3) (isStoryL .~ True)

instance RunMessage ValentinoRivas where
  runMessage msg (ValentinoRivas attrs) = ValentinoRivas <$> runMessage msg attrs
