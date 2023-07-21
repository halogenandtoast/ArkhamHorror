module Arkham.Asset.Cards.JeromeDavids (
  jeromeDavids,
  JeromeDavids (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype JeromeDavids = JeromeDavids AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jeromeDavids :: AssetCard JeromeDavids
jeromeDavids =
  allyWith JeromeDavids Cards.jeromeDavids (1, 4) (isStoryL .~ True)

instance RunMessage JeromeDavids where
  runMessage msg (JeromeDavids attrs) = JeromeDavids <$> runMessage msg attrs
