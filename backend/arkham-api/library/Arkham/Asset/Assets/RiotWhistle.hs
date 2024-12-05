module Arkham.Asset.Assets.RiotWhistle (riotWhistle, RiotWhistle (..)) where

import Arkham.Action.Additional
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Prelude

newtype RiotWhistle = RiotWhistle AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riotWhistle :: AssetCard RiotWhistle
riotWhistle = asset RiotWhistle Cards.riotWhistle

instance HasModifiersFor RiotWhistle where
  getModifiersFor (RiotWhistle a) =
    controllerGets
      a
      [ GiveAdditionalAction
          $ AdditionalAction "Riot Whistle" (toSource a)
          $ ActionRestrictedAdditionalAction #engage
      ]

instance RunMessage RiotWhistle where
  runMessage msg (RiotWhistle attrs) = RiotWhistle <$> runMessage msg attrs
