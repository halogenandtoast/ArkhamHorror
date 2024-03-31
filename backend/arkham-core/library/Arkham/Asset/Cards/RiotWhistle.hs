module Arkham.Asset.Cards.RiotWhistle (riotWhistle, RiotWhistle (..)) where

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
  getModifiersFor (InvestigatorTarget iid) (RiotWhistle a) | a `controlledBy` iid = do
    pure
      $ toModifiers
        a
        [ GiveAdditionalAction
            $ AdditionalAction "Riot Whistle" (toSource a)
            $ ActionRestrictedAdditionalAction #evade
        ]
  getModifiersFor _ _ = pure []

instance RunMessage RiotWhistle where
  runMessage msg (RiotWhistle attrs) = RiotWhistle <$> runMessage msg attrs
