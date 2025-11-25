module Arkham.Asset.Assets.AgentAriQuinnFoundationResearcher (agentAriQuinnFoundationResearcher) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype AgentAriQuinnFoundationResearcher = AgentAriQuinnFoundationResearcher AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

agentAriQuinnFoundationResearcher :: AssetCard AgentAriQuinnFoundationResearcher
agentAriQuinnFoundationResearcher = asset AgentAriQuinnFoundationResearcher Cards.agentAriQuinnFoundationResearcher

instance RunMessage AgentAriQuinnFoundationResearcher where
  runMessage msg (AgentAriQuinnFoundationResearcher attrs) = runQueueT $ case msg of
    _ -> AgentAriQuinnFoundationResearcher <$> liftRunMessage msg attrs
