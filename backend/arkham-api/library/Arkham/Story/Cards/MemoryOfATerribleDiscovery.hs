module Arkham.Story.Cards.MemoryOfATerribleDiscovery (memoryOfATerribleDiscovery) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog (PartnerStatus (Resolute))
import Arkham.CampaignLogKey
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Matcher
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype MemoryOfATerribleDiscovery = MemoryOfATerribleDiscovery StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

memoryOfATerribleDiscovery :: StoryCard MemoryOfATerribleDiscovery
memoryOfATerribleDiscovery = story MemoryOfATerribleDiscovery Cards.memoryOfATerribleDiscovery

instance RunMessage MemoryOfATerribleDiscovery where
  runMessage msg s@(MemoryOfATerribleDiscovery attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      record ClaypoolHasConfrontedHisDemons
      setPartnerStatus Assets.averyClaypoolAntarcticGuide Resolute
      selectForMaybeM (assetIs Assets.averyClaypoolAntarcticGuide) \claypool ->
        push $ ReplaceAsset claypool Assets.averyClaypoolAntarcticGuideResolute
      addToVictory attrs
      mayAdvance attrs
      pure s
    _ -> MemoryOfATerribleDiscovery <$> liftRunMessage msg attrs
