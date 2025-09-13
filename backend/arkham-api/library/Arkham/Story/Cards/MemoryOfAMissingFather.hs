module Arkham.Story.Cards.MemoryOfAMissingFather (memoryOfAMissingFather) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog (PartnerStatus (Resolute))
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Campaigns.EdgeOfTheEarth.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype MemoryOfAMissingFather = MemoryOfAMissingFather StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

memoryOfAMissingFather :: StoryCard MemoryOfAMissingFather
memoryOfAMissingFather = story MemoryOfAMissingFather Cards.memoryOfAMissingFather

instance RunMessage MemoryOfAMissingFather where
  runMessage msg s@(MemoryOfAMissingFather attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      record TakadaHasConfrontedHerDemons
      setPartnerStatus Assets.takadaHirokoAeroplaneMechanic Resolute
      selectForMaybeM (assetIs Assets.takadaHirokoAeroplaneMechanic) \takada ->
        push $ ReplaceAsset takada Assets.takadaHirokoAeroplaneMechanicResolute
      addToVictory attrs
      mayAdvance attrs
      pure s
    _ -> MemoryOfAMissingFather <$> liftRunMessage msg attrs
