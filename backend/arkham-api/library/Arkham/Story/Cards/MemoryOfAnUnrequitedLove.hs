module Arkham.Story.Cards.MemoryOfAnUnrequitedLove (memoryOfAnUnrequitedLove) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog (PartnerStatus (Resolute))
import Arkham.CampaignLogKey
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Matcher
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype MemoryOfAnUnrequitedLove = MemoryOfAnUnrequitedLove StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

memoryOfAnUnrequitedLove :: StoryCard MemoryOfAnUnrequitedLove
memoryOfAnUnrequitedLove = story MemoryOfAnUnrequitedLove Cards.memoryOfAnUnrequitedLove

instance RunMessage MemoryOfAnUnrequitedLove where
  runMessage msg s@(MemoryOfAnUnrequitedLove attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      record DrKenslerHasConfrontedHerDemons
      setPartnerStatus Assets.drAmyKenslerProfessorOfBiology Resolute
      selectForMaybeM (assetIs Assets.drAmyKenslerProfessorOfBiology) \drKensler ->
        push $ ReplaceAsset drKensler Assets.drAmyKenslerProfessorOfBiologyResolute
      addToVictory attrs
      mayAdvance attrs
      pure s
    _ -> MemoryOfAnUnrequitedLove <$> liftRunMessage msg attrs
