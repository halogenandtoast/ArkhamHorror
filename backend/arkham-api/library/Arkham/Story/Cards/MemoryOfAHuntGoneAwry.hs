module Arkham.Story.Cards.MemoryOfAHuntGoneAwry (memoryOfAHuntGoneAwry) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog (PartnerStatus (Resolute))
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Campaigns.EdgeOfTheEarth.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype MemoryOfAHuntGoneAwry = MemoryOfAHuntGoneAwry StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

memoryOfAHuntGoneAwry :: StoryCard MemoryOfAHuntGoneAwry
memoryOfAHuntGoneAwry = story MemoryOfAHuntGoneAwry Cards.memoryOfAHuntGoneAwry

instance RunMessage MemoryOfAHuntGoneAwry where
  runMessage msg s@(MemoryOfAHuntGoneAwry attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      record EliyahHasConfrontedHisDemons
      setPartnerStatus Assets.eliyahAshevakDogHandler Resolute
      selectForMaybeM (assetIs Assets.eliyahAshevakDogHandler) \eliyah ->
        push $ ReplaceAsset eliyah Assets.eliyahAshevakDogHandlerResolute
      addToVictory attrs
      mayAdvance attrs
      pure s
    _ -> MemoryOfAHuntGoneAwry <$> liftRunMessage msg attrs
