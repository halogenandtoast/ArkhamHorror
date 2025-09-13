module Arkham.Story.Cards.MemoryOfAnUnspeakableEvil (memoryOfAnUnspeakableEvil) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog (PartnerStatus (Resolute))
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Campaigns.EdgeOfTheEarth.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype MemoryOfAnUnspeakableEvil = MemoryOfAnUnspeakableEvil StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

memoryOfAnUnspeakableEvil :: StoryCard MemoryOfAnUnspeakableEvil
memoryOfAnUnspeakableEvil = story MemoryOfAnUnspeakableEvil Cards.memoryOfAnUnspeakableEvil

instance RunMessage MemoryOfAnUnspeakableEvil where
  runMessage msg s@(MemoryOfAnUnspeakableEvil attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      record DanforthHasConfrontedHisDemons
      setPartnerStatus Assets.danforthBrilliantStudent Resolute
      selectForMaybeM (assetIs Assets.danforthBrilliantStudent) \danforth ->
        push $ ReplaceAsset danforth Assets.danforthBrilliantStudentResolute
      addToVictory attrs
      mayAdvance attrs
      pure s
    _ -> MemoryOfAnUnspeakableEvil <$> liftRunMessage msg attrs
