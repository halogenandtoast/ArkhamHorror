module Arkham.Story.Cards.MemoryOfAnAlienTranslation (memoryOfAnAlienTranslation) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog (PartnerStatus (Resolute))
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Campaigns.EdgeOfTheEarth.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype MemoryOfAnAlienTranslation = MemoryOfAnAlienTranslation StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

memoryOfAnAlienTranslation :: StoryCard MemoryOfAnAlienTranslation
memoryOfAnAlienTranslation = story MemoryOfAnAlienTranslation Cards.memoryOfAnAlienTranslation

instance RunMessage MemoryOfAnAlienTranslation where
  runMessage msg s@(MemoryOfAnAlienTranslation attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      record EllsworthHasConfrontedHisDemons
      setPartnerStatus Assets.roaldEllsworthIntrepidExplorer Resolute
      selectForMaybeM (assetIs Assets.roaldEllsworthIntrepidExplorer) \ellsworth ->
        push $ ReplaceAsset ellsworth Assets.roaldEllsworthIntrepidExplorerResolute
      addToVictory attrs
      mayAdvance attrs
      pure s
    _ -> MemoryOfAnAlienTranslation <$> liftRunMessage msg attrs
