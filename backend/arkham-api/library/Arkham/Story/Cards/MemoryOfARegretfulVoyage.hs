module Arkham.Story.Cards.MemoryOfARegretfulVoyage (memoryOfARegretfulVoyage) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog (PartnerStatus (Resolute))
import Arkham.CampaignLogKey
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Matcher
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype MemoryOfARegretfulVoyage = MemoryOfARegretfulVoyage StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

memoryOfARegretfulVoyage :: StoryCard MemoryOfARegretfulVoyage
memoryOfARegretfulVoyage = story MemoryOfARegretfulVoyage Cards.memoryOfARegretfulVoyage

instance RunMessage MemoryOfARegretfulVoyage where
  runMessage msg s@(MemoryOfARegretfulVoyage attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      record DyerHasConfrontedHisDemons
      setPartnerStatus Assets.professorWilliamDyerProfessorOfGeology Resolute
      selectForMaybeM (assetIs Assets.professorWilliamDyerProfessorOfGeology) \dyer ->
        push $ ReplaceAsset dyer Assets.professorWilliamDyerProfessorOfGeologyResolute
      addToVictory attrs
      mayAdvance attrs
      pure s
    _ -> MemoryOfARegretfulVoyage <$> liftRunMessage msg attrs
