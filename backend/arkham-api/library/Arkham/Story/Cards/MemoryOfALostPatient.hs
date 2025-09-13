module Arkham.Story.Cards.MemoryOfALostPatient (memoryOfALostPatient) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog (PartnerStatus (Resolute))
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Campaigns.EdgeOfTheEarth.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype MemoryOfALostPatient = MemoryOfALostPatient StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

memoryOfALostPatient :: StoryCard MemoryOfALostPatient
memoryOfALostPatient = story MemoryOfALostPatient Cards.memoryOfALostPatient

instance RunMessage MemoryOfALostPatient where
  runMessage msg s@(MemoryOfALostPatient attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      record DrSinhaHasConfrontedHerDemons
      setPartnerStatus Assets.drMalaSinhaDaringPhysician Resolute
      selectForMaybeM (assetIs Assets.drMalaSinhaDaringPhysician) \drSinha ->
        push $ ReplaceAsset drSinha Assets.drMalaSinhaDaringPhysicianResolute
      addToVictory attrs
      mayAdvance attrs
      pure s
    _ -> MemoryOfALostPatient <$> liftRunMessage msg attrs
