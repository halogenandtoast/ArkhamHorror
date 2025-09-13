module Arkham.Story.Cards.MemoryOfARavagedCountry (memoryOfARavagedCountry) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog (PartnerStatus (Resolute))
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Campaigns.EdgeOfTheEarth.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype MemoryOfARavagedCountry = MemoryOfARavagedCountry StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

memoryOfARavagedCountry :: StoryCard MemoryOfARavagedCountry
memoryOfARavagedCountry = story MemoryOfARavagedCountry Cards.memoryOfARavagedCountry

instance RunMessage MemoryOfARavagedCountry where
  runMessage msg s@(MemoryOfARavagedCountry attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      record CookieHasConfrontedHisDemons
      setPartnerStatus Assets.jamesCookieFredericksDubiousChoice Resolute
      selectForMaybeM (assetIs Assets.jamesCookieFredericksDubiousChoice) \cookie ->
        push $ ReplaceAsset cookie Assets.jamesCookieFredericksDubiousChoiceResolute
      addToVictory attrs
      mayAdvance attrs
      pure s
    _ -> MemoryOfARavagedCountry <$> liftRunMessage msg attrs
