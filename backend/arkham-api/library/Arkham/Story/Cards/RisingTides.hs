module Arkham.Story.Cards.RisingTides (risingTides) where

import Arkham.Campaigns.TheDrownedCity.Helpers (decreaseFloodLevel, increaseFloodLevel)
import Arkham.Helpers.Query (getLead)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype RisingTides = RisingTides StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

risingTides :: StoryCard RisingTides
risingTides = story RisingTides Cards.risingTides

instance RunMessage RisingTides where
  runMessage msg s@(RisingTides attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      lead <- getLead

      withCthulhuLocation \cthulhuLocation -> do
        decreaseFloodLevel cthulhuLocation
        let elsewhere = CanHaveFloodLevelIncreased <> not_ (LocationWithId cthulhuLocation)
        withInvestigators <- select $ elsewhere <> LocationWithInvestigator Anyone
        candidates <- if null withInvestigators then select elsewhere else pure withInvestigators
        chooseOrRunOneM lead $ scenarioI18n do
          questionLabeled' "chooseLocationToFlood"
          targets candidates increaseFloodLevel
      do_ msg
      pure s
    Do (ResolveThisStory _ (is attrs -> True)) -> do
      rage <- getCthulhuRage
      selectEach (InvestigatorAt FullyFloodedLocation) \iid -> do
        sid <- getRandom
        onFailedByEffect sid AnyValue attrs iid do
          forInvestigator iid msg
        beginSkillTest sid iid attrs iid #agility (Fixed rage)
      pure s
    ForInvestigator iid (Do (ResolveThisStory _ (is attrs -> True))) -> do
      nonStory <- selectAny $ assetControlledBy iid <> AssetNonStory <> DiscardableAsset
      chooseOneM iid $ sharedI18n $ countVar 1 do
        labeled' "takeDamage" $ assignDamage iid attrs 1
        labeledValidate' nonStory "discardAssets" $ chooseAndDiscardAssetMatching iid attrs AssetNonStory
      pure s
    _ -> RisingTides <$> liftRunMessage msg attrs
