module Arkham.Campaign.Campaigns.TheDrownedCity (theDrownedCity) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.CampaignSteps
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Card.CardDef (CardDef)
import Arkham.Helpers.FlavorText
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log

newtype TheDrownedCity = TheDrownedCity CampaignAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

theDrownedCity :: Difficulty -> TheDrownedCity
theDrownedCity = campaign TheDrownedCity (CampaignId "11") "The Drowned City"

instance IsCampaign TheDrownedCity where
  campaignTokens = chaosBagContents
  nextStep a = case (toAttrs a).normalizedStep of
    PrologueStep -> continue OneLastJob
    OneLastJob -> continue AnOfferYouCantRefuse
    AnOfferYouCantRefuse -> continue ExpeditionToRlyeh
    -- The west/east branch out of Expedition to R'lyeh is pushed explicitly from
    -- that interlude's handler via setNextCampaignStep.
    other -> defaultNextStep other

-- Each Task: campaign-log key, the story-asset card, and its i18n label.
tasks :: [(TheDrownedCityKey, CardDef, Text)]
tasks =
  [ (WalkInFaith, Assets.walkInFaith, "walkInFaith")
  , (ToeTheLine, Assets.toeTheLine, "toeTheLine")
  , (NoPlaceLikeHome, Assets.noPlaceLikeHome, "noPlaceLikeHome")
  , (GoodMoney, Assets.goodMoney, "goodMoney")
  , (DoNoHarm, Assets.doNoHarm, "doNoHarm")
  , (ProveYourWorth, Assets.proveYourWorth, "proveYourWorth")
  , (DreamsOfDestruction, Assets.dreamsOfDestruction, "dreamsOfDestruction")
  , (PlumbTheDepths, Assets.plumbTheDepths, "plumbTheDepths")
  ]

instance RunMessage TheDrownedCity where
  runMessage msg c = runQueueT $ campaignI18n $ case msg of
    CampaignStep PrologueStep -> do
      scope "prologue" $ flavor $ setTitle "title" >> p "body"
      scope "additionalRulesAndClarifications" do
        flavor $ setTitle "title" >> p "floodTokens"
        flavor $ setTitle "title" >> p "artifacts"
        flavor $ setTitle "title" >> p "alienGlyphs"
      scope "newKeywords" $ flavor $ setTitle "title" >> p "body"
      nextCampaignStep
      pure c
    CampaignStep (InterludeStep 1 _) -> scope "anOfferYouCantRefuse" do
      flavor $ setTitle "title" >> p "interlude1"
      eachInvestigator (`forInvestigator` msg)
      doStep 2 msg
      pure c
    ForInvestigator iid (CampaignStep (InterludeStep 1 _)) -> scope "anOfferYouCantRefuse" do
      takenFlags <- traverse (\(k, _, _) -> getHasRecord k) tasks
      let isTaken key = or [t | ((k, _, _), t) <- zip tasks takenFlags, k == key]
      investigatorStoryWithChooseOneM' iid (setTitle "title" >> p "chooseTask") do
        for_ tasks \(key, cardDef, lbl) ->
          labeledValidate' (not (isTaken key)) lbl do
            record key
            addCampaignCardToDeck iid DoNotShuffleIn cardDef
            scope "task" $ scope lbl $ flavor $ setTitle "title" >> p "body"
      pure c
    DoStep 2 (CampaignStep (InterludeStep 1 _)) -> scope "anOfferYouCantRefuse" do
      flavor $ setTitle "title" >> p "interlude2"
      leadChooseOneM do
        labeled' "refuse" do
          flavor $ setTitle "title" >> p "interlude3"
          gameOver
        labeled' "accept" do
          flavor $ setTitle "title" >> p "interlude4"
          nextCampaignStep
      pure c
    CampaignStep (InterludeStep 2 _) -> scope "expeditionToRlyeh" do
      flavor $ setTitle "title" >> p "body"
      leadChooseOneM do
        labeled' "west" do
          flavor $ setTitle "title" >> p "westernExpedition"
          record TheExpeditionHeadedWest
          addCampaignCardToDeckChoice_ Assets.andyVanNortwick
          setNextCampaignStep TheWesternWall
        labeled' "east" do
          flavor $ setTitle "title" >> p "easternExpedition"
          record TheExpeditionHeadedEast
          addCampaignCardToDeckChoice_ Assets.rubyStandish
          -- TODO: swap a chaos token (remove 1 / add 1) for the remainder of the
          -- campaign, per the Eastern Expedition setup.
          setNextCampaignStep ObsidianCanyons
      pure c
    _ -> lift $ defaultCampaignRunner msg c
