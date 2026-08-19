module Arkham.Campaign.Campaigns.ChildrenOfBlood (childrenOfBlood) where

import Arkham.Campaign.Import.Lifted
import Arkham.Campaigns.ChildrenOfBlood.CampaignSteps
import Arkham.Campaigns.ChildrenOfBlood.ChaosBag
import Arkham.Campaigns.ChildrenOfBlood.Helpers
import Arkham.Helpers.FlavorText

newtype ChildrenOfBlood = ChildrenOfBlood CampaignAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

childrenOfBlood :: Difficulty -> ChildrenOfBlood
childrenOfBlood = campaign ChildrenOfBlood (CampaignId "13") "Children of blood"

instance IsCampaign ChildrenOfBlood where
  campaignTokens = chaosBagContents
  nextStep a = case campaignStep (toAttrs a) of
    PrologueStep -> continue RiverOfBlood
    RiverOfBlood -> continue NewHorizons
    NewHorizons -> continue $ InterludeStep 1 Nothing
    InterludeStep 1 _ -> continue BloodMoney
    BloodMoney -> continue EpilogueStep
    other -> defaultNextStep other

instance RunMessage ChildrenOfBlood where
  runMessage msg c@(ChildrenOfBlood _attrs) = runQueueT $ campaignI18n $ case msg of
    CampaignStep PrologueStep -> do
      scope "intro" $ flavor $ setTitle "title" >> p "body"
      scope "additionalRulesAndClarifications" do
        flavor $ setTitle "title" >> p "bloodTokens"
        flavor $ setTitle "title" >> p "predator"
      scope "prologue" $ flavor $ setTitle "title" >> p "body"
      nextCampaignStep
      pure c
    _ -> lift $ defaultCampaignRunner msg c
