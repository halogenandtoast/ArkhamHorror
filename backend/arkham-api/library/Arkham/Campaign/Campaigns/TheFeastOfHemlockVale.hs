module Arkham.Campaign.Campaigns.TheFeastOfHemlockVale (theFeastOfHemlockVale) where

import Arkham.Campaign.Import.Lifted
import Arkham.Campaigns.TheFeastOfHemlockVale.CampaignSteps
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.ChaosToken
import Arkham.Helpers.FlavorText

newtype TheFeastOfHemlockVale = TheFeastOfHemlockVale CampaignAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

theFeastOfHemlockVale :: Difficulty -> TheFeastOfHemlockVale
theFeastOfHemlockVale = campaign TheFeastOfHemlockVale (CampaignId "10") "The Feast of Hemlock Vale"

{- FOURMOLU_DISABLE -}
campaignChaosBag :: Difficulty -> [ChaosTokenFace]
campaignChaosBag = \case
  Easy ->
    [ #"+1", #"+1", #"0", #"0", #"0", #"-1", #"-1", #"-1", #"-2", #"-2", #"-3"
    , Skull, Skull, ElderThing
    ]
  Standard ->
    [ #"+1", #"0", #"0", #"-1", #"-1", #"-1", #"-2", #"-2", #"-3", #"-3", #"-4"
    , Skull, Skull, ElderThing
    ]
  Hard ->
    [ #"0", #"0", #"0", #"-1", #"-1", #"-2", #"-2", #"-3", #"-3", #"-5", #"-5", #"-7"
    , Skull, Skull, ElderThing
    ]
  Expert ->
    [ #"0", #"-1", #"-1", #"-2", #"-2", #"-3", #"-3", #"-4", #"-5", #"-5", #"-6", #"-6", #"-8"
    , Skull, Skull, ElderThing
    ]
{- FOURMOLU_ENABLE -}

instance IsCampaign TheFeastOfHemlockVale where
  campaignTokens = campaignChaosBag
  nextStep a = case campaignStep (toAttrs a) of
    PrologueStep -> continue PreludeWelcomeToHemlockVale
    EpilogueStep -> Nothing
    UpgradeDeckStep nextStep' -> Just nextStep'
    _ -> Nothing

instance RunMessage TheFeastOfHemlockVale where
  runMessage msg c@(TheFeastOfHemlockVale _attrs) = runQueueT $ campaignI18n $ case msg of
    CampaignStep PrologueStep -> scope "prologue" do
      flavor $ setTitle "title" >> p "body"
      nextCampaignStep
      pure c
    _ -> lift $ defaultCampaignRunner msg c
