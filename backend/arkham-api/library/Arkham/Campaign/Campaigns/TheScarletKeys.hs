module Arkham.Campaign.Campaigns.TheScarletKeys (theScarletKeys) where

import Arkham.Campaign.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.CampaignSteps
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.ChaosToken
import Arkham.Helpers.FlavorText

newtype TheScarletKeys = TheScarletKeys CampaignAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

theScarletKeys :: Difficulty -> TheScarletKeys
theScarletKeys = campaign TheScarletKeys (CampaignId "09") "The Scarlet Keys"

{- FOURMOLU_DISABLE -}
campaignChaosBag :: Difficulty -> [ChaosTokenFace]
campaignChaosBag = \case
  Easy ->
    [ #"+1", #"+1", #"0", #"0", #"0", #"-1", #"-1", #"-1", #"-2", #"-2"
    , Skull, Skull, Tablet, ElderThing, AutoFail, ElderSign
    ]
  Standard ->
    [ #"+1", #"0", #"0", #"-1", #"-1", #"-1", #"-2", #"-2", #"-3", #"-4"
    , Skull, Skull, Tablet, ElderThing, AutoFail, ElderSign
    ]
  Hard ->
    [ #"0", #"0", #"0", #"-1", #"-1", #"-2", #"-2", #"-3", #"-3", #"-4", #"-5"
    , Skull, Skull, Tablet, ElderThing, AutoFail, ElderSign
    ]
  Expert ->
    [ #"0", #"-1", #"-1", #"-2", #"-2", #"-3", #"-3", #"-4", #"-4", #"-5", #"-6", #"-8"
    , Skull, Skull, Tablet, ElderThing, AutoFail, ElderSign
    ]
{- FOURMOLU_ENABLE -}

instance IsCampaign TheScarletKeys where
  campaignTokens = campaignChaosBag
  invalidCards _ = ["02310"] -- The Red-Gloved Man can not be included
  nextStep a = case campaignStep (toAttrs a) of
    PrologueStep -> Just RiddlesAndRain
    RiddlesAndRain -> Nothing
    EpilogueStep -> Nothing
    UpgradeDeckStep nextStep' -> Just nextStep'
    _ -> Nothing

instance RunMessage TheScarletKeys where
  runMessage msg c@(TheScarletKeys _attrs) = runQueueT $ campaignI18n $ case msg of
    CampaignStep PrologueStep -> scope "prologue" do
      flavor $ setTitle "title" >> p "body"
      nextCampaignStep
      pure c
    _ -> lift $ defaultCampaignRunner msg c
