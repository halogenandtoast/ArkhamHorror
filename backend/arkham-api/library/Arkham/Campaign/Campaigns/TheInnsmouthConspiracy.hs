module Arkham.Campaign.Campaigns.TheInnsmouthConspiracy (
  TheInnsmouthConspiracy (..),
  theInnsmouthConspiracy,
) where

import Arkham.Prelude

import Arkham.Campaign.Runner
import Arkham.CampaignStep
import Arkham.Campaigns.TheInnsmouthConspiracy.Import
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Id

newtype TheInnsmouthConspiracy = TheInnsmouthConspiracy CampaignAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

theInnsmouthConspiracy :: Difficulty -> TheInnsmouthConspiracy
theInnsmouthConspiracy difficulty =
  campaign
    TheInnsmouthConspiracy
    (CampaignId "07")
    "The Innsmouth Conspiracy"
    difficulty
    (chaosBagContents difficulty)
 where

instance IsCampaign TheInnsmouthConspiracy where
  nextStep a = case campaignStep (toAttrs a) of
    PrologueStep -> Just ThePitOfDespair
    EpilogueStep -> Nothing
    UpgradeDeckStep nextStep' -> Just nextStep'
    _ -> Nothing

instance RunMessage TheInnsmouthConspiracy where
  runMessage msg c@(TheInnsmouthConspiracy _attrs) = case msg of
    CampaignStep PrologueStep -> do
      push $ NextCampaignStep Nothing
      pure c
    _ -> defaultCampaignRunner msg c
