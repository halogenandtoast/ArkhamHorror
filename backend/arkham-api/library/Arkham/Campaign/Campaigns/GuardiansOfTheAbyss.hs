module Arkham.Campaign.Campaigns.GuardiansOfTheAbyss (guardiansOfTheAbyss) where

import Arkham.Campaign.Import.Lifted
import Arkham.ChaosToken

newtype GuardiansOfTheAbyss = GuardiansOfTheAbyss CampaignAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

{- FOURMOLU_DISABLE -}
chaosBagContents :: Difficulty -> [ChaosTokenFace]
chaosBagContents difficulty
  | difficulty `elem` [Easy, Standard] =
      [ PlusOne , PlusOne , Zero , Zero , MinusOne , MinusOne , MinusOne , MinusTwo , MinusTwo
      , MinusThree , MinusThree , MinusFour , MinusSix , Skull , Skull , Skull , Cultist , Tablet
      , ElderThing , AutoFail , ElderSign
      ]
  | otherwise =
      [ PlusOne , Zero , Zero , MinusOne , MinusOne , MinusOne , MinusTwo , MinusTwo , MinusTwo
      , MinusThree , MinusThree , MinusFour , MinusFour , MinusFive , MinusSeven , Skull , Skull
      , Skull , Cultist , Tablet , ElderThing , AutoFail , ElderSign
      ]
{- FOURMOLU_ENABLE -}

instance IsCampaign GuardiansOfTheAbyss where
  campaignTokens = chaosBagContents
  nextStep a = case (toAttrs a).normalizedStep of
    PrologueStep -> continue (ScenarioStep "83001")
    ScenarioStep "83001" -> continue (ScenarioStep "83016")
    ScenarioStep "83016" -> Nothing
    other -> defaultNextStep other

guardiansOfTheAbyss :: Difficulty -> GuardiansOfTheAbyss
guardiansOfTheAbyss = campaign GuardiansOfTheAbyss "83" "Guardians of the Abyss"

instance RunMessage GuardiansOfTheAbyss where
  runMessage msg c = runQueueT $ case msg of
    CampaignStep PrologueStep -> do
      nextCampaignStep
      pure c
    _ -> lift $ defaultCampaignRunner msg c
