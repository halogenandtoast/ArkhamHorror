{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Campaign.Campaigns.TheDunwichLegacy where

import Arkham.Import

import Arkham.Types.Campaign.Attrs
import Arkham.Types.Campaign.Runner
import Arkham.Types.CampaignStep
import Arkham.Types.Difficulty
import qualified Arkham.Types.Token as Token

newtype TheDunwichLegacy = TheDunwichLegacy Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theDunwichLegacy :: Difficulty -> TheDunwichLegacy
theDunwichLegacy difficulty = TheDunwichLegacy
  (baseAttrs (CampaignId "02") "The Dunwich Legacy" difficulty chaosBagContents)
 where
  chaosBagContents = case difficulty of
    Easy ->
      [ Token.PlusOne
      , Token.PlusOne
      , Token.Zero
      , Token.Zero
      , Token.Zero
      , Token.MinusOne
      , Token.MinusOne
      , Token.MinusOne
      , Token.MinusTwo
      , Token.MinusTwo
      , Token.Skull
      , Token.Skull
      , Token.Cultist
      , Token.Tablet
      , Token.AutoFail
      , Token.ElderSign
      ]
    Standard ->
      [ Token.PlusOne
      , Token.Zero
      , Token.Zero
      , Token.MinusOne
      , Token.MinusOne
      , Token.MinusOne
      , Token.MinusTwo
      , Token.MinusTwo
      , Token.MinusThree
      , Token.MinusFour
      , Token.Skull
      , Token.Skull
      , Token.Cultist
      , Token.Tablet
      , Token.AutoFail
      , Token.ElderSign
      ]
    Hard ->
      [ Token.Zero
      , Token.Zero
      , Token.Zero
      , Token.MinusOne
      , Token.MinusOne
      , Token.MinusTwo
      , Token.MinusTwo
      , Token.MinusThree
      , Token.MinusThree
      , Token.MinusFour
      , Token.MinusFive
      , Token.Skull
      , Token.Skull
      , Token.Cultist
      , Token.Tablet
      , Token.AutoFail
      , Token.ElderSign
      ]
    Expert ->
      [ Token.Zero
      , Token.MinusOne
      , Token.MinusOne
      , Token.MinusTwo
      , Token.MinusTwo
      , Token.MinusThree
      , Token.MinusThree
      , Token.MinusFour
      , Token.MinusFour
      , Token.MinusFive
      , Token.MinusSix
      , Token.MinusEight
      , Token.Skull
      , Token.Skull
      , Token.Cultist
      , Token.Tablet
      , Token.AutoFail
      , Token.ElderSign
      ]

instance (CampaignRunner env) => RunMessage env TheDunwichLegacy where
  runMessage msg (TheDunwichLegacy attrs@Attrs {..}) = case msg of
    NextCampaignStep -> do
      let
        nextStep = case campaignStep of
          Just PrologueStep -> Just (ScenarioStep "02041")
          Just (ScenarioStep "02041") ->
            Just
              . ScenarioStep
              $ if ScenarioStep "02062" `elem` campaignCompletedSteps
                  then "02118"
                  else "02062"
          Just (ScenarioStep "02062") ->
            Just
              . ScenarioStep
              $ if ScenarioStep "02041" `elem` campaignCompletedSteps
                  then "02118"
                  else "02041"
          Just (ScenarioStep "02118") -> Just (ScenarioStep "02159")
          Just (ScenarioStep "02159") -> Just (ScenarioStep "02195")
          Just (ScenarioStep "02195") -> Just (ScenarioStep "02236")
          Just (ScenarioStep "02236") -> Just (ScenarioStep "02274")
          Just (ScenarioStep "02274") -> Just (ScenarioStep "02311")
          Just (ScenarioStep "02311") -> Nothing
          _ -> Nothing
      unshiftMessage (CampaignStep nextStep)
      pure
        . TheDunwichLegacy
        $ attrs
        & step
        .~ nextStep
        & completedSteps
        %~ completeStep campaignStep
    _ -> TheDunwichLegacy <$> runMessage msg attrs
