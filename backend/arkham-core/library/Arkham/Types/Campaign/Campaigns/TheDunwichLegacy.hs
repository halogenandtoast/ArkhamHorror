{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Campaign.Campaigns.TheDunwichLegacy where

import Arkham.Import

import Arkham.Types.Campaign.Attrs
import Arkham.Types.Campaign.Runner
import Arkham.Types.CampaignStep
import Arkham.Types.Difficulty
import Arkham.Types.Game.Helpers
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
      , Token.AutoFail
      , Token.ElderSign
      ]

instance CampaignRunner env => RunMessage env TheDunwichLegacy where
  runMessage msg c@(TheDunwichLegacy attrs@Attrs {..}) = case msg of
    CampaignStep (Just PrologueStep) -> do
      investigatorIds <- getSetList ()
      leadInvestigatorId <- getLeadInvestigatorId
      c <$ unshiftMessages
        [ AskMap
        . mapFromList
        $ [ ( iid
            , ChooseOne
              [ Run
                  [ Continue "Continue"
                  , FlavorText
                    (Just "Prologue")
                    [ "Dr. Henry Armitage pours himself a glass of pinot and sits down at his\
                      \ desk, gesturing for you to sit across from him. “I apologize for the short\
                      \ notice,” he begins. His face is pale, his forehead sweaty and wrinkled\
                      \ with worry."
                    , "Armitage—the head librarian of Miskatonic University, and a former\
                      \ mentor of yours—privately contacted you in the hopes of gaining your\
                      \ assistance. Eager to help, you made your way to his home in Southside.\
                      \ Upon entering, you were surprised to find his home in disarray. Books\
                      \ and notes litter his desk, and an empty bottle of wine has tipped over\
                      \ onto the ground by the fireplace. You’d always known Armitage to be\
                      \ neat and well-organized."
                    , "The elderly man takes a moment to collect his thoughts. “I am\
                      \ looking for two of my colleagues—Dr. Francis Morgan, professor of\
                      \ archaeology, and Warren Rice, professor of languages. Warren was\
                      \ supposed to meet up with me over supper earlier today to discuss several\
                      \ important findings, but he has since gone missing. At first I thought\
                      \ nothing of it, but I have a nagging feeling something else is going on. A\
                      \ very…familiar feeling.” You’ve never seen Armitage quite this worried\
                      \ before. His hands tremble as he reaches for the glass on his desk, and\
                      \ he sips from it nervously. “I tried to find Francis, hoping he knew where\
                      \ Warren was, but he too is out of touch. Francis has been spending a lot\
                      \ of time in some gambling den, or so I am told."
                    , "“I sent for you because I am worried Warren might be in trouble. I\
                      \ would appreciate it greatly if you could find him for me. You may also\
                      \ wish to ask Francis for help, if you can reach him.”"
                    ]
                  ]
              ]
            )
          | iid <- investigatorIds
          ]
        , chooseOne
          leadInvestigatorId
          [ Label
            "Professor Warren Rice was last seen working late at night in the humanities department of Miskatonic University. Let’s search for him there. Proceed with “Scenario I–A: Extracurricular Activity” if you wish to find Professor Warren Rice first."
            [NextCampaignStep (Just $ ScenarioStep "02041")]
          , Label
            "Dr. Francis Morgan was last seen gambling at the Clover Club, an upscale speakeasy and gambling joint located downtown.  Let’s go talk to him.  Proceed with “Scenario I–B: The House Always Wins” if you wish to find Dr. Francis Morgan first."
            [NextCampaignStep (Just $ ScenarioStep "02062")]
          ]
        ]
    NextCampaignStep mNextCampaignStep -> do
      let
        nextStep = case mNextCampaignStep of
          Just nextCampaignStep -> Just nextCampaignStep
          Nothing -> case campaignStep of
            Just PrologueStep -> error "must be handled"
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
      unshiftMessages [CampaignStep nextStep]
      pure
        . TheDunwichLegacy
        $ attrs
        & step
        .~ nextStep
        & completedSteps
        %~ completeStep campaignStep
    _ -> TheDunwichLegacy <$> runMessage msg attrs
