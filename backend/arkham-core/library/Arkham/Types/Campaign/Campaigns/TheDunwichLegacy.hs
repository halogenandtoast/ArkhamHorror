{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Campaign.Campaigns.TheDunwichLegacy where

import Arkham.Import

import Arkham.Types.Campaign.Attrs
import Arkham.Types.Campaign.Runner
import Arkham.Types.CampaignLogKey
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
    CampaignStep (Just (InterludeStep 1)) -> do
      unconsciousForSeveralHours <- asks
        $ hasRecord InvestigatorsWereUnconsciousForSeveralHours
      investigatorIds <- getSetList ()
      leadInvestigatorId <- getLeadInvestigatorId
      if unconsciousForSeveralHours
        then c <$ unshiftMessages
          ([ AskMap
           . mapFromList
           $ [ ( iid
               , ChooseOne
                 [ Run
                     [ Continue "Continue"
                     , FlavorText
                       (Just "Interlude 1: Armitage's Fate")
                       [ "You are more than a little rattled by your experiences\
                        \ in the university and the Clover Club. You’re not sure what to make of\
                        \ whoever—or whatever—was after Rice and Morgan. Worried about Dr.\
                        \   Armitage, you swiftly make your way back to his home. When you arrive,\
                        \ you find that the latches of his front door have been busted open, and his\
                        \ living area and study have been ransacked. Dr. Armitage is nowhere to be\
                        \ found. Searching his home, you find a journal the intruders didn’t steal\
                        \ tucked beneath several other documents in the bottom drawer of Armitage’s\
                        \ desk. The journal appears to be written in a strange language you cannot\
                        \ decode, using a script you’ve never seen in your entire life. Fortunately, it\
                        \ seems Dr. Armitage had already gone through the trouble of translating it\
                        \ into English. Apparently, it belongs to one “Wilbur Whateley.”"
                       , "The journal—along with Armitage’s many notes—tells a startling\
                        \ tale, one you would scarcely believe had it not been for your harrowing\
                        \ experiences earlier tonight…"
                       ]
                     ]
                 ]
               )
             | iid <- investigatorIds
             ]
           , Record DrHenryArmitageWasKidnapped
           ]
          <> [ GainXP iid 2 | iid <- investigatorIds ]
          <> [NextCampaignStep Nothing]
          )
        else c <$ unshiftMessages
          [ AskMap
          . mapFromList
          $ [ ( iid
              , ChooseOne
                [ Run
                    [ Continue "Continue"
                    , FlavorText
                      (Just "Interlude 1: Armitage's Fate")
                      [ "When you arrive at Dr. Armitage’s home in\
                        \ Southside, you find him sitting at his desk, pale-faced and sweating with\
                        \ worry. He is grateful to you for searching for his colleagues, but he doesn’t\
                        \ look relieved. With a long pause, he straightens his glasses and explains:"
                      , "“I’m afraid I must apologize. There’s something I didn’t mention to you\
                        \ earlier.” Dr. Armitage then spins a tale you would scarcely believe had it\
                        \ not been for your harrowing experiences earlier that night…"
                      ]
                    ]
                ]
              )
            | iid <- investigatorIds
            ]
          , chooseOne
            leadInvestigatorId
            [ Label
              "Add Dr. Henry Armitage to a deck"
              [ chooseOne
                  leadInvestigatorId
                  [ TargetLabel
                      (InvestigatorTarget iid)
                      [AddCampaignCardToDeck iid "02040"]
                  | iid <- investigatorIds
                  ]
              ]
            , Label "Do not add Dr. Henry Armitage to any deck" []
            ]
          , NextCampaignStep Nothing
          ]
    NextCampaignStep mNextCampaignStep -> do
      let
        nextStep = case mNextCampaignStep of
          Just nextCampaignStep -> Just nextCampaignStep
          Nothing -> case campaignStep of
            Just PrologueStep -> error "must be handled"
            Just (ScenarioStep "02041") ->
              if ScenarioStep "02062" `elem` campaignCompletedSteps
                then Just $ InterludeStep 1
                else Just $ ScenarioStep "02062"
            Just (ScenarioStep "02062") ->
              if ScenarioStep "02041" `elem` campaignCompletedSteps
                then Just $ InterludeStep 1
                else Just $ ScenarioStep "02041"
            Just (InterludeStep 1) -> Just (ScenarioStep "02118")
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
