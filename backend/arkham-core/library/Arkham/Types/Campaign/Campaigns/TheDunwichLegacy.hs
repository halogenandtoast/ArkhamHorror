module Arkham.Types.Campaign.Campaigns.TheDunwichLegacy where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window


import Arkham.Types.Campaign.Attrs
import Arkham.Types.Campaign.Runner
import Arkham.Types.CampaignLogKey
import Arkham.Types.CampaignStep
import Arkham.Types.Difficulty
import Arkham.Types.Game.Helpers
import qualified Arkham.Types.Token as Token

newtype TheDunwichLegacy = TheDunwichLegacy CampaignAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

findOwner
  :: (MonadReader env m, HasList CampaignStoryCard env ())
  => CardCode
  -> m (Maybe InvestigatorId)
findOwner cardCode = do
  campaignStoryCards <- getList ()
  pure
    $ campaignStoryCardInvestigatorId
    <$> find
          ((== cardCode) . getCardCode . campaignStoryCardPlayerCard)
          campaignStoryCards

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
  runMessage msg c@(TheDunwichLegacy attrs@CampaignAttrs {..}) = case msg of
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
                       (Just "Interlude I: Armitage's Fate")
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
          , addCampaignCardToDeckChoice
            leadInvestigatorId
            investigatorIds
            "02040"
          , NextCampaignStep Nothing
          ]
    CampaignStep (Just (InterludeStep 2)) -> do
      sacrificedToYogSothoth <- asks $ hasRecordSet SacrificedToYogSothoth
      investigatorIds <- getSetList ()
      leadInvestigatorId <- getLeadInvestigatorId
      drHenryArmitageUnowned <- isNothing <$> findOwner "02040"
      professorWarrenRiceUnowned <- isNothing <$> findOwner "02061"
      drFrancisMorganUnowned <- isNothing <$> findOwner "02080"
      let
        addPowderOfIbnGhazi =
          addCampaignCardToDeckChoice leadInvestigatorId investigatorIds "02219"
            <$ guard
                 (any
                   (`notElem` sacrificedToYogSothoth)
                   ["02040", "02061", "02080"]
                 )
        addDrHenryArmitage =
          addCampaignCardToDeckChoice leadInvestigatorId investigatorIds "02040"
            <$ guard
                 (drHenryArmitageUnowned
                 && "02040"
                 `notElem` sacrificedToYogSothoth
                 )
        addProfessorWarrenRice =
          addCampaignCardToDeckChoice leadInvestigatorId investigatorIds "02061"
            <$ guard
                 (professorWarrenRiceUnowned
                 && "02061"
                 `notElem` sacrificedToYogSothoth
                 )
        addDrFrancisMorgan =
          addCampaignCardToDeckChoice leadInvestigatorId investigatorIds "02080"
            <$ guard
                 (drFrancisMorganUnowned
                 && "02080"
                 `notElem` sacrificedToYogSothoth
                 )
        addZebulonWhateley =
          addCampaignCardToDeckChoice leadInvestigatorId investigatorIds "02217"
            <$ guard ("02217" `notElem` sacrificedToYogSothoth)
        addEarlSawyer =
          addCampaignCardToDeckChoice leadInvestigatorId investigatorIds "02218"
            <$ guard ("02218" `notElem` sacrificedToYogSothoth)
      c <$ unshiftMessages
        ([ AskMap
           . mapFromList
           $ [ ( iid
               , ChooseOne
                 [ Run
                     [ Continue "Continue"
                     , FlavorText
                       (Just "Interlude II: The Survivors")
                       [ "Inside the chamber that contained the terrible beast, you find the missing\
                          \ townsfolk and the others from Arkham; they are bound and shackled. You\
                          \ also find several documents that suggest the creature you found isn’t the\
                          \ only one of its kind in Dunwich. You free the creature’s victims from their\
                          \ bonds, and they offer you their thanks. You begin to plan your next move."
                       ]
                     ]
                 ]
               )
             | iid <- investigatorIds
             ]
         ]
        <> [ AskMap
             . mapFromList
             $ [ ( iid
                 , ChooseOne
                   [ Run
                       [ Continue "Continue"
                       , FlavorText
                         (Just "Interlude II: The Survivors")
                         [ "“It is far worse than we had thought,” Dr. Armitage\
                        \ says, pale and trembling. “Wilbur Whateley was only the beginning.\
                        \ There were more, many more in Dunwich, who knew of the ‘Great Old\
                        \ Ones’ and who desired power and knowledge above all else, the Earth\
                        \ be damned. I knew I should have burned that wretch’s journal. But\
                        \ thanks to its contents, I know how we can stop them. We are the only\
                        \ ones who can! Now quickly, help me with this solution—the powder is\
                        \ the key, yes, the powder is the only way…”"
                         ]
                       ]
                   ]
                 )
               | iid <- investigatorIds
               ]
           | "02040" `notElem` sacrificedToYogSothoth
           ]
        <> addDrHenryArmitage
        <> [ AskMap
             . mapFromList
             $ [ ( iid
                 , ChooseOne
                   [ Run
                       [ Continue "Continue"
                       , FlavorText
                         (Just "Interlude II: The Survivors")
                         [ "Professor Rice adjusts his glasses and studies the\
                          \ documents and arcane manuscripts left in the chamber. “I thought\
                          \ this nightmare was over and done with,” he sighs. “But we have a duty\
                          \ to see this through. We have to stop these creatures, or it won’t be just\
                          \ Dunwich in trouble. The powder mixture Armitage created to see the\
                          \ creatures will be our saving grace,” he explains, and sets off to the task\
                          \ of recreating the powder."
                         ]
                       ]
                   ]
                 )
               | iid <- investigatorIds
               ]
           | "02061" `notElem` sacrificedToYogSothoth
           ]
        <> addProfessorWarrenRice
        <> [ AskMap
             . mapFromList
             $ [ ( iid
                 , ChooseOne
                   [ Run
                       [ Continue "Continue"
                       , FlavorText
                         (Just "Interlude II: The Survivors")
                         [ "“Thank you for everything you’ve done,” Dr. Morgan\
                          \ says, taking count of your provisions and ammunition. “Last time, we\
                          \ needed some of that strange powder Armitage concocted to even see the\
                          \ beast that terrorized Dunwich. If there’s more of those things out there,\
                          \ we’re going to need that powder. I think I remember how he made it…”"
                         ]
                       ]
                   ]
                 )
               | iid <- investigatorIds
               ]
           | "02080" `notElem` sacrificedToYogSothoth
           ]
        <> addDrFrancisMorgan
        <> [ AskMap
             . mapFromList
             $ [ ( iid
                 , ChooseOne
                   [ Run
                       [ Continue "Continue"
                       , FlavorText
                         (Just "Interlude II: The Survivors")
                         [ "“Dunwich’s had its fair share of oddities,” Zebulon\
                           \ explains to you with a quavering voice, “but I ain’t ever seen anything\
                           \ as sick and twisted as this…this…thing.” He gives the creature’s\
                           \ remains one last sickened glance before closing the door to the chamber\
                           \ behind him, shuddering. He locks eyes with you, his expression grim.\
                           \ “Whoever dun this gotta pay. I’ll do all I can to help.”"
                         ]
                       ]
                   ]
                 )
               | iid <- investigatorIds
               ]
           | "02217" `notElem` sacrificedToYogSothoth
           ]
        <> addZebulonWhateley
        <> [ AskMap
             . mapFromList
             $ [ ( iid
                 , ChooseOne
                   [ Run
                       [ Continue "Continue"
                       , FlavorText
                         (Just "Interlude II: The Survivors")
                         [ "“I never could’a made it if it weren’t for you,” Earl says with a stammer,\
                           \ shaking your hand repeatedly. “If ’n there’s anything I can do to repay\
                           \ yeh, just ask away. I ain’t much of a fighter or anythin’, but I’ll do all I\
                           \ can. Jus’…don’t make me look at anythin’ like that beast again, a’right?”"
                         ]
                       ]
                   ]
                 )
               | iid <- investigatorIds
               ]
           | "02218" `notElem` sacrificedToYogSothoth
           ]
        <> addEarlSawyer
        <> addPowderOfIbnGhazi
        <> [NextCampaignStep Nothing]
        )
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
            Just (ScenarioStep "02195") ->
              case lookup "02195" campaignResolutions of
                Just NoResolution -> Just (ScenarioStep "02236")
                _ -> Just $ InterludeStep 2
            Just (InterludeStep 2) -> Just (ScenarioStep "02236")
            Just (ScenarioStep "02236") -> Just (ScenarioStep "02274")
            Just (ScenarioStep "02274") -> Just (ScenarioStep "02311")
            Just (ScenarioStep "02311") -> Nothing
            _ -> Nothing
      unshiftMessages [CampaignStep nextStep]
      pure
        . TheDunwichLegacy
        $ attrs
        & (stepL .~ nextStep)
        & (completedStepsL %~ completeStep campaignStep)
    _ -> TheDunwichLegacy <$> runMessage msg attrs
