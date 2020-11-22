{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Scenario.Scenarios.ExtracurricularActivity where

import Arkham.Import hiding (Cultist)

import Arkham.Types.CampaignLogKey
import Arkham.Types.Difficulty
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Scenario.Runner
import Arkham.Types.Token

newtype ExtracurricularActivity = ExtracurricularActivity Attrs
  deriving newtype (Show, ToJSON, FromJSON)

extracurricularActivity :: Difficulty -> ExtracurricularActivity
extracurricularActivity difficulty =
  ExtracurricularActivity $ (baseAttrs
                              "02041"
                              "Extracurricular Activity"
                              ["02042", "02043", "02044"]
                              ["02045", "02046", "02047"]
                              difficulty
                            )
    { scenarioLocationLayout = Just
      [ "orneLibrary        miskatonicQuad scienceBuilding"
      , "humanitiesBuilding studentUnion   administrationBuilding"
      ]
    }

extracurricularActivityIntro :: Message
extracurricularActivityIntro = FlavorText
  (Just "Scenario I-A: Extracurricular Activity")
  [ "Dr. Armitage is worried his colleague, Professor Warren Rice, might be\
    \ in trouble, so he has asked for your help in finding his friend. He seems\
    \ unreasonably nervous about his colleague’s disappearance considering\
    \ Professor Rice has only been “missing” for a matter of hours…"
  ]

instance (HasTokenValue env InvestigatorId, HasCount DiscardCount env InvestigatorId) => HasTokenValue env ExtracurricularActivity where
  getTokenValue (ExtracurricularActivity attrs) iid = \case
    Skull -> pure $ TokenValue
      Skull
      (NegativeModifier $ if isEasyStandard attrs then 1 else 2)
    Cultist -> do
      discardCount <- unDiscardCount <$> getCount iid
      pure $ TokenValue
        Cultist
        (NegativeModifier $ if discardCount >= 10
          then (if isEasyStandard attrs then 3 else 5)
          else 1
        )
    ElderThing -> do
      pure $ TokenValue Tablet (NegativeModifier 0) -- determined by an effect
    otherFace -> getTokenValue attrs iid otherFace

instance ScenarioRunner env => RunMessage env ExtracurricularActivity where
  runMessage msg s@(ExtracurricularActivity attrs@Attrs {..}) = case msg of
    Setup -> do
      investigatorIds <- getInvestigatorIds
      completedTheHouseAlwaysWins <-
        elem "02062" . map unCompletedScenarioId <$> getSetList ()
      encounterDeck <- buildEncounterDeck
        [ EncounterSet.ExtracurricularActivity
        , EncounterSet.Sorcery
        , EncounterSet.TheBeyond
        , EncounterSet.BishopsThralls
        , EncounterSet.Whippoorwills
        , EncounterSet.AncientEvils
        , EncounterSet.LockedDoors
        , EncounterSet.AgentsOfYogSothoth
        ]
      pushMessages
        [ SetEncounterDeck encounterDeck
        , AddAgenda "02042"
        , AddAct "02045"
        , PlaceLocation "02048"
        , PlaceLocation "02050"
        , PlaceLocation "02049"
        , PlaceLocation "02051"
        , PlaceLocation "02056"
        , PlaceLocation "02053"
        , RevealLocation Nothing "02048"
        , MoveAllTo "02048"
        , AskMap
        . mapFromList
        $ [ ( iid
            , ChooseOne
              [Run [Continue "Continue", extracurricularActivityIntro]]
            )
          | iid <- investigatorIds
          ]
        ]
      let
        locations' = mapFromList
          [ ("Miskatonic Quad", ["02048"])
          , ("Humanities Building", ["02049"])
          , ("Orne Library", ["02050"])
          , ("Student Union", ["02051"])
          , ("Dormitories", ["02052"])
          , ("Administration Building", ["02053"])
          , ( "Faculty Offices"
            , [if completedTheHouseAlwaysWins then "02055" else "02054"]
            )
          , ("Science Building", ["02056"])
          , ("Alchemy Labs", ["02057"])
          ]
      ExtracurricularActivity
        <$> runMessage msg (attrs & locations .~ locations')
    FailedSkillTest iid _ _ target@(DrawnTokenTarget token) _ -> do
      s <$ case drawnTokenFace token of
        ElderThing -> unshiftMessage $ DiscardTopOfDeck
          iid
          (if isEasyStandard attrs then 3 else 5)
          (Just target)
        _ -> pure ()
    DiscardedTopOfDeck _iid cards target@(DrawnTokenTarget token) -> do
      s <$ case drawnTokenFace token of
        ElderThing -> do
          let n = sum $ map (toPrintedCost . pcCost) cards
          unshiftMessage $ CreateTokenValueEffect (-n) (toSource attrs) target
        _ -> pure ()
    NoResolution -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      xp <- getXp
      s <$ unshiftMessage
        (chooseOne
          leadInvestigatorId
          [ Run
            $ [ Continue "Continue"
              , FlavorText
                Nothing
                [ "You barely manage to escape\
                  \ your house with your lives. The woman from your parlor\
                  \ follows you out the front door, slamming it behind her. “You\
                  \ fools! See what you have done?” She pushes a chair in front of\
                  \ the door, lodging it beneath the doorknob. “We must get out\
                  \ of here. Come with me, and I will tell you what I know. We\
                  \ are the only ones who can stop the threat that lurks beneath\
                  \ from being unleashed throughout the city.” You’re in no state\
                  \ to argue. Nodding, you follow the woman as she runs from\
                  \ your front porch out into the rainy street, toward Rivertown."
                ]
              , Record YourHouseIsStillStanding
              , Record GhoulPriestIsStillAlive
              , chooseOne
                leadInvestigatorId
                [ Label
                  "Add Lita Chantler to your deck"
                  [AddCampaignCardToDeck leadInvestigatorId "01117"]
                , Label "Do not add Lita Chantler to your deck" []
                ]
              ]
            <> [ GainXP iid (xp + 2) | iid <- investigatorIds ]
            <> [EndOfGame]
          ]
        )
    Resolution 1 -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      xp <- getXp
      s <$ unshiftMessage
        (chooseOne
          leadInvestigatorId
          [ Run
            $ [ Continue "Continue"
              , FlavorText
                Nothing
                [ "You nod and allow the red-haired woman to\
                  \ set the walls and floor of your house ablaze. The fire spreads\
                  \ quickly, and you run out the front door to avoid being caught\
                  \ in the inferno. From the sidewalk, you watch as everything\
                  \ you own is consumed by the flames. “Come with me,” the\
                  \ woman says. “You must be told of the threat that lurks below.\
                  \ Alone, we are surely doomed…but together, we can stop it.”"
                ]
              , Record YourHouseHasBurnedToTheGround
              , chooseOne
                leadInvestigatorId
                [ Label
                  "Add Lita Chantler to your deck"
                  [AddCampaignCardToDeck leadInvestigatorId "01117"]
                , Label "Do not add Lita Chantler to your deck" []
                ]
              , SufferTrauma leadInvestigatorId 0 1
              ]
            <> [ GainXP iid (xp + 2) | iid <- investigatorIds ]
            <> [EndOfGame]
          ]
        )
    Resolution 2 -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      xp <- getXp
      s <$ unshiftMessage
        (chooseOne
          leadInvestigatorId
          [ Run
            $ [ Continue "Continue"
              , FlavorText
                Nothing
                [ "You refuse to follow the overzealous woman’s\
                  \ order and kick her out of your home for fear that she will set\
                  \ it ablaze without your permission. “Fools! You are making\
                  \ a grave mistake!” she warns. “You do not understand the\
                  \ threat that lurks below…the grave danger we are all in!”\
                  \ Still shaken by the night’s events, you decide to hear the\
                  \ woman out. Perhaps she can shed some light on these bizarre\
                  \ events…but she doesn’t seem to trust you very much."
                ]
              , Record YourHouseIsStillStanding
              , GainXP leadInvestigatorId 1
              ]
            <> [ GainXP iid (xp + 2) | iid <- investigatorIds ]
            <> [EndOfGame]
          ]
        )
    Resolution 3 -> do
      leadInvestigatorId <- getLeadInvestigatorId
      s <$ unshiftMessage
        (chooseOne
          leadInvestigatorId
          [ Run
              [ Continue "Continue"
              , FlavorText
                Nothing
                [ "You run to the hallway to try to find a way to\
                  \ escape the house, but the burning-hot barrier still blocks your\
                  \ path. Trapped, the horde of feral creatures that have invaded\
                  \ your home close in, and you have nowhere to run."
                ]
              , Record LitaWasForcedToFindOthersToHelpHerCause
              , Record YourHouseIsStillStanding
              , Record GhoulPriestIsStillAlive
              , chooseOne
                leadInvestigatorId
                [ Label
                  "Add Lita Chantler to your deck"
                  [AddCampaignCardToDeck leadInvestigatorId "01117"]
                , Label "Do not add Lita Chantler to your deck" []
                ]
              , EndOfGame
              ]
          ]
        )
    _ -> ExtracurricularActivity <$> runMessage msg attrs
