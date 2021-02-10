module Arkham.Types.Scenario.Scenarios.ExtracurricularActivity where

import Arkham.Prelude

import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Classes
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.CampaignLogKey
import Arkham.Types.Difficulty
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Scenario.Runner

newtype ExtracurricularActivity = ExtracurricularActivity ScenarioAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

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
      [ "orneLibrary        miskatonicQuad scienceBuilding alchemyLabs"
      , "humanitiesBuilding studentUnion   administrationBuilding ."
      , ".                  dormitories    facultyOffices         ."
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
    Skull -> pure $ toTokenValue attrs Skull 1 2
    Cultist -> do
      discardCount <- unDiscardCount <$> getCount iid
      pure $ TokenValue
        Cultist
        (NegativeModifier $ if discardCount >= 10
          then (if isEasyStandard attrs then 3 else 5)
          else 1
        )
    ElderThing -> pure $ TokenValue Tablet (NegativeModifier 0) -- determined by an effect
    otherFace -> getTokenValue attrs iid otherFace

instance ScenarioRunner env => RunMessage env ExtracurricularActivity where
  runMessage msg s@(ExtracurricularActivity attrs@ScenarioAttrs {..}) =
    case msg of
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
          locations' = mapFromList $ map
            (second pure . toFst (getLocationName . lookupLocation))
            [ "02048"
            , "02049"
            , "02050"
            , "02051"
            , "02052"
            , "02053"
            , if completedTheHouseAlwaysWins then "02055" else "02054"
            , "02056"
            , "02057"
            ]
        ExtracurricularActivity
          <$> runMessage msg (attrs & locationsL .~ locations')
      ResolveToken drawnToken ElderThing iid -> s <$ unshiftMessage
        (DiscardTopOfDeck
          iid
          (if isEasyStandard attrs then 2 else 3)
          (Just $ DrawnTokenTarget drawnToken)
        )
      FailedSkillTest iid _ _ (DrawnTokenTarget token) _ _ ->
        s <$ case drawnTokenFace token of
          Skull -> unshiftMessage $ DiscardTopOfDeck
            iid
            (if isEasyStandard attrs then 3 else 5)
            Nothing
          _ -> pure ()
      DiscardedTopOfDeck _iid cards target@(DrawnTokenTarget token) ->
        s <$ case drawnTokenFace token of
          ElderThing -> do
            let n = sum $ map (toPrintedCost . pcCost) cards
            unshiftMessage $ CreateTokenValueEffect (-n) (toSource attrs) target
          _ -> pure ()
      ScenarioResolution NoResolution -> do
        leadInvestigatorId <- getLeadInvestigatorId
        investigatorIds <- getInvestigatorIds
        xp <- getXp
        s <$ unshiftMessages
          ([ chooseOne
             leadInvestigatorId
             [ Run
                 [ Continue "Continue"
                 , FlavorText
                   Nothing
                   [ "As you flee from the university,\
                  \ you hear screaming from the northern end of the campus. An\
                  \ ambulance passes you by, and you fear the worst. Hours later,\
                  \ you learn that a ‘rabid dog of some sort’ found its way into\
                  \ the university dormitories. The creature attacked the students\
                  \ inside and many were mauled or killed in the attack."
                   ]
                 ]
             ]
           , Record ProfessorWarrenRiceWasKidnapped
           , Record TheInvestigatorsFailedToSaveTheStudents
           , AddToken Tablet
           ]
          <> [ GainXP iid (xp + 1) | iid <- investigatorIds ]
          <> [EndOfGame]
          )
      ScenarioResolution (Resolution 1) -> do
        leadInvestigatorId <- getLeadInvestigatorId
        investigatorIds <- getInvestigatorIds
        xp <- getXp
        s <$ unshiftMessages
          ([ chooseOne
             leadInvestigatorId
             [ Run
                 [ Continue "Continue"
                 , FlavorText
                   Nothing
                   [ "You find Professor Rice bound and gagged\
                  \ in the closet of his office. When you free him, he informs you\
                  \ that the strange men and women wandering around the\
                  \ campus had been stalking him for hours. They cornered him\
                  \ in his office and tied him up, although for what purpose, Rice\
                  \ isn’t sure. You inform him that Dr. Armitage sent you, and\
                  \ Rice looks relieved, although he suspects that Dr. Morgan\
                  \ might be in danger as well. Because the strangers on campus\
                  \ seem to have been targeting Professor Rice, you decide that\
                  \ the best course of action is to escort him away from the\
                  \ campus as quickly as possible. As you leave the university,\
                  \ you hear screaming from the northern end of the campus. An\
                  \ ambulance passes you by, and you fear the worst. Hours later,\
                  \ you learn that a ‘rabid dog of some sort’ found its way into\
                  \ the university dormitories. The creature attacked the students\
                  \ inside, and many were mauled or killed in the attack."
                   ]
                 ]
             ]
           , Record TheInvestigatorsRescuedProfessorWarrenRice
           , AddToken Tablet
           , chooseOne
             leadInvestigatorId
             [ Label
               "Add Professor Warren Rice to your deck"
               [AddCampaignCardToDeck leadInvestigatorId "02061"]
             , Label "Do not add Professor Warren Rice to your deck" []
             ]
           ]
          <> [ GainXP iid xp | iid <- investigatorIds ]
          <> [EndOfGame]
          )
      ScenarioResolution (Resolution 2) -> do
        leadInvestigatorId <- getLeadInvestigatorId
        investigatorIds <- getInvestigatorIds
        xp <- getXp
        s <$ unshiftMessages
          ([ chooseOne
             leadInvestigatorId
             [ Run
                 [ Continue "Continue"
                 , FlavorText
                   Nothing
                   [ "You pull each of the dormitory’s fire alarms\
                  \ and usher the students out of the building’s north exit,\
                  \ hoping to make your way off campus. Many of the students\
                  \ are confused and exhausted, but you believe an attempt to\
                  \ explain the situation will do more harm than good. Minutes\
                  \ later, a terrible screech echoes across the campus, piercing\
                  \ and shrill. You tell the students to wait and head back to the\
                  \ dormitories to investigate. Oddly, you find no trace of the\
                  \ strange creature—a prospect that worries you more than it\
                  \ relieves you. You hurry to the faculty offices to find Professor\
                  \ Rice, but there is no sign of him anywhere."
                   ]
                 ]
             ]
           , Record ProfessorWarrenRiceWasKidnapped
           , Record TheStudentsWereRescued
           ]
          <> [ GainXP iid xp | iid <- investigatorIds ]
          <> [EndOfGame]
          )
      ScenarioResolution (Resolution 3) -> do
        leadInvestigatorId <- getLeadInvestigatorId
        investigatorIds <- getInvestigatorIds
        xp <- getXp
        s <$ unshiftMessages
          ([ chooseOne
             leadInvestigatorId
             [ Run
                 [ Continue "Continue"
                 , FlavorText
                   Nothing
                   [ "After defeating the strange and terrifying\
                  \ creature from the Department of Alchemy, you rush to the\
                  \ faculty offices to find Professor Rice. By the time you get to his\
                  \ office, there is no sign of him anywhere."
                   ]
                 ]
             ]
           , Record ProfessorWarrenRiceWasKidnapped
           , Record TheExperimentWasDefeated
           ]
          <> [ GainXP iid xp | iid <- investigatorIds ]
          <> [EndOfGame]
          )
      ScenarioResolution (Resolution 4) -> do
        leadInvestigatorId <- getLeadInvestigatorId
        investigatorIds <- getInvestigatorIds
        xp <- getXp
        s <$ unshiftMessages
          ([ chooseOne
             leadInvestigatorId
             [ Run
                 [ Continue "Continue"
                 , FlavorText
                   Nothing
                   [ "You awaken hours later, exhausted and\
                  \ injured. You’re not sure what you saw, but the sight of it filled\
                  \ your mind with terror. From other survivors, you learn that\
                  \ a ‘rabid dog of some sort’ found its way into the university\
                  \ dormitories. The creature attacked the students inside, and\
                  \ many were mauled or killed in the attack."
                   ]
                 ]
             ]
           , Record InvestigatorsWereUnconsciousForSeveralHours
           , Record ProfessorWarrenRiceWasKidnapped
           , Record TheInvestigatorsFailedToSaveTheStudents
           , AddToken Tablet
           ]
          <> [ GainXP iid (xp + 1) | iid <- investigatorIds ]
          <> [EndOfGame]
          )
      _ -> ExtracurricularActivity <$> runMessage msg attrs
