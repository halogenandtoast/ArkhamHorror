module Arkham.Scenario.Scenarios.ExtracurricularActivity where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Card.Cost
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Campaign
import Arkham.Investigator.Attrs ( Field (..) )
import Arkham.Location.Cards qualified as Locations
import Arkham.Message
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Target
import Arkham.Token

newtype ExtracurricularActivity = ExtracurricularActivity ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

extracurricularActivity :: Difficulty -> ExtracurricularActivity
extracurricularActivity difficulty = scenario
  ExtracurricularActivity
  "02041"
  "Extracurricular Activity"
  difficulty
  [ "orneLibrary        miskatonicQuad scienceBuilding alchemyLabs"
  , "humanitiesBuilding studentUnion   administrationBuilding ."
  , ".                  dormitories    facultyOffices         ."
  ]

extracurricularActivityIntro :: Message
extracurricularActivityIntro = FlavorText
  (Just "Scenario I-A: Extracurricular Activity")
  [ "Dr. Armitage is worried his colleague, Professor Warren Rice, might be\
    \ in trouble, so he has asked for your help in finding his friend. He seems\
    \ unreasonably nervous about his colleague’s disappearance considering\
    \ Professor Rice has only been “missing” for a matter of hours…"
  ]

instance HasTokenValue ExtracurricularActivity where
  getTokenValue iid tokenFace (ExtracurricularActivity attrs) =
    case tokenFace of
      Skull -> pure $ toTokenValue attrs Skull 1 2
      Cultist -> do
        discardCount <- fieldMap InvestigatorDiscard length iid
        pure $ TokenValue
          Cultist
          (NegativeModifier $ if discardCount >= 10
            then (if isEasyStandard attrs then 3 else 5)
            else 1
          )
      ElderThing -> pure $ TokenValue Tablet (NegativeModifier 0) -- determined by an effect
      otherFace -> getTokenValue iid otherFace attrs

instance RunMessage ExtracurricularActivity where
  runMessage msg s@(ExtracurricularActivity attrs) = case msg of
    Setup -> do
      investigatorIds <- getInvestigatorIds
      completedTheHouseAlwaysWins <- elem "02062" <$> getCompletedScenarios
      encounterDeck <- buildEncounterDeckExcluding
        [ Enemies.theExperiment
        , Assets.jazzMulligan
        , Assets.alchemicalConcoction
        ]
        [ EncounterSet.ExtracurricularActivity
        , EncounterSet.Sorcery
        , EncounterSet.TheBeyond
        , EncounterSet.BishopsThralls
        , EncounterSet.Whippoorwills
        , EncounterSet.AncientEvils
        , EncounterSet.LockedDoors
        , EncounterSet.AgentsOfYogSothoth
        ]

      miskatonicQuad <- genCard Locations.miskatonicQuad
      humanitiesBuilding <- genCard Locations.humanitiesBuilding
      orneLibrary <- genCard Locations.orneLibrary
      studentUnion <- genCard Locations.studentUnion
      scienceBuilding <- genCard Locations.scienceBuilding
      administrationBuilding <- genCard Locations.administrationBuilding

      let miskatonicQuadId = toLocationId miskatonicQuad

      pushAllEnd
        [ SetEncounterDeck encounterDeck
        , SetAgendaDeck
        , SetActDeck
        , PlaceLocation miskatonicQuad
        , PlaceLocation orneLibrary
        , PlaceLocation humanitiesBuilding
        , PlaceLocation studentUnion
        , PlaceLocation scienceBuilding
        , PlaceLocation administrationBuilding
        , RevealLocation Nothing miskatonicQuadId
        , MoveAllTo (toSource attrs) miskatonicQuadId
        , story investigatorIds extracurricularActivityIntro
        ]

      setAsideCards <- traverse
        genCard
        [ if completedTheHouseAlwaysWins
          then Locations.facultyOfficesTheHourIsLate
          else Locations.facultyOfficesTheNightIsStillYoung
        , Assets.jazzMulligan
        , Assets.alchemicalConcoction
        , Enemies.theExperiment
        , Locations.dormitories
        , Locations.alchemyLabs
        , Assets.professorWarrenRice
        ]

      ExtracurricularActivity <$> runMessage
        msg
        (attrs
        & (setAsideCardsL .~ setAsideCards)
        & (actStackL
          . at 1
          ?~ [Acts.afterHours, Acts.ricesWhereabouts, Acts.campusSafety]
          )
        & (agendaStackL
          . at 1
          ?~ [ Agendas.quietHalls
             , Agendas.deadOfNight
             , Agendas.theBeastUnleashed
             ]
          )
        )
    ResolveToken drawnToken ElderThing iid -> s <$ push
      (DiscardTopOfDeck
        iid
        (if isEasyStandard attrs then 2 else 3)
        (Just $ TokenTarget drawnToken)
      )
    FailedSkillTest iid _ _ (TokenTarget token) _ _ ->
      s <$ case tokenFace token of
        Skull -> push $ DiscardTopOfDeck
          iid
          (if isEasyStandard attrs then 3 else 5)
          Nothing
        _ -> pure ()
    DiscardedTopOfDeck _iid cards target@(TokenTarget token) ->
      s <$ case tokenFace token of
        ElderThing -> do
          let
            n = sum $ map
              (toPrintedCost . fromMaybe (StaticCost 0) . cdCost . toCardDef)
              cards
          push $ CreateTokenValueEffect (-n) (toSource attrs) target
        _ -> pure ()
    ScenarioResolution NoResolution -> do
      leadInvestigatorId <- getLeadInvestigatorId
      xp <- getXp
      s <$ pushAll
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
        <> [ GainXP iid (n + 1) | (iid, n) <- xp ]
        <> [EndOfGame Nothing]
        )
    ScenarioResolution (Resolution 1) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      xp <- getXp
      s <$ pushAll
        ([ chooseOne
           leadInvestigatorId
           [ Run
               [ Continue "Continue"
               , FlavorText
                 (Just "Resolution 1")
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
             [ AddCampaignCardToDeck
                 leadInvestigatorId
                 Assets.professorWarrenRice
             ]
           , Label "Do not add Professor Warren Rice to your deck" []
           ]
         ]
        <> [ GainXP iid n | (iid, n) <- xp ]
        <> [EndOfGame Nothing]
        )
    ScenarioResolution (Resolution 2) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      xp <- getXp
      s <$ pushAll
        ([ chooseOne
           leadInvestigatorId
           [ Run
               [ Continue "Continue"
               , FlavorText
                 (Just "Resolution 2")
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
        <> [ GainXP iid n | (iid, n) <- xp ]
        <> [EndOfGame Nothing]
        )
    ScenarioResolution (Resolution 3) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      xp <- getXp
      s <$ pushAll
        ([ chooseOne
           leadInvestigatorId
           [ Run
               [ Continue "Continue"
               , FlavorText
                 (Just "Resolution 3")
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
        <> [ GainXP iid n | (iid, n) <- xp ]
        <> [EndOfGame Nothing]
        )
    ScenarioResolution (Resolution 4) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      xp <- getXp
      s <$ pushAll
        ([ chooseOne
           leadInvestigatorId
           [ Run
               [ Continue "Continue"
               , FlavorText
                 (Just "Resolution 4")
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
        <> [ GainXP iid (n + 1) | (iid, n) <- xp ]
        <> [EndOfGame Nothing]
        )
    _ -> ExtracurricularActivity <$> runMessage msg attrs
