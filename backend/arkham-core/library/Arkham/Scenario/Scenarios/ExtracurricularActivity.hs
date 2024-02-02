module Arkham.Scenario.Scenarios.ExtracurricularActivity where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Campaign
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Message
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.ExtracurricularActivity.FlavorText

newtype ExtracurricularActivity = ExtracurricularActivity ScenarioAttrs
  deriving stock (Generic)
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, NoThunks, NFData, Eq)

extracurricularActivity :: Difficulty -> ExtracurricularActivity
extracurricularActivity difficulty =
  scenario
    ExtracurricularActivity
    "02041"
    "Extracurricular Activity"
    difficulty
    [ "orneLibrary        miskatonicQuad scienceBuilding alchemyLabs"
    , "humanitiesBuilding studentUnion   administrationBuilding ."
    , ".                  dormitories    facultyOffices         ."
    ]

instance HasChaosTokenValue ExtracurricularActivity where
  getChaosTokenValue iid chaosTokenFace (ExtracurricularActivity attrs) =
    case chaosTokenFace of
      Skull -> pure $ toChaosTokenValue attrs Skull 1 2
      Cultist -> do
        discardCount <- fieldMap InvestigatorDiscard length iid
        pure
          $ ChaosTokenValue Cultist
          $ NegativeModifier
          $ if discardCount >= 10 then (if isEasyStandard attrs then 3 else 5) else 1
      ElderThing -> pure $ ChaosTokenValue Tablet (NegativeModifier 0) -- determined by an effect
      otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage ExtracurricularActivity where
  runMessage msg s@(ExtracurricularActivity attrs) = case msg of
    Setup -> do
      players <- allPlayers
      completedTheHouseAlwaysWins <- elem "02062" <$> getCompletedScenarios
      encounterDeck <-
        buildEncounterDeckExcluding
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

      (miskatonicQuadId, placeMiskatonicQuad) <- placeLocationCard Locations.miskatonicQuad
      placeOtherLocations <-
        traverse
          placeLocationCard_
          [ Locations.humanitiesBuilding
          , Locations.orneLibrary
          , Locations.studentUnion
          , Locations.scienceBuilding
          , Locations.administrationBuilding
          ]

      pushAll
        $ [ SetEncounterDeck encounterDeck
          , SetAgendaDeck
          , SetActDeck
          , placeMiskatonicQuad
          ]
        <> placeOtherLocations
        <> [ RevealLocation Nothing miskatonicQuadId
           , MoveAllTo (toSource attrs) miskatonicQuadId
           , story players intro
           ]

      setAsideCards <-
        genCards
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

      agendas <-
        genCards
          [Agendas.quietHalls, Agendas.deadOfNight, Agendas.theBeastUnleashed]
      acts <-
        genCards
          [Acts.afterHours, Acts.ricesWhereabouts, Acts.campusSafety]

      ExtracurricularActivity
        <$> runMessage
          msg
          ( attrs
              & (setAsideCardsL .~ setAsideCards)
              & (actStackL . at 1 ?~ acts)
              & (agendaStackL . at 1 ?~ agendas)
          )
    ResolveChaosToken drawnToken ElderThing iid -> do
      push
        $ DiscardTopOfDeck
          iid
          (if isEasyStandard attrs then 2 else 3)
          (ChaosTokenEffectSource ElderThing)
          (Just $ ChaosTokenTarget drawnToken)
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget (chaosTokenFace -> Skull)) _ _ -> do
      push
        $ DiscardTopOfDeck
          iid
          (if isEasyStandard attrs then 3 else 5)
          (ChaosTokenEffectSource Skull)
          Nothing
      pure s
    DiscardedTopOfDeck _iid cards _ target@(ChaosTokenTarget (chaosTokenFace -> ElderThing)) -> do
      let n = sum $ map (toPrintedCost . fromMaybe (StaticCost 0) . cdCost . toCardDef) cards
      push $ CreateChaosTokenValueEffect (-n) (toSource attrs) target
      pure s
    ScenarioResolution NoResolution -> do
      players <- allPlayers
      xp <- getXp
      pushAll
        $ [ story players noResolution
          , Record ProfessorWarrenRiceWasKidnapped
          , Record TheInvestigatorsFailedToSaveTheStudents
          , AddChaosToken Tablet
          ]
        <> [GainXP iid (toSource attrs) (n + 1) | (iid, n) <- xp]
        <> [EndOfGame Nothing]
      pure s
    ScenarioResolution (Resolution 1) -> do
      lead <- getLeadPlayer
      leadInvestigatorId <- getLeadInvestigatorId
      players <- allPlayers
      xp <- getXp
      pushAll
        $ [ story players resolution1
          , Record TheInvestigatorsRescuedProfessorWarrenRice
          , AddChaosToken Tablet
          , chooseOne
              lead
              [ Label
                  "Add Professor Warren Rice to your deck"
                  [AddCampaignCardToDeck leadInvestigatorId Assets.professorWarrenRice]
              , Label "Do not add Professor Warren Rice to your deck" []
              ]
          ]
        <> [GainXP iid (toSource attrs) n | (iid, n) <- xp]
        <> [EndOfGame Nothing]
      pure s
    ScenarioResolution (Resolution 2) -> do
      players <- allPlayers
      xp <- getXp
      pushAll
        $ [ story players resolution2
          , Record ProfessorWarrenRiceWasKidnapped
          , Record TheStudentsWereRescued
          ]
        <> [GainXP iid (toSource attrs) n | (iid, n) <- xp]
        <> [EndOfGame Nothing]
      pure s
    ScenarioResolution (Resolution 3) -> do
      players <- allPlayers
      xp <- getXp
      pushAll
        $ [ story players resolution3
          , Record ProfessorWarrenRiceWasKidnapped
          , Record TheExperimentWasDefeated
          ]
        <> [GainXP iid (toSource attrs) n | (iid, n) <- xp]
        <> [EndOfGame Nothing]
      pure s
    ScenarioResolution (Resolution 4) -> do
      players <- allPlayers
      xp <- getXp
      pushAll
        $ [ story players resolution4
          , Record InvestigatorsWereUnconsciousForSeveralHours
          , Record ProfessorWarrenRiceWasKidnapped
          , Record TheInvestigatorsFailedToSaveTheStudents
          , AddChaosToken Tablet
          ]
        <> [GainXP iid (toSource attrs) (n + 1) | (iid, n) <- xp]
        <> [EndOfGame Nothing]
      pure s
    _ -> ExtracurricularActivity <$> runMessage msg attrs
