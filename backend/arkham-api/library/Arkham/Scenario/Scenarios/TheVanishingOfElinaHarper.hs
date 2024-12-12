module Arkham.Scenario.Scenarios.TheVanishingOfElinaHarper (
  TheVanishingOfElinaHarper (..),
  theVanishingOfElinaHarper,
) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Difficulty
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Exception
import Arkham.Helpers.Agenda (getCurrentAgendaStep)
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheVanishingOfElinaHarper.Helpers
import Arkham.Story.Cards qualified as Stories
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheVanishingOfElinaHarper = TheVanishingOfElinaHarper ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theVanishingOfElinaHarper :: Difficulty -> TheVanishingOfElinaHarper
theVanishingOfElinaHarper difficulty =
  scenario
    TheVanishingOfElinaHarper
    "07056"
    "The Vanishing of Elina Harper"
    difficulty
    [ "esotericOrderOfDagon .                    newChurchGreen  .                theHouseOnWaterStreet"
    , "esotericOrderOfDagon firstNationalGrocery newChurchGreen  marshRefinery    theHouseOnWaterStreet"
    , "theLittleBookshop    firstNationalGrocery innsmouthSquare marshRefinery    innsmouthHarbour"
    , "theLittleBookshop    gilmanHouse          innsmouthSquare fishStreetBridge innsmouthHarbour"
    , "sawboneAlley         gilmanHouse          innsmouthJail   fishStreetBridge shorewardSlums"
    , "sawboneAlley         .                    innsmouthJail   .                shorewardSlums"
    ]

instance HasChaosTokenValue TheVanishingOfElinaHarper where
  getChaosTokenValue iid tokenFace (TheVanishingOfElinaHarper attrs) = case tokenFace of
    Skull -> do
      n <- getCurrentAgendaStep
      pure $ toChaosTokenValue attrs Skull n (n + 1)
    Cultist -> pure $ ChaosTokenValue Cultist (NegativeModifier 2)
    Tablet -> pure $ ChaosTokenValue Tablet (NegativeModifier 3)
    ElderThing -> pure $ ChaosTokenValue ElderThing (NegativeModifier 4)
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheVanishingOfElinaHarper where
  runMessage msg s@(TheVanishingOfElinaHarper attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "intro1"
      story $ i18n "townInfo"
      story $ i18nWithTitle "intro2"
      standalone <- getIsStandalone
      unless standalone do
        eachInvestigator chooseUpgradeDeck
        push ReloadDecks
      pure s
    StandaloneSetup -> do
      {- FOURMOLU_DISABLE -}
      setChaosTokens
        [ #"+1" , #"0" , #"0" , #"-1" , #"-1" , #"-1" , #"-2" , #"-2" , #"-3" , #"-4"
        , Skull , Skull , Cultist , Cultist , Tablet , Tablet , ElderThing , ElderThing
        , AutoFail , ElderSign
        ]
      {- FOURMOLU_ENABLE -}
      pure s
    Setup -> runScenarioSetup TheVanishingOfElinaHarper attrs do
      gather Set.TheVanishingOfElinaHarper
      gather Set.AgentsOfDagon
      gather Set.FogOverInnsmouth
      gather Set.TheLocals
      gather Set.ChillingCold
      gather Set.LockedDoors
      gather Set.Nightgaunts
      gatherJust Set.TheMidnightMasks [Treacheries.falseLead, Treacheries.huntingShadow]

      setAgendaDeck [Agendas.decrepitDecay, Agendas.growingSuspicion]
      setActDeck [Acts.theSearchForAgentHarper]

      startAt =<< place Locations.innsmouthSquare

      placeAll
        [ Locations.marshRefinery
        , Locations.innsmouthHarbour
        , Locations.fishStreetBridge
        , Locations.firstNationalGrocery
        , Locations.gilmanHouse
        , Locations.theLittleBookshop
        ]

      (hideout, remainingHideouts) <- sampleWithRest hideouts
      (kidnapper, remainingSuspects) <- sampleWithRest suspects

      excludeFromEncounterDeck [hideout, kidnapper]
      addExtraDeck LeadsDeck =<< shuffle (remainingHideouts <> remainingSuspects)

      setAside
        [ Agendas.franticPursuit
        , Acts.theRescue
        , Assets.thomasDawsonSoldierInANewWar
        , Assets.elinaHarperKnowsTooMuch
        , Enemies.huntingNightgaunt
        , Enemies.huntingNightgaunt
        , Enemies.wingedOneFogOverInnsmouth
        ]

      findingAgentHarper <- genCard Stories.findingAgentHarper
      push $ PlaceStory findingAgentHarper Global
      let target = StoryTarget $ StoryId $ coerce $ toCardCode findingAgentHarper
      kidnapperCard <- genCard kidnapper
      hideoutCard <- genCard hideout
      placeUnderneath target [kidnapperCard, hideoutCard]
      setMeta $ Meta {kidnapper = kidnapperCard, hideout = hideoutCard}
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      let amount = if isEasyStandard attrs then 1 else 2
      case token.face of
        Cultist -> do
          closestEnemy <- select $ NearestEnemyTo iid AnyEnemy
          chooseTargetM iid closestEnemy \x -> placeDoom Cultist x amount
        Tablet ->
          if isEasyStandard attrs
            then assignHorror iid Tablet 1
            else assignDamageAndHorror iid Tablet 1 1
        ElderThing -> placeCluesOnLocation iid ElderThing amount
        _ -> pure ()
      pure s
    PassedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      when (isHardExpert attrs) do
        case token.face of
          Cultist -> do
            closestEnemy <- select $ NearestEnemyTo iid AnyEnemy
            chooseTargetM iid closestEnemy \x -> placeDoom Cultist x 1
          Tablet -> assignHorror iid Tablet 1
          ElderThing -> placeCluesOnLocation iid ElderThing 1
          _ -> pure ()
      pure s
    ScenarioResolution resolution -> scope "resolutions" do
      case resolution of
        NoResolution -> do
          story $ i18n "noResolution"
          push R1
        Resolution n -> do
          story $ i18n $ case n of
            1 -> "resolution1"
            2 -> "resolution2"
            3 -> "resolution3"
            4 -> "resolution4"
            5 -> "resolution5"
            6 -> "resolution6"
            7 -> "resolution7"
            _ -> throw $ UnknownResolution resolution
          case n of
            1 -> record TheMissionFailed
            _ -> record TheMissionWasSuccessful
          allGainXp attrs
          endOfScenario
      pure s
    _ -> TheVanishingOfElinaHarper <$> liftRunMessage msg attrs
