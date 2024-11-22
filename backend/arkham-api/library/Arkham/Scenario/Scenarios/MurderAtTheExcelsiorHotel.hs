module Arkham.Scenario.Scenarios.MurderAtTheExcelsiorHotel (
  MurderAtTheExcelsiorHotel (..),
  murderAtTheExcelsiorHotel,
) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Classes
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers hiding (skillTestModifier)
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Helpers hiding (
  addCampaignCardToDeckChoice,
  forceAddCampaignCardToDeckChoice,
  skillTestModifier,
 )
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Types (ScenarioAttrs (..))
import Arkham.ScenarioLogKey
import Arkham.Scenarios.MurderAtTheExcelsiorHotel.FlavorText
import Arkham.Trait (Trait (Detective, Guest, Innocent, Madness, Police))
import Arkham.Treachery.Cards qualified as Treacheries

newtype MurderAtTheExcelsiorHotel = MurderAtTheExcelsiorHotel ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

murderAtTheExcelsiorHotel :: Difficulty -> MurderAtTheExcelsiorHotel
murderAtTheExcelsiorHotel difficulty =
  sideStory
    MurderAtTheExcelsiorHotel
    "84001"
    "Murder at the Excelsior Hotel"
    difficulty
    [ ".           .         hotelRoof       hotelRoof       room245  room245 .            .           "
    , "room212     room212   hotelRoof       hotelRoof       room245  room245 .            .           "
    , "room212     room212   secondFloorHall secondFloorHall room225  room225 suiteBalcony suiteBalcony"
    , "restaurant restaurant secondFloorHall secondFloorHall room225  room225 suiteBalcony suiteBalcony"
    , "restaurant restaurant foyer           foyer           office   office  .            ."
    , ".          .          foyer           foyer           office   office  .            ."
    , ".          .          .               basement        basement .       .            ."
    , ".          .          .               basement        basement .       .            ."
    ]

instance HasChaosTokenValue MurderAtTheExcelsiorHotel where
  getChaosTokenValue iid tokenFace (MurderAtTheExcelsiorHotel attrs) = case tokenFace of
    Skull -> do
      guests <- selectCount $ EnemyWithTrait Guest
      innocents <- selectCount $ EnemyWithTrait Innocent
      pure $ toChaosTokenValue attrs Skull guests innocents
    Cultist -> pure $ toChaosTokenValue attrs Cultist 1 2
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 5
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 3
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage MurderAtTheExcelsiorHotel where
  runMessage msg s@(MurderAtTheExcelsiorHotel attrs) = runQueueT $ case msg of
    PreScenarioSetup -> do
      n <- getPlayerCount
      story intro1
      story $ if n == 1 then intro2 else intro3
      story intro4
      pure s
    Setup -> runScenarioSetup MurderAtTheExcelsiorHotel attrs do
      lead <- getLead
      otherPlayers <- deleteFirst lead <$> allInvestigators
      gather Set.MurderAtTheExcelsiorHotel

      room225 <- place Locations.room225
      foyer <- place Locations.foyerMurderAtTheExcelsiorHotel

      placeAll [Locations.suiteBalcony, Locations.secondFloorHall, Locations.restaurant]

      moveTo_ attrs lead room225
      beginWithStoryAsset lead Assets.bloodstainedDagger
      for_ otherPlayers \p -> moveTo_ attrs p foyer

      setAside
        [ Assets.sergeantMonroe
        , Treacheries.whatHaveYouDone
        , Enemies.arkhamOfficer
        , Enemies.arkhamOfficer
        , Enemies.arkhamOfficer
        ]

      setAgendaDeck [Agendas.theMurder, Agendas.specialInvestigation]
      setActDeck [Acts.whatHappened, Acts.followingLeads]

      addExtraDeck LeadsDeck
        =<< shuffle
          [ Assets.alienDevice
          , Assets.managersKey
          , Assets.tomeOfRituals
          , Assets.sinisterSolution
          , Assets.timeWornLocket
          ]
    StandaloneSetup -> do
      let
        tokens =
          if isEasyStandard attrs
            then
              [ PlusOne
              , Zero
              , MinusOne
              , MinusOne
              , MinusTwo
              , MinusThree
              , MinusThree
              , MinusFour
              , Skull
              , Skull
              , Cultist
              , Tablet
              , ElderThing
              , AutoFail
              , ElderSign
              ]
            else
              [ Zero
              , MinusOne
              , MinusTwo
              , MinusThree
              , MinusFour
              , MinusFour
              , MinusFive
              , MinusSix
              , Skull
              , Skull
              , Cultist
              , Tablet
              , ElderThing
              , AutoFail
              , ElderSign
              ]
      setChaosTokens tokens
      pure s
    ResolveChaosToken _ Cultist iid -> do
      innocentInVictory <- selectAny $ VictoryDisplayCardMatch $ basic $ #enemy <> withTrait Innocent
      pushWhen innocentInVictory $ DrawAnotherChaosToken iid
      pure s
    ResolveChaosToken token Tablet iid -> do
      clues <- field InvestigatorClues iid
      mLocation <- field InvestigatorLocation iid
      for_ mLocation \_ ->
        when (clues > 0) do
          let n = if isEasyStandard attrs then 1 else 2
          withSkillTest \sid ->
            chooseOneM iid do
              labeled ("Place one of your clues on your location to treat this as a -" <> tshow n) do
                push $ InvestigatorPlaceCluesOnLocation iid (ChaosTokenEffectSource Tablet) 1
                skillTestModifier sid Tablet token (ChangeChaosTokenModifier (NegativeModifier n))
              labeled "Skip" nothing
      pure s
    ResolveChaosToken _ ElderThing iid -> do
      innocentInVictory <- selectAny $ VictoryDisplayCardMatch $ basic $ #enemy <> withTrait Innocent
      when innocentInVictory $ assignHorror iid (ChaosTokenEffectSource Tablet) 1
      pure s
    ScenarioResolution r -> do
      lead <- getLead
      case r of
        NoResolution -> do
          anyResigned <- selectAny ResignedInvestigator
          if anyResigned
            then do
              story noResolutionResigned
              record TheInvestigatorsFledTheSceneOfTheCrime
              push R3
            else do
              story noResolution
              record TheExcelsiorClaimsAnotherVictim
              push R2
        Resolution 1 -> do
          investigators <- allInvestigators

          story resolution1
          record TheExcelsiorIsQuietForNow
          forceAddCampaignCardToDeckChoice [lead] Treacheries.whatHaveYouDone
          addCampaignCardToDeckChoice [lead] Assets.bloodstainedDagger

          policeOnYourSide <- remembered ThePoliceAreOnYourSide
          when policeOnYourSide do
            addCampaignCardToDeckChoice investigators Assets.sergeantMonroe

          policeInVictory <- selectAny $ VictoryDisplayCardMatch $ basic $ #enemy <> withTrait Police
          when (not policeOnYourSide && policeInVictory) do
            searchCollectionForRandom
              lead
              attrs
              (mapOneOf CardWithTrait [Detective, Madness] <> BasicWeaknessCard)
          allGainXp attrs
          endOfScenario
        Resolution 2 -> do
          if scenarioTimesPlayed attrs == 0
            then do
              story resolution2
              chooseOne
                lead
                [ Label "Play again" [ScenarioResolutionStep 10 (Resolution 2)]
                , Label "Leave things alone" [ScenarioResolutionStep 2 (Resolution 2)]
                ]
            else do
              story resolution2
              push $ ScenarioResolutionStep 2 (Resolution 2)
        Resolution 3 -> do
          if scenarioTimesPlayed attrs == 0
            then do
              story resolution3
              chooseOne
                lead
                [ Label "Play again" [ScenarioResolutionStep 10 (Resolution 3)]
                , Label "Leave things alone" [ScenarioResolutionStep 2 (Resolution 3)]
                ]
            else do
              story resolution3
              push $ ScenarioResolutionStep 2 (Resolution 3)
        _ -> error "Invalid Resolution"
      pure s
    ScenarioResolutionStep 2 (Resolution 2) -> do
      record TheMurdersContinueUnsolved
      allGainXp attrs
      lead <- getLead
      forceAddCampaignCardToDeckChoice [lead] Treacheries.whatHaveYouDone
      addCampaignCardToDeckChoice [lead] Assets.bloodstainedDagger
      endOfScenario
      pure s
    ScenarioResolutionStep 2 (Resolution 3) -> do
      record TheMurdersContinueUnsolved
      allGainXp attrs
      lead <- getLead
      forceAddCampaignCardToDeckChoice [lead] Treacheries.whatHaveYouDone
      addCampaignCardToDeckChoice [lead] Assets.bloodstainedDagger
      searchCollectionForRandom
        lead
        attrs
        (mapOneOf CardWithTrait [Detective, Madness] <> BasicWeaknessCard)
      endOfScenario
      pure s
    ScenarioResolutionStep 10 _ -> do
      standalone <- getIsStandalone
      pushAll
        $ [ResetGame]
        <> [StandaloneSetup | standalone]
        <> [ ChooseLeadInvestigator
           , SetupInvestigators
           , InvestigatorsMulligan
           , Setup
           , EndSetup
           ]
      let resetAttrs = toAttrs $ murderAtTheExcelsiorHotel attrs.difficulty
      pure
        . MurderAtTheExcelsiorHotel
        $ resetAttrs
          { scenarioTimesPlayed = scenarioTimesPlayed attrs + 1
          , scenarioPlayerDecks = scenarioPlayerDecks attrs
          , scenarioStoryCards = scenarioStoryCards attrs
          }
    _ -> MurderAtTheExcelsiorHotel <$> liftRunMessage msg attrs
