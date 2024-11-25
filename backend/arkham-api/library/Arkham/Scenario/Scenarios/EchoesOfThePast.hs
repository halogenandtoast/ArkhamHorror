module Arkham.Scenario.Scenarios.EchoesOfThePast (EchoesOfThePast (..), echoesOfThePast) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Campaigns.ThePathToCarcosa.Helpers
import Arkham.Card
import Arkham.Classes hiding (matches)
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (enemyAt)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Resolution
import Arkham.Scenario.Helpers hiding (
  addCampaignCardToDeckChoice,
  forceAddCampaignCardToDeckChoice,
  recordSetInsert,
  roundModifier,
 )
import Arkham.Scenario.Import.Lifted hiding (replicate)
import Arkham.Scenarios.EchoesOfThePast.Helpers
import Arkham.Scenarios.EchoesOfThePast.Story
import Arkham.Token
import Arkham.Trait (Trait (SecondFloor, ThirdFloor))
import Arkham.Treachery.Cards qualified as Treacheries

newtype EchoesOfThePast = EchoesOfThePast ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

echoesOfThePast :: Difficulty -> EchoesOfThePast
echoesOfThePast difficulty =
  scenario
    EchoesOfThePast
    "03120"
    "Echoes of the Past"
    difficulty
    [ "thirdFloor1  quietHalls2 thirdFloor2  . ."
    , "secondFloor1 quietHalls1 secondFloor2 . hiddenLibrary"
    , "groundFloor1 entryHall   groundFloor2 . ."
    ]

instance HasChaosTokenValue EchoesOfThePast where
  getChaosTokenValue iid chaosTokenFace (EchoesOfThePast attrs) = case chaosTokenFace of
    Skull -> do
      highestDoom <- fieldMax EnemyDoom AnyInPlayEnemy
      totalDoom <- selectSum EnemyDoom AnyInPlayEnemy
      pure $ toChaosTokenValue attrs Skull highestDoom totalDoom
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 4
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 4
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 2 4
    otherFace -> getChaosTokenValue iid otherFace attrs

placeAndLabelLocations :: ReverseQueue m => Text -> [CardDef] -> ScenarioBuilderT m [LocationId]
placeAndLabelLocations prefix locations =
  for (withIndex1 locations) $ \(idx, location) -> do
    l <- place location
    push $ SetLocationLabel l (prefix <> tshow idx)
    pure l

placeAndLabelLocations_ :: ReverseQueue m => Text -> [CardDef] -> ScenarioBuilderT m ()
placeAndLabelLocations_ prefix locations = void $ placeAndLabelLocations prefix locations

standaloneChaosTokens :: [ChaosTokenFace]
standaloneChaosTokens =
  [ PlusOne
  , Zero
  , Zero
  , MinusOne
  , MinusOne
  , MinusOne
  , MinusTwo
  , MinusTwo
  , MinusThree
  , MinusFour
  , Skull
  , Skull
  , Skull
  , AutoFail
  , ElderSign
  ]

instance RunMessage EchoesOfThePast where
  runMessage msg s@(EchoesOfThePast attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story intro
      whenInterviewed Assets.sebastienMoreau $ story sebastiensInformation
      pure s
    StandaloneSetup -> do
      randomToken <- sample (Cultist :| [Tablet, ElderThing])
      setChaosTokens $ standaloneChaosTokens <> [randomToken, randomToken]
      pure s
    Setup -> runScenarioSetup EchoesOfThePast attrs do
      gather Set.EchoesOfThePast
      gather Set.CultOfTheYellowSign
      gather Set.Delusions
      gather Set.LockedDoors
      gather Set.DarkCult
      gatherJust Set.TheMidnightMasks [Treacheries.falseLead, Treacheries.huntingShadow]

      groundFloor <-
        sampleN 2
          $ Locations.historicalSocietyMeetingRoom
          :| [ Locations.historicalSocietyRecordOffice_129
             , Locations.historicalSocietyHistoricalMuseum_130
             ]

      secondFloor <-
        sampleN 2
          $ Locations.historicalSocietyHistoricalMuseum_132
          :| [ Locations.historicalSocietyHistoricalLibrary_133
             , Locations.historicalSocietyReadingRoom
             ]

      thirdFloor <-
        sampleN 2
          $ Locations.historicalSocietyHistoricalLibrary_136
          :| [ Locations.historicalSocietyPeabodysOffice
             , Locations.historicalSocietyRecordOffice_138
             ]

      entryHall <- place Locations.entryHall
      startAt entryHall
      placeLabeled_ "quietHalls1" Locations.quietHalls_131
      placeLabeled_ "quietHalls2" Locations.quietHalls_135

      placeAndLabelLocations_ "groundFloor" groundFloor
      secondFloorPlacements <- shuffleM =<< placeAndLabelLocations "secondFloor" secondFloor
      thirdFloorPlacements <- shuffleM =<< placeAndLabelLocations "thirdFloor" thirdFloor

      whenInterviewed Assets.sebastienMoreau $ placeTokens attrs entryHall Clue 1

      getPlayerCount >>= \case
        1 -> pure ()
        2 ->
          enemyAtMatching Enemies.seekerOfCarcosa
            $ EmptyLocation
            <> LocationWithTrait ThirdFloor
            <> mapOneOf LocationWithId thirdFloorPlacements
        3 -> for_ thirdFloorPlacements $ enemyAt Enemies.seekerOfCarcosa
        4 ->
          replicateM_ 3
            $ enemyAtMatching Enemies.seekerOfCarcosa
            $ EmptyLocation
            <> oneOf [LocationWithTrait SecondFloor, LocationWithTrait ThirdFloor]
            <> mapOneOf LocationWithId (secondFloorPlacements <> thirdFloorPlacements)
        _ -> error "Invalid player count"

      whenHasRecord YouFledTheDinnerParty do
        eachInvestigator $ \iid -> do
          roundModifier attrs iid $ AdditionalActions "Fleeing the dinner party" (toSource attrs) 1

      setAside
        [ Locations.hiddenLibrary
        , Enemies.possessedOathspeaker
        , Assets.mrPeabody
        , Assets.theTatteredCloak
        , Assets.claspOfBlackOnyx
        ]
      setAgendaDeck
        [Agendas.theTruthIsHidden, Agendas.ransackingTheManor, Agendas.secretsBetterLeftHidden]
      setActDeck [Acts.raceForAnswers, Acts.mistakesOfThePast, Acts.theOath]
    ResolveChaosToken _ Cultist iid | isHardExpert attrs -> do
      es <- select $ NearestEnemyTo iid AnyEnemy
      chooseTargetM iid es \target -> placeDoom Cultist target 1
      pure s
    ResolveChaosToken _ Tablet iid | isHardExpert attrs -> do
      randomDiscard iid Tablet
      pure s
    ResolveChaosToken _ ElderThing iid | isHardExpert attrs -> do
      whenAny (EnemyAt YourLocation) $ assignHorror iid ElderThing 1
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ | isEasyStandard attrs -> do
      case chaosTokenFace token of
        Cultist -> do
          es <- select $ NearestEnemyTo iid AnyEnemy
          chooseTargetM iid es \target -> placeDoom Cultist target 1
        Tablet -> randomDiscard iid Tablet
        ElderThing -> whenAny (EnemyAt YourLocation) $ assignHorror iid ElderThing 1
        _ -> pure ()
      pure s
    ScenarioResolution NoResolution -> do
      story noResolution
      push R4
      pure s
    ScenarioResolution (Resolution n) -> do
      investigators <- allInvestigators
      case n of
        1 -> do
          story resolution1
          record YouTookTheOnyxClasp
          markConviction
          forceAddCampaignCardToDeckChoice investigators Assets.claspOfBlackOnyx
        2 -> do
          story resolution2
          record YouLeftTheOnyxClaspBehind
          markDoubt
        3 -> do
          story resolution3
          record YouDestroyedTheOathspeaker
          addCampaignCardToDeckChoice investigators Assets.theTatteredCloak
        4 -> do
          story resolution4
          record TheFollowersOfTheSignHaveFoundTheWayForward
        _ -> error "Invalid resolution"

      allGainXpWithBonus attrs $ if n == 4 then toBonus "resolution4" 1 else NoBonus
      sebastienSlain <- selectOne (VictoryDisplayCardMatch $ basic $ cardIs Enemies.sebastienMoreau)
      for_ sebastienSlain $ \sebastien ->
        recordSetInsert VIPsSlain [toCardCode sebastien]

      removeAllChaosTokens Cultist
      removeAllChaosTokens Tablet
      removeAllChaosTokens ElderThing
      replicateM_ 2 $ addChaosToken $ case n of
        1 -> Cultist
        2 -> Tablet
        3 -> Tablet
        4 -> ElderThing
        _ -> error "Invalid resolution"
      endOfScenario
      pure s
    _ -> EchoesOfThePast <$> liftRunMessage msg attrs
