module Arkham.Scenario.Scenarios.EchoesOfThePast (echoesOfThePast, EchoesOfThePast (..), setupEchoesOfThePast) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.ThePathToCarcosa.Import
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Exception
import Arkham.Helpers.FlavorText
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Helpers.Query
import Arkham.Helpers.Xp
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (enemyAt)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Modifier
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted hiding (replicate)
import Arkham.Scenarios.EchoesOfThePast.Helpers
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

setupEchoesOfThePast :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> ScenarioBuilderT m ()
setupEchoesOfThePast attrs = do
  pc <- getPlayerCount
  fledTheDinnerParty <- getHasRecord YouFledTheDinnerParty
  setup do
    ul do
      li "gatherSets"
      li "chooseLocations"
      li "placeLocations"
      li "setAside"
      li.nested "changes.instructions" do
        li.validate (pc == 1) "changes.exactly1"
        li.validate (pc == 2) "changes.exactly2"
        li.validate (pc == 3) "changes.exactly3"
        li.validate (pc == 4) "changes.exactly4"
      li.validate fledTheDinnerParty "fledTheDinnerParty"
      unscoped $ li "shuffleRemainder"
  whenReturnTo $ gather Set.ReturnToEchoesOfThePast
  gather Set.EchoesOfThePast
  gather Set.CultOfTheYellowSign
  gather Set.Delusions `orWhenReturnTo` gather Set.MaddeningDelusions
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

  whenReturnTo do
    placeAndLabelLocations_ "basement"
      =<< sampleN
        2
        ( Locations.historicalSocietyDustyArchives
            :| [Locations.historicalSocietyMuseumStorage, Locations.historicalSocietyBoilerRoom]
        )
    placeLabeled_ "quietHalls3" Locations.returnToQuietHalls
    setAside [Enemies.keeperOfTheOath, Enemies.keeperOfTheOath]
    addAdditionalReferences ["52028b"]

  whenInterviewed Assets.sebastienMoreau $ placeTokens attrs entryHall Clue =<< perPlayer 1

  case pc of
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

  when fledTheDinnerParty do
    eachInvestigator \iid -> do
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

instance RunMessage EchoesOfThePast where
  runMessage msg s@(EchoesOfThePast attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      didInterview <- interviewed Assets.sebastienMoreau
      flavor do
        h "title"
        p "body"
        unscoped (campaignI18n (nameVar Assets.sebastienMoreau $ p "checkIfInterviewed"))
        p.validate didInterview "proceedToSebastiensInformation"
        p.validate (not didInterview) "otherwise"
      whenInterviewed Assets.sebastienMoreau $ flavor $ p "sebastiensInformation"
      pure s
    StandaloneSetup -> do
      randomToken <- sample (Cultist :| [Tablet, ElderThing])
      setChaosTokens $ standaloneChaosTokens <> [randomToken, randomToken]
      pure s
    Setup -> runScenarioSetup EchoesOfThePast attrs $ setupEchoesOfThePast attrs
    ResolveChaosToken _ Cultist iid | isHardExpert attrs -> do
      es <- select $ NearestEnemyToFallback iid AnyEnemy
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
          es <- select $ NearestEnemyToFallback iid AnyEnemy
          chooseTargetM iid es \target -> placeDoom Cultist target 1
        Tablet -> randomDiscard iid Tablet
        ElderThing -> whenAny (EnemyAt YourLocation) $ assignHorror iid ElderThing 1
        _ -> pure ()
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          resolution "noResolution"
          do_ R4
        _ -> do_ msg
      pure s
    Do (ScenarioResolution r) -> scope "resolutions" do
      investigators <- allInvestigators
      for_ [Cultist, Tablet, ElderThing] removeAllChaosTokens
      case r of
        Resolution 1 -> do
          record YouTookTheOnyxClasp
          markConviction
          twice $ addChaosToken Cultist
          resolutionWithXp "resolution1" $ allGainXp' attrs
          forceAddCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.claspOfBlackOnyx
        Resolution 2 -> do
          record YouLeftTheOnyxClaspBehind
          twice $ addChaosToken Tablet
          markDoubt
          resolutionWithXp "resolution2" $ allGainXp' attrs
        Resolution 3 -> do
          record YouDestroyedTheOathspeaker
          twice $ addChaosToken Tablet
          resolutionWithXp "resolution3" $ allGainXp' attrs
          addCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.theTatteredCloak
        Resolution 4 -> do
          record TheFollowersOfTheSignHaveFoundTheWayForward
          twice $ addChaosToken ElderThing
          resolutionWithXp "resolution4" $ allGainXpWithBonus' attrs (toBonus "bonus" 1)
        _ -> throw $ UnknownResolution r

      sebastienSlain <- selectOne (VictoryDisplayCardMatch $ basic $ cardIs Enemies.sebastienMoreau)
      for_ sebastienSlain \sebastien -> recordSetInsert VIPsSlain [toCardCode sebastien]
      endOfScenario
      pure s
    UseCardAbility _ ScenarioSource 1 _ _ -> do
      keepers <- getSetAsideCardsMatching $ cardIs Enemies.keeperOfTheOath
      case keepers of
        (keeper : _) -> createEnemyAtLocationMatching_ keeper $ EmptyLocation <> "Historical Society"
        _ -> pure ()
      pure s
    _ -> EchoesOfThePast <$> liftRunMessage msg attrs
