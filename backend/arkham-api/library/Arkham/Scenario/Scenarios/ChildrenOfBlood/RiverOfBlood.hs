module Arkham.Scenario.Scenarios.ChildrenOfBlood.RiverOfBlood (riverOfBlood) where

import Arkham.Act.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Acts
import Arkham.Agenda.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Agendas
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.CardDefs.ChildrenOfBlood.AgentsOfZburamoarte qualified as Enemies
import Arkham.Enemy.CardDefs.ChildrenOfBlood.PreyedUpon qualified as Enemies
import Arkham.Enemy.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Enemies
import Arkham.Helpers.Agenda
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Locations
import Arkham.Matcher
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.ChildrenOfBlood.RiverOfBlood.Helpers
import Arkham.Story.CardDefs.ChildrenOfBlood qualified as Stories
import Arkham.Token qualified as Token
import Arkham.Trait (Trait (Dawn, Dusk, Lair))

newtype RiverOfBlood = RiverOfBlood ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riverOfBlood :: Difficulty -> RiverOfBlood
riverOfBlood difficulty =
  scenario
    RiverOfBlood
    "13001"
    "River of Blood"
    difficulty
    [ ".             heart     ."
    , "square        heart     triangle"
    , "square        .         triangle"
    , "unvisitedIsle circle    ."
    , "unvisitedIsle circle    ."
    , "t             .         moon"
    , "t             hourglass moon"
    , ".             hourglass ."
    ]

instance HasChaosTokenValue RiverOfBlood where
  getChaosTokenValue iid tokenFace (RiverOfBlood attrs) = case tokenFace of
    Skull -> do
      n <- getCurrentAgendaStep
      pure $ toChaosTokenValue attrs Skull n (n + 1)
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage RiverOfBlood where
  runMessage msg s@(RiverOfBlood attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "intro"
      pure s
    Setup -> runScenarioSetup RiverOfBlood attrs do
      setup $ ul do
        li "gatherSets"
        li "gatherAdditionalSets"
        li.nested "placeLocations" do
          li "dawn"
          li "dusk"
          li "startAt"
        li.nested "waterfrontCivilians" do
          li "additionalWaterfrontCivilians"
        li.nested "juliaStern" do
          li "onTheRun"
          li "stalkingTheStreets"
          li "preyingUponArkham"
        li "setOutOfPlay"
        unscoped $ li "shuffleRemainder"
        li "lairs"
        unscoped $ li "readyToBegin"

      n <- getPlayerCount
      gather Set.RiverOfBlood
      gather Set.BloodBlight
      gather Set.BloodMoon
      gather Set.ChildrenOfBlood
      gather Set.Hunted
      gather Set.Infected
      gather Set.Misinformation
      if isEasyStandard attrs
        then do
          gather Set.PreyedUpon
          gather Set.Vermin
        else do
          gather Set.AgentsOfZburamoarte
          gather Set.Mongrels
      gather Set.DeadEnds
      gather Set.FlyingTerrors
      when (n >= 3) $ gather Set.Afflicted

      setAgendaDeck
        [Agendas.theFirstDay, Agendas.theFirstNight, Agendas.theSecondDay, Agendas.theFinalNight]
      setActDeck [Acts.locateTheLair, Acts.cornerTheSuspect]

      if isEasyStandard attrs
        then do
          removeCards =<< amongGathered (#location <> CardWithTrait Dusk)
          startAt =<< place Locations.waterStreetDawn
          mainStreet <- place Locations.mainStreetDawn
          garrisonStreet <- place Locations.garrisonStreetDawn
          enemyAt_ Enemies.waterfrontCivilian mainStreet
          when (n >= 3) $ enemyAt_ Enemies.waterfrontCivilian garrisonStreet
          placeAll
            [ Locations.riverDocksDawn
            , Locations.erwinBridgeDawn
            , Locations.unvisitedIsleDawn
            , Locations.waterfrontWarehouseDawn
            , Locations.backAlleyDawn
            ]
          setAsideEvery (cardIs Enemies.nightFeeder)
        else do
          removeCards =<< amongGathered (#location <> CardWithTrait Dawn)
          startAt =<< place Locations.waterStreetDusk
          mainStreet <- place Locations.mainStreetDusk
          garrisonStreet <- place Locations.garrisonStreetDusk
          enemyAt_ Enemies.waterfrontCivilian mainStreet
          when (n >= 3) $ enemyAt_ Enemies.waterfrontCivilian garrisonStreet
          placeAll
            [ Locations.riverDocksDusk
            , Locations.erwinBridgeDusk
            , Locations.unvisitedIsleDusk
            , Locations.waterfrontWarehouseDusk
            , Locations.backAlleyDusk
            ]
          setAsideEvery (cardIs Enemies.spawnOfZburamoarte)

      setAside
        [ case attrs.difficulty of
            Easy -> Enemies.juliaSternOnTheRun
            Standard -> Enemies.juliaSternStalkingTheStreets
            _other -> Enemies.juliaSternPreyingUponArkham
        ]
      removeCards =<< amongGathered (#enemy <> CardWithTitle "Julia Stern")
      setAside =<< amongGathered (cardIs Enemies.waterfrontCivilian)
      placeStory Stories.bloodToken
      doStep 1 Setup
    DoStep 1 Setup -> do
      xs <- map toCard . toList . take 2 <$> getEncounterDeck
      julia <- fetchCard $ case attrs.difficulty of
        Easy -> Enemies.juliaSternOnTheRun
        Standard -> Enemies.juliaSternStalkingTheStreets
        _other -> Enemies.juliaSternPreyingUponArkham
      xs' <- shuffle $ julia : xs
      traverse_ obtainCard xs'
      lairs <- select $ LocationWithTrait Lair
      for_ (zip lairs xs') \(lair, x) -> placeUnderneath lair (only x)
      pure s
    ResolveChaosToken _ Tablet iid -> do
      when (isHardExpert attrs) do
        whenMatch iid (InvestigatorAt $ LocationWithTrait Lair) $ assignHorror iid Tablet 1
      pure s
    ResolveChaosToken _ ElderThing iid | isHardExpert attrs -> do
      whenAny (SealedOnInvestigator (InvestigatorWithId iid) #blood) do
        afterSkillTestQuiet $ doStep 1 msg
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ | isEasyStandard attrs -> do
      case token.face of
        Tablet -> whenMatch iid (InvestigatorAt $ LocationWithTrait Lair) do
          assignHorror iid Tablet 1
        ElderThing -> whenAny (SealedOnInvestigator (InvestigatorWithId iid) #blood) do
          afterSkillTestQuiet $ doStep 1 msg
        _ -> pure ()
      pure s
    DoStep 1 (FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _) | token.face == ElderThing && isEasyStandard attrs -> do
      mtkn <- selectOne $ SealedOnInvestigator (InvestigatorWithId iid) #blood
      for_ mtkn \tkn -> do
        unsealChaosToken tkn
        assignHorror iid Tablet 1
      pure s
    DoStep 1 (ResolveChaosToken _ ElderThing iid) -> do
      mtkn <- selectOne $ SealedOnInvestigator (InvestigatorWithId iid) #blood
      for_ mtkn \tkn -> do
        unsealChaosToken tkn
        assignHorror iid Tablet 1
      pure s
    ScenarioSpecific "placeSnare" _ -> do
      withMatch (EnemyWithTitle "Julia Stern") $ placeTokensOn ScenarioSource Token.Snare 1
      pure s
    _ -> RiverOfBlood <$> liftRunMessage msg attrs
