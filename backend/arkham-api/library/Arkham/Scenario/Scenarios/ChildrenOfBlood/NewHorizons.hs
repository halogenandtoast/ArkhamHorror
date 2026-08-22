module Arkham.Scenario.Scenarios.ChildrenOfBlood.NewHorizons (newHorizons) where

import Arkham.Act.CardDefs.ChildrenOfBlood.NewHorizons qualified as Acts
import Arkham.Agenda.CardDefs.ChildrenOfBlood.NewHorizons qualified as Agendas
import Arkham.Asset.Cards.ChildrenOfBlood qualified as Assets
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.CardDefs.ChildrenOfBlood.NewHorizons qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Id
import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.ChildrenOfBlood.NewHorizons.Helpers
import Arkham.Trait (Trait (Cave, Day, Night))
import Arkham.Treachery.CardDefs.ChildrenOfBlood.NewHorizons qualified as Treacheries

newtype NewHorizons = NewHorizons ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

newHorizons :: Difficulty -> NewHorizons
newHorizons difficulty =
  scenario
    NewHorizons
    "13031"
    "New Horizons"
    difficulty
    [ ".            star         star         triangle     triangle     ."
    , "square       square       t            t            moon         moon"
    , ".            .            hourglass    hourglass    .            ."
    , "sideChamber1 sideChamber1 sideChamber2 sideChamber2 sideChamber3 sideChamber3"
    , ".            .            diamond      diamond      .            ."
    ]

instance HasChaosTokenValue NewHorizons where
  getChaosTokenValue iid tokenFace (NewHorizons attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage NewHorizons where
  runMessage msg s@(NewHorizons attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      addChaosToken #cultist
      storyWithChooseOneM
        ( buildFlavor $ scope "intro" do
            setTitle "title"
            p "body"
            ul do
              li "addCultist"
              li.nested "chooseAsGroup" do
                li "searchDuringTheDay"
                li "searchAfterDark"
        )
        do
          labeled' "searchDuringTheDay" $ setScenarioMeta $ object ["searchAfterDark" .= False]
          labeled' "searchAfterDark" $ setScenarioMeta $ object ["searchAfterDark" .= True]
      pure s
    Setup -> do
      afterDark <- getScenarioMetaKeyDefault "searchAfterDark" False
      doStep (if afterDark then 2 else 1) Setup
      pure s
    DoStep 1 Setup -> runScenarioSetup NewHorizons attrs $ scope "version1" do
      setup' $ ul do
        li "gatherSets"
        li.nested "gatherLocations" do
          li "day"
          li "shallowTunnels"
          li "darkestDepths"
        li "agendaDeck"
        li "factoryWorkers"
        li.nested "zburamoarte" do
          li "lethargicBeast"
          li "sourceOfTheBlight"
          li "progenitorOfMonsters"
        li "setAside"
        li "removeNightWatchman"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      gather Set.NewHorizons
      gather Set.Afflicted
      gather Set.BloodBlight
      gather Set.Bloodthirst
      gather Set.Hunted
      gather Set.Infected
      gather Set.SanguineSecrets
      gather Set.Vermin
      gather Set.FlyingTerrors

      setAgendaDeck [Agendas.busyDay, Agendas.diggingDeeperV1]
      setActDeck [Acts.toNewHorizons, Acts.theSearchForAnswers, Acts.bringDownTheBeast]

      removeCards =<< amongGathered (#location <> CardWithTrait Night)
      placeAll [Locations.managersOfficeDay, Locations.loadingDockDay, Locations.storageDay]
      factoryFloors <- traverse place [Locations.factoryFloorWestDay, Locations.factoryFloorEastDay]
      startAtFactoryFloor attrs factoryFloors
      for_ factoryFloors $ enemyAt_ Enemies.factoryWorker

      setAsideTunnels attrs
      setAsideZburamoarte attrs
      setAside [Enemies.javierRivera]
      setAsideCommon
      removeEvery [Enemies.nightWatchman]
    DoStep 2 Setup -> runScenarioSetup NewHorizons attrs $ scope "version2" do
      setup' $ ul do
        li "gatherSets"
        li.nested "gatherLocations" do
          li "night"
          li "shallowTunnels"
          li "darkestDepths"
        li "agendaDeck"
        li.nested "zburamoarte" do
          li "lethargicBeast"
          li "sourceOfTheBlight"
          li "progenitorOfMonsters"
        li "setAside"
        li "removeJavierRivera"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      gather Set.NewHorizons
      gather Set.Afflicted
      gather Set.ChildrenOfBlood
      gather Set.Infected
      gather Set.PreyedUpon
      gather Set.SanguineSecrets
      gather Set.Stalked
      gather Set.FlyingTerrors
      gather Set.ReekingDecay

      setAgendaDeck [Agendas.quietNight, Agendas.diggingDeeperV2]
      setActDeck [Acts.toNewHorizons, Acts.theSearchForAnswers, Acts.bringDownTheBeast]

      removeCards =<< amongGathered (#location <> CardWithTrait Day)
      placeAll [Locations.managersOfficeNight, Locations.loadingDockNight, Locations.storageNight]
      factoryFloors <- traverse place [Locations.factoryFloorWestNight, Locations.factoryFloorEastNight]
      startAtFactoryFloor attrs factoryFloors

      setAsideTunnels attrs
      setAsideZburamoarte attrs
      setAside [Enemies.nightWatchman]
      setAsideCommon
      removeEvery [Enemies.javierRivera, Enemies.factoryWorker]
    _ -> NewHorizons <$> liftRunMessage msg attrs

startAtFactoryFloor :: ReverseQueue m => ScenarioAttrs -> [LocationId] -> ScenarioBuilderT m ()
startAtFactoryFloor attrs factoryFloors =
  eachInvestigator \iid -> chooseTargetM iid factoryFloors $ moveTo_ attrs iid

setAsideTunnels :: ReverseQueue m => ScenarioAttrs -> ScenarioBuilderT m ()
setAsideTunnels attrs = do
  setAside
    $ Locations.descendingTunnel
    : if isEasyStandard attrs
      then
        [ Locations.cavernEntranceShallowTunnels
        , Locations.hiddenLaboratoryShallowTunnels
        , Locations.lockedChamberShallowTunnels
        , Locations.secretChamberShallowTunnels
        ]
      else
        [ Locations.cavernEntranceDarkestDepths
        , Locations.hiddenLaboratoryDarkestDepths
        , Locations.lockedChamberDarkestDepths
        , Locations.secretChamberDarkestDepths
        ]
  removeCards =<< amongGathered (#location <> CardWithTrait Cave)

setAsideZburamoarte :: ReverseQueue m => ScenarioAttrs -> ScenarioBuilderT m ()
setAsideZburamoarte attrs = do
  setAside
    [ case attrs.difficulty of
        Easy -> Enemies.zburamoarteLethargicBeast
        Standard -> Enemies.zburamoarteTheSourceOfTheBlight
        _other -> Enemies.zburamoarteProgenitorOfMonsters
    ]
  removeCards =<< amongGathered (#enemy <> CardWithTitle "Zburamoarte")

setAsideCommon :: ReverseQueue m => ScenarioBuilderT m ()
setAsideCommon = do
  setAsideEvery $ cardIs Enemies.blightedWorker
  setAsideEvery $ cardIs Treacheries.echoingInDarkness
  setAsideEvery $ CardFromEncounterSet Set.Infected
  setAsideEvery $ CardFromEncounterSet Set.FlyingTerrors
  setAside [Assets.forgedPermit, Assets.sanguineSong]
