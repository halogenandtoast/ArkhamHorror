module Arkham.Scenario.Scenarios.SanguineShadows (sanguineShadows) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.ChaosToken
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWith)
import Arkham.Helpers.Query (getLead)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Modifier (setActiveDuringSetup)
import Arkham.Scenario.Import.Lifted
import Arkham.ScenarioLogKey
import Arkham.Scenarios.SanguineShadows.Helpers
import Arkham.Token qualified as Token
import Arkham.Trait (Trait (Criminal))

newtype SanguineShadows = SanguineShadows ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor SanguineShadows where
  getModifiersFor (SanguineShadows attrs) = do
    modifySelectWith
      attrs
      (not_ $ LocationWithToken Token.Target)
      setActiveDuringSetup
      [ScenarioModifier "noConcealed[LaChicaRoja]"]

sanguineShadows :: Difficulty -> SanguineShadows
sanguineShadows difficulty =
  scenario
    SanguineShadows
    "09545"
    "Sanguine Shadows"
    difficulty
    [ ". . b b c c . ."
    , ". . b b c c . ."
    , ". . b b c c . ."
    , "h h . . . . d d"
    , "h h . a a . d d"
    , "h h . a a . d d"
    , "g g . a a . e e"
    , "g g . . . . e e"
    , "g g . f f . e e"
    , ". . . f f . . ."
    , ". . . f f . . ."
    ]

instance HasChaosTokenValue SanguineShadows where
  getChaosTokenValue iid tokenFace (SanguineShadows attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage SanguineShadows where
  runMessage msg s@(SanguineShadows attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      storyWithChooseOneM' (setTitle "title" >> p "intro1") do
        labeled' "everything" $ doStep 2 PreScenarioSetup
        labeled' "breadcrumbs" $ doStep 3 PreScenarioSetup
        labeled' "insist" $ doStep 4 PreScenarioSetup
      pure s
    DoStep 2 PreScenarioSetup -> scope "intro" do
      criminal <- selectAny $ InvestigatorWithTrait Criminal
      flavor $ setTitle "title" >> p "intro2Part1" >> p.validate criminal "criminal" >> p "intro2Part2"
      remember MatiasBolivarTrustsYou
      removeChaosToken ElderThing
      pure s
    DoStep 3 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro3"
      pure s
    DoStep 4 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro4"
      remember MatiasBolivarDoesntTrustYou
      removeChaosToken Tablet
      pure s
    Setup -> runScenarioSetup SanguineShadows attrs do
      setup $ ul do
        li "gatherSets"
        li "decks"
        li.nested "placeStart" do
          li "beginPlay"
        li "placeRemaining"
        li.nested "placeTargets" do
          li "targets"
        li "miniCards"
        li "laChicaRoja"
        li "setAside"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"
      gather Set.SanguineShadows
      gather Set.DarkVeiling
      gather Set.MysteriesAbound
      gather Set.ShadowOfADoubt
      gather Set.StrangeHappenings
      gather Set.LockedDoors
      gather Set.Nightgaunts

      setAgendaDeck [Agendas.whereIsShe]
      setActDeck [Acts.theScarletShadow, Acts.inTheSearchlight]

      startAt =<< place Locations.avenidaDeMayo
      otherLocations <-
        placeAllCapture
          . drop 1
          =<< shuffle
            [ Locations.casaRosada
            , Locations.catedralMetropolitana
            , Locations.cementarioDeLaRecoleta
            , Locations.palacioErrazuriz
            , Locations.theCabildo
            , Locations.bancoDeLaProvincia
            , Locations.teatroColon
            ]

      for_ otherLocations $ placeTokensOn ScenarioSource Token.Target 1

      lead <- getLead
      laChicaRoja <- fetchCard Enemies.laChicaRojaTheGirlInTheCarmineCoat
      drawCard lead laChicaRoja
    _ -> SanguineShadows <$> liftRunMessage msg attrs
