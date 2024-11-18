module Arkham.Scenario.Scenarios.IceAndDeath (IceAndDeath (..), iceAndDeath) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.IceAndDeath.Helpers
import Arkham.Trait (Trait (Uncharted))

newtype IceAndDeath = IceAndDeath ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

iceAndDeath :: Difficulty -> IceAndDeath
iceAndDeath difficulty =
  scenario
    IceAndDeath
    "08501"
    "Ice and Death"
    difficulty
    [ "trefoil  .       .     .         .        .         plus"
    , "trefoil  .       .     moon      .        .         plus"
    , ".        droplet .     moon      .        equals    ."
    , ".        droplet .     .         .        equals    ."
    , ".        .       heart .         triangle .         ."
    , ".        .       heart .         triangle .         ."
    , ".        .       .     circle    .        .         ."
    , ".        star    .     circle    .        hourglass ."
    , ".        star    .     diamond   .        hourglass ."
    , ".        .       .     diamond   .        .         ."
    , ".        .       .     square    .        .         ."
    , ".        .       .     square    .        .         ."
    , ".        .       .     squiggle  .        .         ."
    , ".        .       .     squiggle  .        .         ."
    ]

instance HasChaosTokenValue IceAndDeath where
  getChaosTokenValue iid tokenFace (IceAndDeath attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage IceAndDeath where
  runMessage msg s@(IceAndDeath attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "iceAndDeath"
      doStep 1 PreScenarioSetup
      pure s
    DoStep 1 PreScenarioSetup -> do
      story $ i18nWithTitle "iceAndDeathPart1Intro1"

      selectAny (investigatorIs Investigators.winifredHabbamock) >>= \case
        True -> doStep 2 PreScenarioSetup
        False -> doStep 3 PreScenarioSetup

      pure s
    DoStep 2 PreScenarioSetup -> do
      story $ i18nWithTitle "iceAndDeathPart1Intro2"
      doStep 4 PreScenarioSetup
      pure s
    DoStep 3 PreScenarioSetup -> do
      story $ i18nWithTitle "iceAndDeathPart1Intro3"
      doStep 4 PreScenarioSetup
      pure s
    DoStep 4 PreScenarioSetup -> do
      story $ i18nWithTitle "iceAndDeathPart1Intro4"
      killed <- sample expeditionTeam
      crossOutRecordSetEntries ExpeditionTeam [killed.cardCode]
      recordSetInsert WasKilledInThePlaneCrash [killed.cardCode]
      if
        | killed == Assets.professorWilliamDyerProfessorOfGeology ->
            story $ i18n "williamDyerKilledInPlaneCrash"
        | killed == Assets.roaldEllsworthIntrepidExplorer -> story $ i18n "roaldEllsworthKilledInPlaneCrash"
        | killed == Assets.eliyahAshevakDogHandler -> story $ i18n "eliyahAshevakKilledInPlaneCrash"
        | killed == Assets.danforthBrilliantStudent -> story $ i18n "danforthKilledInPlaneCrash"
        | killed == Assets.jamesCookieFredericksDubiousChoice ->
            story $ i18n "jamesFredericksKilledInPlaneCrash"
        | killed == Assets.averyClaypoolAntarcticGuide -> story $ i18n "averyClaypoolKilledInPlaneCrash"
        | killed == Assets.takadaHirokoAeroplaneMechanic -> story $ i18n "takadaHirokoKilledInPlaneCrash"
        | killed == Assets.drMalaSinhaDaringPhysician -> story $ i18n "malaSinhaKilledInPlaneCrash"
        | killed == Assets.drAmyKenslerProfessorOfBiology -> story $ i18n "amyKenslerKilledInPlaneCrash"
        | otherwise -> error "Invalid card in expedition team"

      pure s
    Setup -> runScenarioSetup IceAndDeath attrs do
      gather Set.IceAndDeath
      gather Set.TheCrash
      gather Set.DeadlyWeather
      gather Set.HazardsOfAntarctica
      gather Set.SilenceAndMystery
      gather Set.Tekelili
      gather Set.AncientEvils

      gatherAndSetAside Set.CreaturesInTheIce

      placeAll [Locations.precariousIceSheet, Locations.treacherousPath, Locations.frozenShores]
      startAt =<< place Locations.crashSite

      setAside
        =<< amongGathered
          ( oneOf
              [ #location <> CardWithTrait Uncharted
              , cardIs Enemies.skitteringNonsense
              , cardIs Enemies.terrorOfTheStarsBringerOfIceAndDeath
              ]
          )

      setAgendaDeck [Agendas.coldWelcome, Agendas.intoTheWhite, Agendas.runningOutOfTime]
      setActDeck [Acts.searchForACampSite]

      addExtraDeck TekeliliDeck =<< shuffle =<< amongGathered (CardWithTitle "Tekeli-li")

      case attrs.difficulty of
        Expert -> placeDoomOnAgenda 2
        Hard -> placeDoomOnAgenda 1
        _ -> pure ()
    _ -> IceAndDeath <$> liftRunMessage msg attrs
