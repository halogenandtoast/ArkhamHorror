module Arkham.Scenario.Scenarios.FateOfTheVale (fateOfTheVale) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (connectBothWays)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (enemyAt)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.FateOfTheVale.Helpers
import Arkham.Story.Cards qualified as Stories

newtype FateOfTheVale = FateOfTheVale ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fateOfTheVale :: Difficulty -> FateOfTheVale
fateOfTheVale difficulty = scenario FateOfTheVale "10651" "Fate of the Vale" difficulty []

instance HasChaosTokenValue FateOfTheVale where
  getChaosTokenValue iid tokenFace (FateOfTheVale attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage FateOfTheVale where
  runMessage msg s@(FateOfTheVale attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "body"
      pure s
    Setup -> runScenarioSetup FateOfTheVale attrs do
      setup $ ul do
        li "gatherSets"
        li "nightThree"
        li "shatteredSelf"
        li.nested "mirrorNests" do
          li "shuffleMirrorNests"
          li "connections"
          li "startingLocation"
        li.nested "setAside" do
          li "removeScenarioReference"
        li.nested "createTheAbyss" do
          li "seedAbyssDeck"
          li "shuffleTrueSelves"
          li "placeTheAbyss"
        unscoped $ li "readyToBegin"

      scope "theAbyss" $ flavor $ setTitle "title" >> p "body"
      scope "cosmicEmissary" $ flavor $ setTitle "title" >> p "body"
      scope "shatteredSelf" $ flavor $ setTitle "title" >> p "body"

      gather Set.TheFinalDay
      gather Set.FateOfTheVale
      gather Set.AgentsOfTheColour
      gather Set.Refractions
      gather Set.Transfiguration
      gather Set.TheVale
      gather Set.HorrorsInTheRock

      gatherAndSetAside Set.DayOfTheFeast
      gatherAndSetAside Set.Residents
      gatherAndSetAside Set.Fire

      placeStory Stories.nightThree
      setScenarioDayAndTime

      setAgendaDeck [Agendas.theSilence, Agendas.theMiasma, Agendas.theSpiral]
      setActDeck [Acts.shatteredMemories, Acts.lostSelf]

      -- Replace each player's investigator card with a Shattered Self investigator card
      eachInvestigator $ push . BecomeShatteredSelf

      -- Shuffle the 4 Mirror Nest locations and put them into play, unrevealed
      mirrorNests <-
        shuffle
          [ Locations.mirrorNest10666
          , Locations.mirrorNest10667
          , Locations.mirrorNest10668
          , Locations.mirrorNest10669
          ]
      nestLids <- traverse place mirrorNests

      -- Each Mirror Nest is connected to the location clockwise and counter-clockwise from it
      for_ (zip nestLids (drop 1 nestLids <> take 1 nestLids)) (uncurry connectBothWays)

      -- Place a Cosmic Emissary enemy (Colour side faceup) bordering each Mirror Nest
      let emissaries =
            [ Enemies.cosmicEmissaryTheAbyss
            , Enemies.cosmicEmissaryThePhantasm
            , Enemies.cosmicEmissaryTheMiasma
            , Enemies.cosmicEmissaryTheBrilliance
            ]
      for_ (zip emissaries nestLids) \(emissary, lid) -> void $ enemyAt emissary lid

      -- Each investigator begins play at a different Mirror Nest location of their choice
      eachInvestigator \iid -> chooseTargetM iid nestLids $ moveTo_ attrs iid

      -- Set aside, out of play
      setAside
        [ Acts.fateOfTheValeV1
        , Acts.fateOfTheValeV2
        , Acts.fateOfTheValeV3
        , Acts.fateOfTheValeV4
        ]
      setAsideEvery (CardFromEncounterSet Set.TheVale <> #location)
      setAsideEvery (CardFromEncounterSet Set.HorrorsInTheRock <> #location)
      setAsideEvery (cardIs Enemies.crystalParasite)
    _ -> FateOfTheVale <$> liftRunMessage msg attrs
