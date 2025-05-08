module Arkham.Scenario.Scenarios.ReturnToLostInTimeAndSpace (returnToLostInTimeAndSpace) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Campaigns.TheDunwichLegacy.Key
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.LostInTimeAndSpace
import Arkham.Scenarios.LostInTimeAndSpace.Helpers

newtype ReturnToLostInTimeAndSpace = ReturnToLostInTimeAndSpace LostInTimeAndSpace
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToLostInTimeAndSpace :: Difficulty -> ReturnToLostInTimeAndSpace
returnToLostInTimeAndSpace difficulty =
  scenario
    (ReturnToLostInTimeAndSpace . LostInTimeAndSpace)
    "51053"
    "Return to Lost in Time and Space"
    difficulty
    scenarioLayout

instance RunMessage ReturnToLostInTimeAndSpace where
  runMessage msg (ReturnToLostInTimeAndSpace lostInTimeAndSpace'@(LostInTimeAndSpace attrs)) = runQueueT $ scenarioI18n $ case msg of
    Setup -> runScenarioSetup (ReturnToLostInTimeAndSpace . LostInTimeAndSpace) attrs do
      setup do
        ul do
          li "gatherSets"
          li "placeLocations"
          li "setAside"
          unscoped $ li "shuffleRemainder"

      scope "locationsInTheEncounterDeck" $ flavor do
        setTitle "title"
        p "body"

      gather Set.ReturnToLostInTimeAndSpace
      gather Set.LostInTimeAndSpace
      gather Set.Sorcery
      gather Set.BeyondTheThreshold
      gather Set.HideousAbominations
      gather Set.YogSothothsEmissaries

      startAt =<< place Locations.anotherDimension
      enemyAt_ Enemies.yogSothoth =<< place Locations.realmsBeyondAllInOne

      setAside
        [ Locations.theEdgeOfTheUniverse
        , Locations.tearThroughTime
        , Enemies.sethBishopThrallOfYogSothoth
        ]

      ritualWasCompleted <- getHasRecord TheRitualWasCompleted

      setActDeck
        [ Acts.outOfThisWorld
        , if ritualWasCompleted then Acts.intoTheBeyondV2 else Acts.intoTheBeyond
        , Acts.closeTheRift
        , Acts.findingANewWay
        ]
      setAgendaDeck
        [ Agendas.allIsOne
        , Agendas.pastPresentAndFuture
        , Agendas.breakingThroughV2
        , Agendas.theEndOfAllThings
        ]
    _ -> ReturnToLostInTimeAndSpace <$> liftRunMessage msg lostInTimeAndSpace'
