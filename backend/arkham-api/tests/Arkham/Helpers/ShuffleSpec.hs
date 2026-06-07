module Arkham.Helpers.ShuffleSpec (spec) where

import Arkham.Deck qualified as Deck
import Arkham.Event.Cards qualified as Events
import Arkham.Helpers.Scenario (scenarioFieldMap)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted qualified as Lifted
import Arkham.Scenario.Deck
import Arkham.Scenario.Types (Field (..))
import Data.Map.Strict qualified as Map
import TestImport.New

spec :: Spec
spec = describe "shuffleCardsIntoDeck" do
  it "matches investigators who can shuffle into an empty deck" . gameTest $ \self -> do
    selectAny (InvestigatorWithId self.id <> CanShuffleIn) `shouldReturn` True

  it "can shuffle a single card into an empty scenario deck" . gameTest $ \_ -> do
    chamberOfTime <- genCard Locations.chamberOfTime

    Lifted.runQueueT $ Lifted.shuffleCardsIntoDeck ExplorationDeck (only chamberOfTime)
    runMessages

    scenarioFieldMap ScenarioDecks (Map.lookup ExplorationDeck) `shouldReturn` Just [chamberOfTime]

  it "does not shuffle an empty card list into a scenario deck" . gameTest $ \_ -> do
    Lifted.runQueueT $ Lifted.shuffleCardsIntoDeck ExplorationDeck ([] :: [Card])
    runMessages

    scenarioFieldMap ScenarioDecks (Map.lookup ExplorationDeck) `shouldReturn` Nothing

  it "queues shuffling an empty investigator deck" . gameTest $ \self -> do
    messages <- Lifted.capture $ Lifted.shuffleDeck self.id

    messages `shouldBe` [ShuffleDeck (Deck.InvestigatorDeck self.id)]

  it "queues shuffling cards into the top of an empty investigator deck" . gameTest $ \self -> do
    emergencyCache <- genCard Events.emergencyCache

    messages <- Lifted.capture $ Lifted.shuffleCardsIntoTopOfDeck self.id 1 [emergencyCache]

    messages `shouldBe` [ShuffleCardsIntoTopOfDeck (Deck.InvestigatorDeck self.id) 1 [emergencyCache]]
