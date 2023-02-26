module Arkham.Helpers.Deck where

import Arkham.Prelude

import Arkham.Deck qualified as Deck
import Arkham.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Projection
import Arkham.Card
import Arkham.Helpers.Scenario
import Data.HashMap.Strict qualified as HashMap
import Arkham.Investigator.Types (Field(..))
import Arkham.Scenario.Types (Field(..))

withDeck :: ([a] -> [a]) -> Deck a -> Deck a
withDeck f (Deck xs) = Deck (f xs)

withDeckM :: Functor f => ([a] -> f [a]) -> Deck a -> f (Deck a)
withDeckM f (Deck xs) = Deck <$> f xs

removeEachFromDeck :: (HasCardDef a, HasCardDef b) => Deck a -> [b] -> Deck a
removeEachFromDeck deck removals = flip withDeck deck $ \cards ->
  foldl' (\cs m -> deleteFirstMatch ((== toCardDef m) . toCardDef) cs) cards removals

getDeck :: HasGame m => Deck.DeckSignifier -> m [Card]
getDeck = \case
  Deck.InvestigatorDeck iid -> fieldMap InvestigatorDeck (map PlayerCard . unDeck) iid
  Deck.InvestigatorDiscard iid -> fieldMap InvestigatorDiscard (map PlayerCard) iid
  Deck.EncounterDeck -> scenarioFieldMap ScenarioEncounterDeck (map EncounterCard . unDeck)
  Deck.EncounterDiscard -> scenarioFieldMap ScenarioDiscard (map EncounterCard)
  Deck.ScenarioDeckByKey k -> scenarioFieldMap ScenarioDecks (HashMap.findWithDefault [] k)
  Deck.InvestigatorDeckByKey iid k -> fieldMap InvestigatorDecks (HashMap.findWithDefault [] k) iid

