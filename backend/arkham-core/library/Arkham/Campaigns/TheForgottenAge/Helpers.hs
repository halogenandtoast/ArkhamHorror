module Arkham.Campaigns.TheForgottenAge.Helpers where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Scenario
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenario.Types
import Arkham.Source
import Arkham.Target
import Arkham.Treachery.Cards qualified as Treacheries

getVengeanceInVictoryDisplay :: (HasGame m, Monad m) => m Int
getVengeanceInVictoryDisplay =
  sum
    . map (fromMaybe 0 . cdVictoryPoints . toCardDef)
    <$> scenarioField ScenarioVictoryDisplay

getExplorationDeck :: (HasGame m, Monad m) => m [Card]
getExplorationDeck = scenarioFieldMap
  ScenarioDecks
  (findWithDefault (error "missing deck") ExplorationDeck)

getIsPoisoned :: (HasGame m, Monad m) => InvestigatorId -> m Bool
getIsPoisoned iid =
  selectAny $ treacheryIs Treacheries.poisoned <> treacheryInThreatAreaOf iid

getSetAsidePoisoned :: (HasGame m, Monad m) => m Card
getSetAsidePoisoned =
  fromJustNote "not enough poison cards"
    . find ((== Treacheries.poisoned) . toCardDef)
    <$> scenarioField ScenarioSetAsideCards

explore :: InvestigatorId -> Source -> (Card -> Bool) -> GameT ()
explore iid source isValidMatch = do
  explorationDeck <- getExplorationDeck
  let
    (notMatched, matchedOnTop) = break isValidMatch explorationDeck
  case matchedOnTop of
    [] -> do
      deck' <- shuffleM notMatched
      pushAll
        [ FocusCards notMatched
        , chooseOne
          iid
          [ Label
              "No Matches Found"
              [UnfocusCards, SetScenarioDeck ExplorationDeck deck']
          ]
        ]
    (x : xs) -> do
      let
        msgs = if cdCardType (toCardDef x) == LocationType
          then
            [PlaceLocation x, MoveTo source iid (toLocationId x)]
          else [DrewTreachery iid x]
      deck' <- if null notMatched
        then pure xs
        else shuffleM (xs <> notMatched)
      pushAll
        [ FocusCards (notMatched <> [x])
        , chooseOne
          iid
          [ TargetLabel
              (CardIdTarget $ toCardId x)
              (UnfocusCards : SetScenarioDeck ExplorationDeck deck' : msgs)
          ]
        ]
