module Arkham.Campaigns.TheForgottenAge.Helpers where

import Arkham.Prelude

import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.History
import Arkham.Id
import Arkham.Investigator.Types
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Scenario.Types
import Arkham.Source
import Arkham.Target
import qualified Arkham.Timing as Timing
import qualified Arkham.Treachery.Cards as Treacheries
import Arkham.Window (Result(..), Window(..))
import qualified Arkham.Window as Window

getHasSupply :: (HasGame m, Monad m) => InvestigatorId -> Supply -> m Bool
getHasSupply iid s = (> 0) <$> getSupplyCount iid s

getSupplyCount :: (HasGame m, Monad m) => InvestigatorId -> Supply -> m Int
getSupplyCount iid s = fieldMap InvestigatorSupplies (length . filter (== s)) iid

getInvestigatorsWithSupply :: (HasGame m, Monad m) => Supply -> m [InvestigatorId]
getInvestigatorsWithSupply s = getInvestigatorIds >>= filterM (`getHasSupply` s)

getInvestigatorsWithoutSupply :: (HasGame m, Monad m) => Supply -> m [InvestigatorId]
getInvestigatorsWithoutSupply s = getInvestigatorIds >>= filterM (fmap not . (`getHasSupply` s))

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

explore :: InvestigatorId -> Source -> CardMatcher -> GameT ()
explore iid source cardMatcher = do
  explorationDeck <- getExplorationDeck
  let
    cardMatcher' = CardWithOneOf [CardWithType TreacheryType, cardMatcher]
    (notMatched, matchedOnTop) =
      break (`cardMatch` cardMatcher') explorationDeck
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
      msgs <- if cdCardType (toCardDef x) == LocationType
        then do
          let historyItem = mempty { historySuccessfulExplore = True }
          windowMsg <- checkWindows
            [Window Timing.After $ Window.Explored iid Success]
          pure
            [ PlaceLocation x
            , MoveTo source iid (toLocationId x)
            , UpdateHistory iid historyItem
            , windowMsg
            ]
        else do
          windowMsg <- checkWindows
            [Window Timing.After $ Window.Explored iid Failure]
          pure [DrewTreachery iid x, windowMsg]
      deck' <- if null notMatched then pure xs else shuffleM (xs <> notMatched)
      pushAll
        [ FocusCards (notMatched <> [x])
        , chooseOne
          iid
          [ TargetLabel
              (CardIdTarget $ toCardId x)
              (UnfocusCards : SetScenarioDeck ExplorationDeck deck' : msgs)
          ]
        ]
