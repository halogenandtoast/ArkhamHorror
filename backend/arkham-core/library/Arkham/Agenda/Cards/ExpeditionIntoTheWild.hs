module Arkham.Agenda.Cards.ExpeditionIntoTheWild
  ( ExpeditionIntoTheWild(..)
  , expeditionIntoTheWild
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Helpers.Scenario
import Arkham.Location.Types
import Arkham.Message
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Scenario.Types
import Arkham.Target

newtype ExpeditionIntoTheWild = ExpeditionIntoTheWild AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

expeditionIntoTheWild :: AgendaCard ExpeditionIntoTheWild
expeditionIntoTheWild =
  agenda (1, A) ExpeditionIntoTheWild Cards.expeditionIntoTheWild (Static 6)

instance HasAbilities ExpeditionIntoTheWild where
  getAbilities (ExpeditionIntoTheWild a) =
    [ restrictedAbility a 1 (ScenarioDeckWithCard ExplorationDeck)
        $ ActionAbility (Just Action.Explore)
        $ ActionCost 1
    ]

instance RunMessage ExpeditionIntoTheWild where
  runMessage msg a@(ExpeditionIntoTheWild attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      explorationDeck <- scenarioFieldMap
        ScenarioDecks
        (findWithDefault (error "missing deck") ExplorationDeck)
      locationSymbols <-
        fieldMap LocationCard (cdLocationRevealedConnections . toCardDef)
          =<< getJustLocation iid
      let
        isValidMatch c = case cdLocationRevealedSymbol (toCardDef c) of
          Nothing -> cdCardType (toCardDef c) == TreacheryType
          Just sym -> sym `elem` locationSymbols
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
                [PlaceLocation x, MoveTo (toSource attrs) iid (toLocationId x)]
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
      pure a
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      a <$ pushAll [ShuffleEncounterDiscardBackIn, AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
    _ -> ExpeditionIntoTheWild <$> runMessage msg attrs
