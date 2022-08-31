module Arkham.Location.Cards.Entryway
  ( entryway
  , Entryway(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Deck qualified as Deck
import Arkham.Direction
import Arkham.GameValue
import Arkham.Id
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenario.Deck
import Arkham.Target

newtype Entryway = Entryway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

entryway :: LocationCard Entryway
entryway = locationWith
  Entryway
  Cards.entryway
  2
  (PerPlayer 1)
  (connectsToL .~ singleton LeftOf)

instance HasAbilities Entryway where
  getAbilities (Entryway attrs) = withResignAction
    attrs
    [ restrictedAbility
        attrs
        1
        (Here <> HasSupply Torches)
        (ActionAbility Nothing $ ActionCost 1)
    | locationRevealed attrs
    ]

handleTreacheries :: InvestigatorId -> [EncounterCard] -> Message
handleTreacheries iid [] = chooseOne iid [Label "No Treacheries Found" []]
handleTreacheries iid treacheries = chooseOneAtATime
  iid
  [ TargetLabel (CardIdTarget $ toCardId c) [AddToEncounterDiscard c]
  | c <- treacheries
  ]

instance RunMessage Entryway where
  runMessage msg l@(Entryway attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      explorationDeck <- getExplorationDeck
      let
        deckKey = Deck.ScenarioDeckByKey ExplorationDeck
        (viewing, rest) = splitAt 2 explorationDeck
        (treacheries, other) =
          partition (`cardMatch` CardWithType TreacheryType) viewing
      pushAll
        $ [ FocusCards viewing
          , SetScenarioDeck ExplorationDeck $ other <> rest
          , handleTreacheries
            iid
            (mapMaybe (preview _EncounterCard) treacheries)
          ]
        <> [ PutCardOnBottomOfDeck iid deckKey c | c <- other ]
        <> [UnfocusCards, ShuffleDeck deckKey]
      pure l
    _ -> Entryway <$> runMessage msg attrs
