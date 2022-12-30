module Arkham.Investigator.Cards.JoeDiamond
  ( joeDiamond
  , JoeDiamond(..)
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Deck
import Arkham.Helpers
import Arkham.Helpers.Deck
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Target

newtype Metadata = Metadata { hunchDeck :: [PlayerCard] }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype JoeDiamond = JoeDiamond (InvestigatorAttrs `With` Metadata)
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

joeDiamond :: InvestigatorCard JoeDiamond
joeDiamond = investigator
  (JoeDiamond . (`with` Metadata []))
  Cards.joeDiamond
  Stats
    { health = 8
    , sanity = 6
    , willpower = 2
    , intellect = 4
    , combat = 4
    , agility = 2
    }

instance HasAbilities JoeDiamond where
  getAbilities (JoeDiamond _) = []

instance HasTokenValue JoeDiamond where
  getTokenValue iid ElderSign (JoeDiamond (attrs `With` _))
    | iid == toId attrs = do
      pure $ TokenValue ElderSign NoModifier
  getTokenValue _ token _ = pure $ TokenValue token mempty

instance RunMessage JoeDiamond where
  runMessage msg i@(JoeDiamond (attrs `With` meta)) = case msg of
    Setup -> do
      let
        insights = filter
          (`cardMatch` (CardWithTrait Insight <> CardWithType EventType))
          (unDeck $ investigatorDeck attrs)
      if length insights == 11
        then do
          hunchDeck <- shuffleM insights
          pure
            $ JoeDiamond
            . (`with` Metadata hunchDeck)
            $ attrs
            & deckL
            %~ withDeck (filter (`notElem` insights))
        else do
          let
            unsolvedCase = fromJustNote "Deck missing unsolved case"
              $ find (`cardMatch` (cardIs Events.unsolvedCase)) insights
          pushAll
            [ FocusCards $ map PlayerCard insights
            , ShuffleCardsIntoDeck HunchDeck [PlayerCard unsolvedCase]
            , chooseN
              (toId attrs)
              10
              [ TargetLabel
                  (CardIdTarget $ toCardId insight)
                  [ShuffleCardsIntoDeck HunchDeck [PlayerCard insight]]
              | insight <- insights
              ]
            , UnfocusCards
            ]
          pure i

    ShuffleCardsIntoDeck HunchDeck [PlayerCard insight] -> do
      hunchDeck <- shuffleM (insight : hunchDeck meta)
      pure
        $ JoeDiamond
        . (`with` Metadata hunchDeck)
        $ attrs
        & deckL
        %~ withDeck (filter (/= insight))
    _ -> JoeDiamond . (`with` meta) <$> runMessage msg attrs
