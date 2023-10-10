module Arkham.Event.Cards.GlimpseTheUnthinkable5 (
  glimpseTheUnthinkable5,
  GlimpseTheUnthinkable5 (..),
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype GlimpseTheUnthinkable5 = GlimpseTheUnthinkable5 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

glimpseTheUnthinkable5 :: EventCard GlimpseTheUnthinkable5
glimpseTheUnthinkable5 =
  eventWith GlimpseTheUnthinkable5 Cards.glimpseTheUnthinkable5 (afterPlayL .~ RemoveThisFromGame)

instance RunMessage GlimpseTheUnthinkable5 where
  runMessage msg e@(GlimpseTheUnthinkable5 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      cards <-
        selectList
          $ InHandOf (InvestigatorWithId iid)
          <> BasicCardMatch NonWeakness
      player <- getPlayer iid
      push
        $ chooseAmounts
          player
          "Choose number of cards to discard"
          (MaxAmountTarget $ length cards)
          [("Number of cards to discard", (0, length cards))]
          (toTarget attrs)
      pure e
    ResolveAmounts iid choices (isTarget attrs -> True) -> do
      let
        n = getChoiceAmount "Number of cards to discard" choices
      handLimit <- field InvestigatorHandSize iid
      handCards <- fieldMap InvestigatorHand length iid

      let toDraw = handLimit - (handCards - n)

      cards <-
        selectList
          $ InHandOf (InvestigatorWithId iid)
          <> BasicCardMatch NonWeakness
      drawing <- drawCards iid attrs toDraw
      player <- getPlayer iid
      pushAll
        [ chooseN
            player
            n
            [ TargetLabel
              (CardIdTarget $ toCardId c)
              [ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [c]]
            | c <- cards
            ]
        , drawing
        ]

      pure e
    _ -> GlimpseTheUnthinkable5 <$> runMessage msg attrs
