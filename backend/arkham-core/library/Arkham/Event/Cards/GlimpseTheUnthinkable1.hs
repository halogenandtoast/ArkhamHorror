module Arkham.Event.Cards.GlimpseTheUnthinkable1 (
  glimpseTheUnthinkable1,
  GlimpseTheUnthinkable1 (..),
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Deck
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message

newtype GlimpseTheUnthinkable1 = GlimpseTheUnthinkable1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

glimpseTheUnthinkable1 :: EventCard GlimpseTheUnthinkable1
glimpseTheUnthinkable1 =
  event GlimpseTheUnthinkable1 Cards.glimpseTheUnthinkable1

instance RunMessage GlimpseTheUnthinkable1 where
  runMessage msg e@(GlimpseTheUnthinkable1 attrs) = case msg of
    InvestigatorPlayEvent iid eid mtarget windows' _ | eid == toId attrs -> do
      drawing <- drawCards iid attrs 1
      pushAll
        [ drawing
        , ResolveEvent iid eid mtarget windows'
        ]
      pure e
    ResolveEvent iid eid _ _ | eid == toId attrs -> do
      cards <-
        selectList
          $ InHandOf (InvestigatorWithId iid)
          <> BasicCardMatch NonWeakness
      push
        $ chooseAmounts
          iid
          "Choose number of cards to discard"
          (MaxAmountTarget $ length cards)
          [("Number of cards to discard", (0, length cards))]
          (toTarget attrs)
      pure e
    ResolveAmounts iid choices (isTarget attrs -> True) -> do
      let
        n = getChoiceAmount "Number of cards to discard" choices
      cards <-
        selectList
          $ InHandOf (InvestigatorWithId iid)
          <> BasicCardMatch NonWeakness
      drawing <- drawCards iid attrs n
      pushAll
        [ chooseN
            iid
            n
            [ TargetLabel
              (CardIdTarget $ toCardId c)
              [ShuffleCardsIntoDeck (InvestigatorDeck iid) [c]]
            | c <- cards
            ]
        , drawing
        ]

      pure e
    _ -> GlimpseTheUnthinkable1 <$> runMessage msg attrs
