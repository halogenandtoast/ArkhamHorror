module Arkham.Event.Cards.ParallelFates2 (
  parallelFates2,
  ParallelFates2 (..),
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype Metadata = Metadata {drawnCards :: [EncounterCard]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype ParallelFates2 = ParallelFates2 (EventAttrs `With` Metadata)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

parallelFates2 :: EventCard ParallelFates2
parallelFates2 =
  event (ParallelFates2 . (`with` Metadata [])) Cards.parallelFates2

instance RunMessage ParallelFates2 where
  runMessage msg e@(ParallelFates2 (attrs `With` meta)) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      targets <-
        selectTargets
          $ InvestigatorWithoutModifier CannotManipulateDeck
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ TargetLabel
            target
            [ lookAt iid attrs target [fromTopOfDeck 6] AnyCard (DeferSearchedToTarget $ toTarget attrs)
            ]
          | target <- EncounterDeckTarget : targets
          ]
      pure e
    SearchFound iid (isTarget attrs -> True) Deck.EncounterDeck cards -> do
      player <- getPlayer iid
      pushAll
        [ FocusCards cards
        , chooseOne
            player
            [ Label "Shuffle them in" [ShuffleCardsIntoDeck Deck.EncounterDeck cards]
            , Label
                "Put back in any order"
                [ chooseOneAtATime
                    player
                    [ targetLabel
                      (toCardId card)
                      [ PutCardOnTopOfDeck
                          iid
                          Deck.EncounterDeck
                          (EncounterCard card)
                      ]
                    | card <- mapMaybe (preview _EncounterCard) cards
                    ]
                ]
            ]
        , UnfocusCards
        ]
      pure e
    SearchFound iid (isTarget attrs -> True) deck@(Deck.InvestigatorDeck _) cards -> do
      let drawing = drawCards iid attrs 1
      player <- getPlayer iid
      pushAll
        [ FocusCards cards
        , chooseOne
            player
            [ Label "Shuffle them in" [ShuffleCardsIntoDeck deck cards]
            , Label
                "Put back in any order"
                [ chooseOneAtATime
                    player
                    [ targetLabel
                      (toCardId card)
                      [ PutCardOnTopOfDeck
                          iid
                          deck
                          (PlayerCard card)
                      ]
                    | card <- mapMaybe (preview _PlayerCard) cards
                    ]
                ]
            ]
        , UnfocusCards
        , drawing
        ]
      pure e
    _ -> ParallelFates2 . (`with` meta) <$> runMessage msg attrs
