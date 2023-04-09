module Arkham.Asset.Cards.ScrollOfSecretsSeeker3
  ( scrollOfSecretsSeeker3
  , ScrollOfSecretsSeeker3(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Cost
import Arkham.Criteria
import Arkham.Deck qualified as Deck
import Arkham.Matcher

newtype ScrollOfSecretsSeeker3 = ScrollOfSecretsSeeker3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scrollOfSecretsSeeker3 :: AssetCard ScrollOfSecretsSeeker3
scrollOfSecretsSeeker3 =
  asset ScrollOfSecretsSeeker3 Cards.scrollOfSecretsSeeker3

instance HasAbilities ScrollOfSecretsSeeker3 where
  getAbilities (ScrollOfSecretsSeeker3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility Nothing
        $ ActionCost 1
        <> ExhaustCost (toTarget a)
        <> UseCost (AssetWithId $ toId a) Secret 1
    ]

instance RunMessage ScrollOfSecretsSeeker3 where
  runMessage msg a@(ScrollOfSecretsSeeker3 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      targets <- selectTargets
        $ InvestigatorWithoutModifier CannotManipulateDeck
      push $ chooseOne
        iid
        [ TargetLabel
            target
            [ Search
                iid
                (toSource attrs)
                target
                [fromBottomOfDeck 3]
                AnyCard
                (DeferSearchedToTarget $ toTarget attrs)
            ]
        | target <- EncounterDeckTarget : targets
        ]
      pure a
    SearchFound _ (isTarget attrs -> True) _ cards | notNull cards -> do
      push (DoStep 1 msg)
      pure a
    DoStep 1 msg'@(SearchFound iid (isTarget attrs -> True) deck cards)
      | notNull cards -> do
        let
          discardMsg (PlayerCard card) = case deck of
            Deck.InvestigatorDeck iid' -> AddToDiscard iid' card
            _ -> error "Deck mismatch"
          discardMsg (EncounterCard card) = case deck of
            Deck.EncounterDeck -> AddToEncounterDiscard card
            _ -> error "Deck mismatch"
          discardMsg _ = error "Card mismatch"

        pushAll
          [ FocusCards cards
          , questionLabel "Discard 1 card?" iid
          $ ChooseOne
          $ Label "Do not discard" [UnfocusCards, DoStep 2 msg']
          : [ targetLabel
                (toCardId card)
                [ UnfocusCards
                , discardMsg card
                , DoStep 2 $ SearchFound
                  iid
                  (toTarget attrs)
                  deck
                  (deleteFirst card cards)
                ]
            | card <- cards
            ]
          ]
        pure a
    DoStep 2 msg'@(SearchFound _ (isTarget attrs -> True) Deck.EncounterDeck _)
      -> do
        push $ DoStep 3 msg'
        pure a
    DoStep 2 msg'@(SearchFound iid (isTarget attrs -> True) deck@(Deck.InvestigatorDeck iid') cards)
      | notNull cards
      -> do
        let playerCards = mapMaybe (preview _PlayerCard) cards
        if null playerCards
          then push $ DoStep 3 msg'
          else pushAll
            [ FocusCards cards
            , questionLabel "Add 1 card to hand?" iid
            $ ChooseOne
            $ Label "Do not add to hand" [UnfocusCards, DoStep 3 msg']
            : [ targetLabel
                  (toCardId card)
                  [ UnfocusCards
                  , addToHand iid' (PlayerCard card)
                  , DoStep 3 $ SearchFound
                    iid
                    (toTarget attrs)
                    deck
                    (deleteFirst (PlayerCard card) cards)
                  ]
              | card <- playerCards
              ]
            ]
        pure a
    DoStep 3 (SearchFound iid (isTarget attrs -> True) deck cards)
      | notNull cards -> do
        pushAll
          [ FocusCards cards
          , chooseOrRunOne
            iid
            [ targetLabel
                (toCardId card)
                [ chooseOne
                    iid
                    [ Label
                      "Place on bottom of deck"
                      [ UnfocusCards
                      , FocusCards [card]
                      , PutCardOnBottomOfDeck iid deck card
                      , DoStep 3 $ SearchFound
                        iid
                        (toTarget attrs)
                        deck
                        (deleteFirst card cards)
                      ]
                    , Label
                      "Place on top of deck"
                      [ UnfocusCards
                      , FocusCards [card]
                      , PutCardOnTopOfDeck iid deck card
                      , DoStep 3 $ SearchFound
                        iid
                        (toTarget attrs)
                        deck
                        (deleteFirst card cards)
                      ]
                    ]
                ]
            | card <- cards
            ]
          ]
        pure a
    _ -> ScrollOfSecretsSeeker3 <$> runMessage msg attrs
