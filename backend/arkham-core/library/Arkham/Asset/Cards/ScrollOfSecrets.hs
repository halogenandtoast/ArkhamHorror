module Arkham.Asset.Cards.ScrollOfSecrets (
  scrollOfSecrets,
  ScrollOfSecrets (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Matcher

newtype ScrollOfSecrets = ScrollOfSecrets AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scrollOfSecrets :: AssetCard ScrollOfSecrets
scrollOfSecrets = asset ScrollOfSecrets Cards.scrollOfSecrets

instance HasAbilities ScrollOfSecrets where
  getAbilities (ScrollOfSecrets a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility Nothing
        $ ActionCost 1
        <> ExhaustCost (toTarget a)
        <> UseCost (AssetWithId $ toId a) Secret 1
    ]

instance RunMessage ScrollOfSecrets where
  runMessage msg a@(ScrollOfSecrets attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      targets <-
        selectTargets
          $ InvestigatorWithoutModifier CannotManipulateDeck
      push
        $ chooseOne
          iid
          [ TargetLabel
            target
            [ search
                iid
                attrs
                target
                [fromBottomOfDeck 1]
                AnyCard
                (DeferSearchedToTarget $ toTarget attrs)
            ]
          | target <- EncounterDeckTarget : targets
          ]
      pure a
    SearchFound iid (isTarget attrs -> True) Deck.EncounterDeck cards -> do
      pushAll
        [ FocusCards cards
        , chooseOrRunOne
            iid
            [ targetLabel
              (toCardId card)
              [ chooseOne
                  iid
                  [ Label "Discard" [AddToEncounterDiscard card]
                  , Label
                      "Place on bottom of encounter deck"
                      [ PutCardOnBottomOfDeck
                          iid
                          Deck.EncounterDeck
                          (EncounterCard card)
                      ]
                  , Label
                      "Place on top of encounter deck"
                      [ PutCardOnTopOfDeck
                          iid
                          Deck.EncounterDeck
                          (EncounterCard card)
                      ]
                  ]
              ]
            | card <- mapMaybe (preview _EncounterCard) cards
            ]
        , UnfocusCards
        ]
      pure a
    SearchFound iid (isTarget attrs -> True) deck@(Deck.InvestigatorDeck iid') cards ->
      do
        pushAll
          [ FocusCards cards
          , chooseOrRunOne
              iid
              [ targetLabel
                (toCardId card)
                [ chooseOne
                    iid
                    [ Label "Discard" [AddToDiscard iid' card]
                    , Label "Add to Hand" [addToHand iid' (PlayerCard card)]
                    , Label
                        "Place on bottom of deck"
                        [PutCardOnBottomOfDeck iid deck (PlayerCard card)]
                    , Label
                        "Place on top of deck"
                        [PutCardOnTopOfDeck iid deck (PlayerCard card)]
                    ]
                ]
              | card <- mapMaybe (preview _PlayerCard) cards
              ]
          , UnfocusCards
          ]
        pure a
    _ -> ScrollOfSecrets <$> runMessage msg attrs
