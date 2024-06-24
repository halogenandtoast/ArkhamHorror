module Arkham.Asset.Cards.ScrollOfSecrets (
  scrollOfSecrets,
  ScrollOfSecrets (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Capability
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
    [ controlledAbility
        a
        1
        (exists $ oneOf [affectsOthers can.manipulate.deck, You <> can.target.encounterDeck])
        $ actionAbilityWithCost
        $ exhaust a
        <> assetUseCost a Secret 1
    ]

instance RunMessage ScrollOfSecrets where
  runMessage msg a@(ScrollOfSecrets attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      targets <- selectTargets $ affectsOthers can.manipulate.deck
      hasEncounterDeck <- can.target.encounterDeck iid
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ TargetLabel
            target
            [lookAt iid attrs target [fromBottomOfDeck 1] AnyCard (defer attrs IsNotDraw)]
          | target <- [EncounterDeckTarget | hasEncounterDeck] <> targets
          ]
      pure a
    SearchFound iid (isTarget attrs -> True) Deck.EncounterDeck cards -> do
      player <- getPlayer iid
      pushAll
        [ FocusCards cards
        , chooseOrRunOne
            player
            [ targetLabel
              (toCardId card)
              [ chooseOne
                  player
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
            | card <- onlyEncounterCards cards
            ]
        , UnfocusCards
        ]
      pure a
    SearchFound iid (isTarget attrs -> True) deck@(Deck.InvestigatorDeck iid') cards -> do
      player <- getPlayer iid
      pushAll
        [ FocusCards cards
        , chooseOrRunOne
            player
            [ targetLabel
              (toCardId card)
              [ chooseOne
                  player
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
            | card <- onlyPlayerCards cards
            ]
        , UnfocusCards
        ]
      pure a
    _ -> ScrollOfSecrets <$> runMessage msg attrs
