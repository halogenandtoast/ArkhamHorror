module Arkham.Asset.Cards.ScrollOfSecretsMystic3 (
  scrollOfSecretsMystic3,
  ScrollOfSecretsMystic3 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Capability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Matcher

newtype ScrollOfSecretsMystic3 = ScrollOfSecretsMystic3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

scrollOfSecretsMystic3 :: AssetCard ScrollOfSecretsMystic3
scrollOfSecretsMystic3 = asset ScrollOfSecretsMystic3 Cards.scrollOfSecretsMystic3

instance HasAbilities ScrollOfSecretsMystic3 where
  getAbilities (ScrollOfSecretsMystic3 a) =
    [ controlledAbility
        a
        1
        (exists $ oneOf [affectsOthers can.manipulate.deck, You <> can.target.encounterDeck])
        $ actionAbilityWithCost
        $ exhaust a
        <> assetUseCost a Secret 1
    ]

instance RunMessage ScrollOfSecretsMystic3 where
  runMessage msg a@(ScrollOfSecretsMystic3 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      targets <- selectTargets $ affectsOthers can.manipulate.deck
      hasEncounterDeck <- can.target.encounterDeck iid
      let
        doSearch target x =
          lookAt
            iid
            attrs
            target
            [x 1]
            AnyCard
            (DeferSearchedToTarget $ toTarget attrs)
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ TargetLabel
            target
            [ chooseOne
                player
                [ Label "Look at top" [doSearch target fromTopOfDeck]
                , Label "Look at bottom" [doSearch target fromBottomOfDeck]
                ]
            ]
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
            | card <- mapMaybe (preview _EncounterCard) cards
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
            | card <- mapMaybe (preview _PlayerCard) cards
            ]
        , UnfocusCards
        ]
      pure a
    _ -> ScrollOfSecretsMystic3 <$> runMessage msg attrs
