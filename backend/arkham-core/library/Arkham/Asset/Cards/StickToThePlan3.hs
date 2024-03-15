module Arkham.Asset.Cards.StickToThePlan3 (
  stickToThePlan3,
  StickToThePlan3 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Trait qualified as Trait

newtype StickToThePlan3 = StickToThePlan3 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stickToThePlan3 :: AssetCard StickToThePlan3
stickToThePlan3 = asset StickToThePlan3 Cards.stickToThePlan3

instance HasModifiersFor StickToThePlan3 where
  getModifiersFor (InvestigatorTarget iid) (StickToThePlan3 attrs) | controlledBy attrs iid = do
    pure $ toModifiers attrs (map AsIfInHand $ assetCardsUnderneath attrs)
  getModifiersFor (CardIdTarget cardId) (StickToThePlan3 attrs)
    | cardId `elem` map toCardId (assetCardsUnderneath attrs) = do
        pure $ toModifiers attrs [AdditionalCost $ exhaust attrs]
  getModifiersFor _ _ = pure []

instance HasAbilities StickToThePlan3 where
  getAbilities (StickToThePlan3 attrs) =
    [ restrictedAbility attrs 1 ControlsThis
        $ freeReaction
        $ DrawingStartingHand #when You
    ]

instance RunMessage StickToThePlan3 where
  runMessage msg a@(StickToThePlan3 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ search iid attrs iid [fromDeck] AnyCard (DeferSearchedToTarget $ toTarget attrs)
      pure a
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      let
        tacticsAndSupplies =
          filter
            (`cardMatch` (#event <> oneOf (map CardWithTrait [Trait.Tactic, Trait.Supply])))
            cards
      additionalTargets <- getAdditionalSearchTargets iid
      player <- getPlayer iid
      if null tacticsAndSupplies
        then pushAll [FocusCards cards, chooseOne player [Label "No cards found" [UnfocusCards]]]
        else
          push
            $ chooseUpToN
              player
              (3 + additionalTargets)
              "Choose no more events"
              [ targetLabel
                (toCardId card)
                [ RemoveCardFromSearch iid (toCardId card)
                , PlaceUnderneath (toTarget attrs) [card]
                ]
              | card <- tacticsAndSupplies
              ]
      pure a
    InitiatePlayCard iid card _ _ _ _ | controlledBy attrs iid && card `elem` assetCardsUnderneath attrs -> do
      let remaining = deleteFirstMatch (== card) $ assetCardsUnderneath attrs
      pushAll
        [ costModifier attrs (toCardId card) (AdditionalCost $ ExhaustCost $ toTarget attrs)
        , addToHand iid card
        , msg
        ]
      pure $ StickToThePlan3 $ attrs & cardsUnderneathL .~ remaining
    _ -> StickToThePlan3 <$> runMessage msg attrs
