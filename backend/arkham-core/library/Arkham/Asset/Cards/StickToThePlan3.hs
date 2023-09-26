module Arkham.Asset.Cards.StickToThePlan3 (
  stickToThePlan3,
  StickToThePlan3 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Timing qualified as Timing
import Arkham.Trait qualified as Trait

newtype StickToThePlan3 = StickToThePlan3 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stickToThePlan3 :: AssetCard StickToThePlan3
stickToThePlan3 = asset StickToThePlan3 Cards.stickToThePlan3

instance HasModifiersFor StickToThePlan3 where
  getModifiersFor (InvestigatorTarget iid) (StickToThePlan3 attrs)
    | controlledBy attrs iid =
        pure
          $ toModifiers attrs (map AsIfInHand $ assetCardsUnderneath attrs)
  getModifiersFor (CardIdTarget cardId) (StickToThePlan3 attrs)
    | cardId `elem` map toCardId (assetCardsUnderneath attrs) =
        pure
          $ toModifiers
            attrs
            [AdditionalCost $ ExhaustCost $ AssetTarget $ toId attrs]
  getModifiersFor _ _ = pure []

instance HasAbilities StickToThePlan3 where
  getAbilities (StickToThePlan3 attrs) =
    [ restrictedAbility attrs 1 ControlsThis
        $ ReactionAbility (DrawingStartingHand Timing.When You) Free
    ]

instance RunMessage StickToThePlan3 where
  runMessage msg a@(StickToThePlan3 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push
        $ search
          iid
          (toSource attrs)
          (toTarget iid)
          [fromDeck]
          AnyCard
          (DeferSearchedToTarget $ toTarget attrs)
      pure a
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      let
        tacticsAndSupplies =
          filter
            ( `cardMatch`
                ( CardWithType EventType
                    <> CardWithOneOf
                      (map CardWithTrait [Trait.Tactic, Trait.Supply])
                )
            )
            cards
      additionalTargets <- getAdditionalSearchTargets iid
      push
        $ chooseUpToN
          iid
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
    InitiatePlayCard iid card _ _ _
      | controlledBy attrs iid && card `elem` assetCardsUnderneath attrs -> do
          let remaining = deleteFirstMatch (== card) $ assetCardsUnderneath attrs
          pushAll
            [ CreateWindowModifierEffect
                EffectTurnWindow
                ( EffectModifiers
                    $ toModifiers
                      attrs
                      [AdditionalCost $ ExhaustCost $ AssetTarget $ toId attrs]
                )
                (toSource attrs)
                (CardIdTarget $ toCardId card)
            , addToHand iid card
            , msg
            ]
          pure $ StickToThePlan3 $ attrs & cardsUnderneathL .~ remaining
    _ -> StickToThePlan3 <$> runMessage msg attrs
