module Arkham.Asset.Cards.StickToThePlan
  ( stickToThePlan
  , StickToThePlan(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Types
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Cost
import Arkham.Criteria
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Matcher hiding ( PlaceUnderneath )
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Trait qualified as Trait

newtype StickToThePlan = StickToThePlan AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stickToThePlan :: AssetCard StickToThePlan
stickToThePlan = asset StickToThePlan Cards.stickToThePlan

instance HasModifiersFor StickToThePlan where
  getModifiersFor _ (InvestigatorTarget iid) (StickToThePlan attrs)
    | controlledBy attrs iid = pure
    $ toModifiers attrs (map AsIfInHand $ assetCardsUnderneath attrs)
  getModifiersFor _ (CardIdTarget cardId) (StickToThePlan attrs)
    | cardId `elem` map toCardId (assetCardsUnderneath attrs) = pure
    $ toModifiers
        attrs
        [AdditionalCost $ ExhaustCost $ AssetTarget $ toId attrs]
  getModifiersFor _ _ _ = pure []

instance HasAbilities StickToThePlan where
  getAbilities (StickToThePlan attrs) =
    [ restrictedAbility attrs 1 ControlsThis
        $ ReactionAbility (DrawingStartingHand Timing.When You) Free
    ]

instance RunMessage StickToThePlan where
  runMessage msg a@(StickToThePlan attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) _ 1 _ -> do
      push $ Search
        iid
        (toSource attrs)
        (InvestigatorTarget iid)
        [fromDeck]
        AnyCard
        (DeferSearchedToTarget $ toTarget attrs)
      pure a
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      let
        tacticsAndSupplies = filter
          (`cardMatch` (CardWithType EventType <> CardWithOneOf
                         (map CardWithTrait [Trait.Tactic, Trait.Supply])
                       )
          )
          cards
      push $ chooseUpToN
        iid
        3
        "Choose no more events"
        [ TargetLabel
            (CardIdTarget $ toCardId card)
            [ RemoveCardFromSearch iid (toCardId card)
            , PlaceUnderneath (toTarget attrs) [card]
            ]
        | card <- tacticsAndSupplies
        ]
      pure a
    InitiatePlayCard iid cardId _ _
      | controlledBy attrs iid && cardId `elem` map
        toCardId
        (assetCardsUnderneath attrs)
      -> do
        let
          card =
            fromJustNote "card missing" $ find matcher $ assetCardsUnderneath
              attrs
          remaining = deleteFirstMatch matcher $ assetCardsUnderneath attrs
          matcher = (== cardId) . toCardId
        pushAll
          [ CreateWindowModifierEffect
            EffectTurnWindow
            (EffectModifiers $ toModifiers
              attrs
              [AdditionalCost $ ExhaustCost $ AssetTarget $ toId attrs]
            )
            (toSource attrs)
            (CardIdTarget $ toCardId card)
          , AddToHand iid card
          , msg
          ]
        pure $ StickToThePlan $ attrs & cardsUnderneathL .~ remaining
    _ -> StickToThePlan <$> runMessage msg attrs
