module Arkham.Asset.Cards.DeVermisMysteriis2 (
  deVermisMysteriis2,
  DeVermisMysteriis2 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Trait (Trait (Insight, Spell))
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype DeVermisMysteriis2 = DeVermisMysteriis2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deVermisMysteriis2 :: AssetCard DeVermisMysteriis2
deVermisMysteriis2 = asset DeVermisMysteriis2 Cards.deVermisMysteriis2

instance HasAbilities DeVermisMysteriis2 where
  getAbilities (DeVermisMysteriis2 a) =
    [ restrictedAbility
        a
        1
        ( ControlsThis
            <> ExtendedCardExists
              ( PlayableCardWithCostReduction 1 $
                  InDiscardOf You
                    <> BasicCardMatch
                      ( CardWithOneOf [CardWithTrait Spell, CardWithTrait Insight]
                          <> CardWithType EventType
                      )
              )
        )
        $ ActionAbility Nothing
        $ ActionCost 1
          <> ExhaustCost (toTarget a)
          <> DoomCost (toSource a) (toTarget a) 1
    ]

instance RunMessage DeVermisMysteriis2 where
  runMessage msg a@(DeVermisMysteriis2 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 windows' _ -> do
      let
        windows'' =
          nub $
            windows'
              <> [ Window Timing.When (Window.DuringTurn iid)
                 , Window Timing.When Window.FastPlayerWindow
                 ]
      cards <-
        selectList $
          PlayableCardWithCostReduction 1 $
            InDiscardOf (InvestigatorWithId iid)
              <> BasicCardMatch
                (EventCard <> CardWithOneOf [CardWithTrait Spell, CardWithTrait Insight] <> EventCard)

      push $
        chooseOne
          iid
          [ targetLabel
            (toCardId card)
            [ CreateWindowModifierEffect
                (EffectCardCostWindow $ toCardId card)
                ( EffectModifiers $
                    toModifiers
                      (toSource attrs)
                      [ ReduceCostOf (CardWithId $ toCardId card) 1
                      ]
                )
                (toSource attrs)
                (toTarget $ toCardId card)
            , CreateWindowModifierEffect
                EffectAbilityWindow
                ( EffectModifiers $
                    toModifiers
                      (toSource attrs)
                      [ RemoveFromGameInsteadOfDiscard
                      ]
                )
                (toSource attrs)
                (toTarget $ toCardId card)
            , AddToHand iid [card]
            , PayCardCost iid card windows''
            , RemoveFromGame (CardIdTarget $ toCardId card)
            ]
          | card <- cards
          ]
      pure a
    _ -> DeVermisMysteriis2 <$> runMessage msg attrs
