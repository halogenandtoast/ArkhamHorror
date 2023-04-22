module Arkham.Asset.Cards.RobesOfEndlessNight2
  ( robesOfEndlessNight2
  , RobesOfEndlessNight2(..)
  )
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Timing qualified as Timing
import Arkham.Trait ( Trait (Spell) )
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

newtype RobesOfEndlessNight2 = RobesOfEndlessNight2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

robesOfEndlessNight2 :: AssetCard RobesOfEndlessNight2
robesOfEndlessNight2 =
  assetWith RobesOfEndlessNight2 Cards.robesOfEndlessNight2 (healthL ?~ 2)

instance HasModifiersFor RobesOfEndlessNight2 where
  getModifiersFor (InvestigatorTarget iid) (RobesOfEndlessNight2 a) | controlledBy a iid && not (assetExhausted a) =
      pure $ toModifiers a [CanReduceCostOf (CardWithTrait Spell) 1]
  getModifiersFor _ _ = pure []

instance HasAbilities RobesOfEndlessNight2 where
  getAbilities (RobesOfEndlessNight2 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
            (Matcher.PlayCard
              Timing.When
              You
              (BasicCardMatch $ CardWithTrait Spell)
            )
        $ ExhaustCost (toTarget a)
    ]

instance RunMessage RobesOfEndlessNight2 where
  runMessage msg a@(RobesOfEndlessNight2 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 [Window Timing.When (Window.PlayCard _ card)] _ -> do
      push $ CreateWindowModifierEffect
        EffectCostWindow
        (EffectModifiers
        $ toModifiers attrs
          [ ReduceCostOf (CardWithId $ toCardId card) 1
          , ActionDoesNotCauseAttacksOfOpportunity Action.Play
          ]
        )
        (toSource attrs)
        (InvestigatorTarget iid)
      pure a
    _ -> RobesOfEndlessNight2 <$> runMessage msg attrs
