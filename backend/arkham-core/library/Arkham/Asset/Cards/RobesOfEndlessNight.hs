module Arkham.Asset.Cards.RobesOfEndlessNight (
  robesOfEndlessNight,
  RobesOfEndlessNight (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Timing qualified as Timing
import Arkham.Trait (Trait (Spell))
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype RobesOfEndlessNight = RobesOfEndlessNight AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

robesOfEndlessNight :: AssetCard RobesOfEndlessNight
robesOfEndlessNight =
  assetWith RobesOfEndlessNight Cards.robesOfEndlessNight (healthL ?~ 2)

instance HasModifiersFor RobesOfEndlessNight where
  getModifiersFor (InvestigatorTarget iid) (RobesOfEndlessNight a)
    | controlledBy a iid && not (assetExhausted a) =
        pure $ toModifiers a [CanReduceCostOf (CardWithTrait Spell) 1]
  getModifiersFor _ _ = pure []

instance HasAbilities RobesOfEndlessNight where
  getAbilities (RobesOfEndlessNight a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          ( Matcher.PlayCard
              Timing.When
              You
              (BasicCardMatch $ CardWithTrait Spell)
          )
        $ ExhaustCost (toTarget a)
    ]

instance RunMessage RobesOfEndlessNight where
  runMessage msg a@(RobesOfEndlessNight attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 [Window Timing.When (Window.PlayCard _ card) _] _ -> do
      push
        $ CreateWindowModifierEffect
          EffectCostWindow
          ( EffectModifiers
              $ toModifiers attrs [ReduceCostOf (CardWithId $ toCardId card) 1]
          )
          (toSource attrs)
          (InvestigatorTarget iid)
      pure a
    _ -> RobesOfEndlessNight <$> runMessage msg attrs
