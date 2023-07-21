module Arkham.Asset.Cards.ArbiterOfFates (
  arbiterOfFates,
  arbiterOfFatesEffect,
  ArbiterOfFates (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype ArbiterOfFates = ArbiterOfFates AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arbiterOfFates :: AssetCard ArbiterOfFates
arbiterOfFates =
  asset ArbiterOfFates Cards.arbiterOfFates

instance HasModifiersFor ArbiterOfFates where
  getModifiersFor (AbilityTarget "60401" ab) (ArbiterOfFates a)
    | abilityIndex ab == 1 && abilitySource ab == InvestigatorSource "60401" && not (assetExhausted a) = do
        pure $ toModifiers a [CanIgnoreLimit]
  getModifiersFor _ _ = pure []

instance HasAbilities ArbiterOfFates where
  getAbilities (ArbiterOfFates a) =
    [ restrictedAbility a 1 ControlsThis $
        ReactionAbility
          (ActivateAbility Timing.When You $ AbilityIs (InvestigatorSource "60401") 1)
          (ExhaustCost $ toTarget a)
    ]

getAbility :: [Window] -> Ability
getAbility [] = error "invalid ability"
getAbility (Window _ (Window.ActivateAbility _ ab) : _) = ab
getAbility (_ : xs) = getAbility xs

instance RunMessage ArbiterOfFates where
  runMessage msg a@(ArbiterOfFates attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getAbility -> ab) _ -> do
      pushAll
        [ DoNotCountUseTowardsAbilityLimit "60401" ab
        , createCardEffect Cards.arbiterOfFates Nothing attrs iid
        ]
      pure a
    _ -> ArbiterOfFates <$> runMessage msg attrs

newtype ArbiterOfFatesEffect = ArbiterOfFatesEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arbiterOfFatesEffect :: EffectArgs -> ArbiterOfFatesEffect
arbiterOfFatesEffect = cardEffect ArbiterOfFatesEffect Cards.arbiterOfFates

instance HasModifiersFor ArbiterOfFatesEffect where
  getModifiersFor (AbilityTarget "60401" ab) (ArbiterOfFatesEffect a)
    | abilityIndex ab == 1 && abilitySource ab == InvestigatorSource "60401" = do
        pure $ toModifiers a [IgnoreLimit | not (effectFinished a)]
  getModifiersFor _ _ = pure []

instance RunMessage ArbiterOfFatesEffect where
  runMessage msg e@(ArbiterOfFatesEffect attrs@EffectAttrs {..}) = case msg of
    UseCardAbility "60401" (InvestigatorSource "60401") 1 _ _ -> do
      pure . ArbiterOfFatesEffect $ attrs & finishedL .~ True
    ResolvedAbility ab | abilitySource ab == InvestigatorSource "60401" && abilityIndex ab == 1 -> do
      push $ DisableEffect effectId
      pure e
    _ -> ArbiterOfFatesEffect <$> runMessage msg attrs
