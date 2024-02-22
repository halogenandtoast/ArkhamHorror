module Arkham.Asset.Cards.DaisysToteBagAdvanced (
  daisysToteBagAdvanced,
  daisysToteBagAdvancedEffect,
  DaisysToteBagAdvanced (..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Effect.Runner
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Matcher qualified as Matcher
import Arkham.Prelude
import Arkham.Timing qualified as Timing
import Arkham.Trait
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype DaisysToteBagAdvanced = DaisysToteBagAdvanced AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

daisysToteBagAdvanced :: AssetCard DaisysToteBagAdvanced
daisysToteBagAdvanced = asset DaisysToteBagAdvanced Cards.daisysToteBagAdvanced

instance HasAbilities DaisysToteBagAdvanced where
  getAbilities (DaisysToteBagAdvanced a) =
    [ controlledAbility a 1 (DuringTurn You)
        $ ReactionAbility (Matcher.PlayCard #when You (basic #tome))
        $ (exhaust a)
    ]

instance HasModifiersFor DaisysToteBagAdvanced where
  getModifiersFor (InvestigatorTarget iid) (DaisysToteBagAdvanced a) | controlledBy a iid = do
    pure [toModifier a $ CanBecomeFast $ #asset <> #tome]
  getModifiersFor _ _ = pure []

slot :: AssetAttrs -> Slot
slot attrs = TraitRestrictedSlot (toSource attrs) Tome []

instance RunMessage DaisysToteBagAdvanced where
  runMessage msg a@(DaisysToteBagAdvanced attrs) = case msg of
    -- Slots need to be added before the asset is played so we hook into played card
    CardEnteredPlay iid card | toCardId card == toCardId attrs -> do
      pushAll $ replicate 2 (AddSlot iid HandSlot (slot attrs))
      DaisysToteBagAdvanced <$> runMessage msg attrs
    UseCardAbility _ (isSource attrs -> True) 1 [Window Timing.When (Window.PlayCard _ card) _] _ -> do
      push $ createCardEffect Cards.daisysToteBagAdvanced Nothing (attrs.ability 1) card
      pure a
    _ -> DaisysToteBagAdvanced <$> runMessage msg attrs

newtype DaisysToteBagAdvancedEffect = DaisysToteBagAdvancedEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

daisysToteBagAdvancedEffect :: EffectArgs -> DaisysToteBagAdvancedEffect
daisysToteBagAdvancedEffect = cardEffect DaisysToteBagAdvancedEffect Cards.daisysToteBagAdvanced

instance HasModifiersFor DaisysToteBagAdvancedEffect where
  getModifiersFor target (DaisysToteBagAdvancedEffect attrs) | target == attrs.target = do
    pure $ toModifiers attrs [BecomesFast]
  getModifiersFor _ _ = pure []

instance RunMessage DaisysToteBagAdvancedEffect where
  runMessage msg e@(DaisysToteBagAdvancedEffect attrs) = case msg of
    CardEnteredPlay _ card | CardIdTarget (toCardId card) == attrs.target -> do
      push $ disable attrs
      pure e
    _ -> DaisysToteBagAdvancedEffect <$> runMessage msg attrs
