module Arkham.Asset.Cards.Fieldwork (fieldwork, fieldworkEffect, Fieldwork (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Effect.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype Fieldwork = Fieldwork AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

fieldwork :: AssetCard Fieldwork
fieldwork = asset Fieldwork Cards.fieldwork

instance HasAbilities Fieldwork where
  getAbilities (Fieldwork attrs) =
    [reaction attrs 1 ControlsThis (exhaust attrs) (Enters #after You LocationWithAnyClues)]

instance RunMessage Fieldwork where
  runMessage msg a@(Fieldwork attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ createCardEffect Cards.fieldwork Nothing (attrs.ability 1) iid
      pure a
    _ -> Fieldwork <$> runMessage msg attrs

newtype FieldworkEffect = FieldworkEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

fieldworkEffect :: EffectArgs -> FieldworkEffect
fieldworkEffect = cardEffect FieldworkEffect Cards.fieldwork

instance HasModifiersFor FieldworkEffect where
  getModifiersFor target (FieldworkEffect a) | a `is` target = do
    mSkillTestSource <- getSkillTestSource
    pure $ toModifiers a [AnySkillValue 2 | isJust mSkillTestSource]
  getModifiersFor _ _ = pure []

instance RunMessage FieldworkEffect where
  runMessage msg e@(FieldworkEffect attrs) = case msg of
    EndPhase -> do
      push $ disable attrs
      pure e
    SkillTestEnds {} -> do
      push $ disable attrs
      pure e
    _ -> FieldworkEffect <$> runMessage msg attrs
