module Arkham.Asset.Cards.Fieldwork (fieldwork, fieldworkEffect, Fieldwork (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Effect.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype Fieldwork = Fieldwork AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

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
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fieldworkEffect :: EffectArgs -> FieldworkEffect
fieldworkEffect = cardEffectWith FieldworkEffect Cards.fieldwork (setEffectMeta @Bool False)

instance HasModifiersFor FieldworkEffect where
  getModifiersFor target (FieldworkEffect a) | a.target == target = do
    mSkillTestSource <- getSkillTestSource
    let meta = toResult @Bool a.extra
    pure $ toModifiers a [AnySkillValue 2 | isJust mSkillTestSource && meta]
  getModifiersFor _ _ = pure []

instance RunMessage FieldworkEffect where
  runMessage msg e@(FieldworkEffect attrs) = case msg of
    BeginSkillTestAfterFast -> do
      pure . FieldworkEffect $ attrs & setEffectMeta @Bool True
    EndPhase -> do
      push $ disable attrs
      pure e
    SkillTestEnds {} -> do
      let meta = toResult @Bool attrs.extra
      pushWhen meta $ disable attrs
      pure e
    _ -> FieldworkEffect <$> runMessage msg attrs
