module Arkham.Effect.Effects.Lucky
  ( lucky
  , Lucky(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Effect.Helpers
import Arkham.Message
import Arkham.Target

newtype Lucky = Lucky EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lucky :: EffectArgs -> Lucky
lucky = Lucky . uncurry4 (baseAttrs "01080")

instance HasModifiersFor Lucky where
  getModifiersFor _ target (Lucky a@EffectAttrs {..}) | target == effectTarget =
    pure [toModifier a $ AnySkillValue 2]
  getModifiersFor _ _ _ = pure []

instance RunMessage Lucky where
  runMessage msg e@(Lucky attrs) = case msg of
    CreatedEffect eid _ _ (InvestigatorTarget _) | eid == effectId attrs ->
      e <$ push RerunSkillTest
    SkillTestEnds _ -> e <$ push (DisableEffect $ effectId attrs)
    _ -> Lucky <$> runMessage msg attrs
