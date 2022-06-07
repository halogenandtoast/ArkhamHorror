module Arkham.Effect.Effects.Lucky2
  ( lucky2
  , Lucky2(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Effect.Helpers
import Arkham.Message
import Arkham.Modifier
import Arkham.Target

newtype Lucky2 = Lucky2 EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lucky2 :: EffectArgs -> Lucky2
lucky2 = Lucky2 . uncurry4 (baseAttrs "01084")

instance HasModifiersFor Lucky2 where
  getModifiersFor _ target (Lucky2 a@EffectAttrs {..})
    | target == effectTarget = pure [toModifier a $ AnySkillValue 2]
  getModifiersFor _ _ _ = pure []

instance RunMessage Lucky2 where
  runMessage msg e@(Lucky2 attrs) = case msg of
    CreatedEffect eid _ _ (InvestigatorTarget _) | eid == effectId attrs ->
      e <$ push RerunSkillTest
    SkillTestEnds _ -> e <$ push (DisableEffect $ effectId attrs)
    _ -> Lucky2 <$> runMessage msg attrs
