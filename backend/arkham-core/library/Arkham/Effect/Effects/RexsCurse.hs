module Arkham.Effect.Effects.RexsCurse
  ( RexsCurse(..)
  , rexsCurse
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Attrs
import Arkham.Message

newtype RexsCurse = RexsCurse EffectAttrs
  deriving anyclass (HasModifiersFor env, HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rexsCurse :: EffectArgs -> RexsCurse
rexsCurse = RexsCurse . uncurry4 (baseAttrs "02009")

instance HasQueue env => RunMessage env RexsCurse where
  runMessage msg e@(RexsCurse attrs) = case msg of
    FailedSkillTest iid _ _ target _ _ | target == effectTarget attrs ->
      e <$ push (ShuffleIntoDeck iid target)
    SkillTestEnds _ -> e <$ push (DisableEffect (toId attrs))
    _ -> RexsCurse <$> runMessage msg attrs
