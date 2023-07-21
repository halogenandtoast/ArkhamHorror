module Arkham.Effect.Effects.WillToSurvive3 (
  willToSurvive3,
  WillToSurvive3 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Helpers
import Arkham.Effect.Runner
import Arkham.Message

newtype WillToSurvive3 = WillToSurvive3 EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

willToSurvive3 :: EffectArgs -> WillToSurvive3
willToSurvive3 = WillToSurvive3 . uncurry4 (baseAttrs "01085")

instance HasModifiersFor WillToSurvive3 where
  getModifiersFor target (WillToSurvive3 a@EffectAttrs {..})
    | target == effectTarget =
        pure
          [toModifier a DoNotDrawChaosTokensForSkillChecks]
  getModifiersFor _ _ = pure []

instance RunMessage WillToSurvive3 where
  runMessage msg e@(WillToSurvive3 attrs) = case msg of
    EndTurn _ -> e <$ push (DisableEffect $ effectId attrs) -- TODO: should we actually check who?
    _ -> WillToSurvive3 <$> runMessage msg attrs
