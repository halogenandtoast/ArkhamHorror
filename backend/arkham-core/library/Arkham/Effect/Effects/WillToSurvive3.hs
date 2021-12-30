module Arkham.Effect.Effects.WillToSurvive3
  ( willToSurvive3
  , WillToSurvive3(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Attrs
import Arkham.Effect.Helpers
import Arkham.Message
import Arkham.Modifier

newtype WillToSurvive3 = WillToSurvive3 EffectAttrs
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

willToSurvive3 :: EffectArgs -> WillToSurvive3
willToSurvive3 = WillToSurvive3 . uncurry4 (baseAttrs "01085")

instance HasModifiersFor env WillToSurvive3 where
  getModifiersFor _ target (WillToSurvive3 a@EffectAttrs {..})
    | target == effectTarget = pure
      [toModifier a DoNotDrawChaosTokensForSkillChecks]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env WillToSurvive3 where
  runMessage msg e@(WillToSurvive3 attrs) = case msg of
    EndTurn _ -> e <$ push (DisableEffect $ effectId attrs) -- TODO: should we actually check who?
    _ -> WillToSurvive3 <$> runMessage msg attrs
