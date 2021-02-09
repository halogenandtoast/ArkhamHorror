module Arkham.Types.Effect.Effects.WillToSurvive4
  ( willToSurvive4
  , WillToSurvive4(..)
  )
where


import Arkham.Types.Effect.Attrs
import Arkham.Types.Effect.Helpers

newtype WillToSurvive4 = WillToSurvive4 EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

willToSurvive4 :: EffectArgs -> WillToSurvive4
willToSurvive4 = WillToSurvive4 . uncurry4 (baseAttrs "01085")

instance HasModifiersFor env WillToSurvive4 where
  getModifiersFor _ target (WillToSurvive4 a@EffectAttrs {..})
    | target == effectTarget = pure
      [toModifier a DoNotDrawChaosTokensForSkillChecks]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env WillToSurvive4 where
  runMessage msg e@(WillToSurvive4 attrs) = case msg of
    EndTurn _ -> e <$ unshiftMessage (DisableEffect $ effectId attrs) -- TODO: should we actually check who?
    _ -> WillToSurvive4 <$> runMessage msg attrs
