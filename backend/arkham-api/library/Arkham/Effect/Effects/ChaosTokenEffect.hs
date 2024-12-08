module Arkham.Effect.Effects.ChaosTokenEffect (
  ChaosTokenEffect (..),
  chaosTokenEffect,
  chaosTokenEffect',
) where

import Arkham.Prelude

import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Id
import Arkham.Window (Window)
import Control.Monad.Writer.Class
import Data.Map.Monoidal.Strict (MonoidalMap (..))

newtype ChaosTokenEffect = ChaosTokenEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chaosTokenEffect :: EffectArgs -> ChaosTokenEffect
chaosTokenEffect = ChaosTokenEffect . uncurry (baseAttrs "tokef")

chaosTokenEffect'
  :: EffectId -> EffectMetadata Window Message -> Source -> ChaosToken -> ChaosTokenEffect
chaosTokenEffect' eid metadata source chaosToken =
  ChaosTokenEffect
    $ EffectAttrs
      { effectId = eid
      , effectSource = source
      , effectTarget = ChaosTokenTarget chaosToken
      , effectCardCode = "tokef"
      , effectMetadata = Just metadata
      , effectTraits = mempty
      , effectWindow = Nothing
      , effectFinished = False
      , effectExtraMetadata = Null
      , effectSkillTest = Nothing
      , effectCardId = Nothing
      }

instance HasModifiersFor ChaosTokenEffect where
  getModifiersFor (ChaosTokenEffect attrs) =
    case effectMetadata attrs of
      Just (EffectModifiers modifiers) -> tell $ MonoidalMap $ singletonMap attrs.target modifiers
      _ -> pure ()

instance RunMessage ChaosTokenEffect where
  runMessage msg e@(ChaosTokenEffect attrs@EffectAttrs {..}) = case msg of
    ResetChaosTokens _ -> e <$ push (DisableEffect effectId)
    ReturnChaosTokens chaosTokens -> do
      when
        (effectTarget `elem` map ChaosTokenTarget chaosTokens)
        (push $ DisableEffect effectId)
      pure e
    _ -> ChaosTokenEffect <$> runMessage msg attrs
