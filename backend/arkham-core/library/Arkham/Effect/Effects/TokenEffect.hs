module Arkham.Effect.Effects.TokenEffect
  ( TokenEffect(..)
  , tokenEffect
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Attrs
import Arkham.EffectMetadata
import Arkham.Id
import Arkham.Message
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Window (Window)

newtype TokenEffect = TokenEffect EffectAttrs
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tokenEffect
  :: EffectId -> EffectMetadata Window Message -> Source -> Token -> TokenEffect
tokenEffect eid metadata source token = TokenEffect $ EffectAttrs
  { effectId = eid
  , effectSource = source
  , effectTarget = TokenTarget token
  , effectCardCode = Nothing
  , effectMetadata = Just metadata
  , effectTraits = mempty
  , effectWindow = Nothing
  }

instance HasModifiersFor env TokenEffect where
  getModifiersFor _ target (TokenEffect attrs) | target == effectTarget attrs =
    case effectMetadata attrs of
      Just (EffectModifiers modifiers) -> pure modifiers
      Just (FailedByEffectModifiers modifiers) -> pure modifiers
      _ -> pure []
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env TokenEffect where
  runMessage msg e@(TokenEffect attrs@EffectAttrs {..}) = case msg of
    ResetTokens _ -> e <$ push (DisableEffect effectId)
    ReturnTokens tokens -> e <$ when
      (effectTarget `elem` map TokenTarget tokens)
      (push $ DisableEffect effectId)
    _ -> TokenEffect <$> runMessage msg attrs
