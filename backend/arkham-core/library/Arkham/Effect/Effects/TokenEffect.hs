module Arkham.Effect.Effects.TokenEffect
  ( TokenEffect(..)
  , tokenEffect
  , tokenEffect'
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Id
import Arkham.Message
import Arkham.Source
import Arkham.Token
import Arkham.Window (Window)

newtype TokenEffect = TokenEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tokenEffect :: EffectArgs -> TokenEffect
tokenEffect = TokenEffect . uncurry4 (baseAttrs "tokef")

tokenEffect'
  :: EffectId -> EffectMetadata Window Message -> Source -> Token -> TokenEffect
tokenEffect' eid metadata source token = TokenEffect $ EffectAttrs
  { effectId = eid
  , effectSource = source
  , effectTarget = TokenTarget token
  , effectCardCode = "tokef"
  , effectMetadata = Just metadata
  , effectTraits = mempty
  , effectWindow = Nothing
  , effectFinished = False
  }

instance HasModifiersFor TokenEffect where
  getModifiersFor target (TokenEffect attrs) | target == effectTarget attrs =
    case effectMetadata attrs of
      Just (EffectModifiers modifiers) -> pure modifiers
      Just (FailedByEffectModifiers modifiers) -> pure modifiers
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage TokenEffect where
  runMessage msg e@(TokenEffect attrs@EffectAttrs {..}) = case msg of
    ResetTokens _ -> e <$ push (DisableEffect effectId)
    ReturnTokens tokens -> e <$ when
      (effectTarget `elem` map TokenTarget tokens)
      (push $ DisableEffect effectId)
    _ -> TokenEffect <$> runMessage msg attrs
