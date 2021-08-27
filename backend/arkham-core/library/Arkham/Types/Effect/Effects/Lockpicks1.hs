module Arkham.Types.Effect.Effects.Lockpicks1
  ( Lockpicks1(..)
  , lockpicks1
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.EffectMetadata
import Arkham.Types.Game.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Source
import Arkham.Types.Target

newtype Lockpicks1 = Lockpicks1 EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lockpicks1 :: EffectArgs -> Lockpicks1
lockpicks1 = Lockpicks1 . uncurry4 (baseAttrs "03031")

instance HasModifiersFor env Lockpicks1 where
  getModifiersFor _ target (Lockpicks1 a) | target == effectTarget a =
    case effectMetadata a of
      Just (EffectInt n) -> pure $ toModifiers a [AnySkillValue n]
      _ -> error "needs to be set"
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env Lockpicks1 where
  runMessage msg e@(Lockpicks1 attrs) = case msg of
    SkillTestEnds _ -> e <$ push (DisableEffect $ effectId attrs)
    PassedSkillTest _ _ _ SkillTestInitiatorTarget{} _ n | n < 2 ->
      case effectSource attrs of
        AssetSource aid ->
          e <$ pushAll [Discard $ AssetTarget aid, DisableEffect $ toId attrs]
        _ -> error "lockpicks is an asset"
    FailedSkillTest _ _ _ SkillTestInitiatorTarget{} _ n | n < 2 ->
      case effectSource attrs of
        AssetSource aid ->
          e <$ pushAll [Discard $ AssetTarget aid, DisableEffect $ toId attrs]
        _ -> error "lockpicks is an asset"
    _ -> Lockpicks1 <$> runMessage msg attrs
