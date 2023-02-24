module Arkham.Effect.Effects.Lockpicks
  ( Lockpicks(..)
  , lockpicks
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Game.Helpers
import Arkham.Message
import Arkham.Source

newtype Lockpicks = Lockpicks EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lockpicks :: EffectArgs -> Lockpicks
lockpicks = Lockpicks . uncurry4 (baseAttrs "60305")

instance HasModifiersFor Lockpicks where
  getModifiersFor target (Lockpicks a) | target == effectTarget a =
    case effectMetadata a of
      Just (EffectInt n) -> pure $ toModifiers a [AnySkillValue n]
      _ -> error "needs to be set"
  getModifiersFor _ _ = pure []

instance RunMessage Lockpicks where
  runMessage msg e@(Lockpicks attrs) = case msg of
    SkillTestEnds _ _ -> e <$ push (DisableEffect $ effectId attrs)
    PassedSkillTest _ _ _ SkillTestInitiatorTarget{} _ n | n < 2 ->
      case effectSource attrs of
        AssetSource aid ->
          e <$ pushAll [Discard (effectSource attrs) $ AssetTarget aid, DisableEffect $ toId attrs]
        _ -> error "lockpicks is an asset"
    FailedSkillTest _ _ _ SkillTestInitiatorTarget{} _ n | n < 2 ->
      case effectSource attrs of
        AssetSource aid ->
          e <$ pushAll [Discard (effectSource attrs) $ AssetTarget aid, DisableEffect $ toId attrs]
        _ -> error "lockpicks is an asset"
    _ -> Lockpicks <$> runMessage msg attrs
