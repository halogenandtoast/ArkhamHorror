module Arkham.Effect.Effects.EighteenDerringer
  ( EighteenDerringer(..)
  , eighteenDerringer
  ) where

import Arkham.Prelude

import Arkham.Asset.Uses
import Arkham.Classes
import Arkham.Effect.Attrs
import Arkham.Message
import Arkham.Target

newtype EighteenDerringer = EighteenDerringer EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eighteenDerringer :: EffectArgs -> EighteenDerringer
eighteenDerringer = EighteenDerringer . uncurry4 (baseAttrs "60505")

instance HasQueue env => RunMessage env EighteenDerringer where
  runMessage msg e@(EighteenDerringer attrs) = case msg of
    FailedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | effectSource attrs == source
      -> e <$ pushAll
        [AddUses (effectTarget attrs) Ammo 1, DisableEffect $ toId attrs]
    SkillTestEnds _ -> e <$ push (DisableEffect $ toId attrs)
    _ -> EighteenDerringer <$> runMessage msg attrs
