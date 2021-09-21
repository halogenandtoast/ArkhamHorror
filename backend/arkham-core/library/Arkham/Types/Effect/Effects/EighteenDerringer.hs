module Arkham.Types.Effect.Effects.EighteenDerringer
  ( EighteenDerringer(..)
  , eighteenDerringer
  ) where

import Arkham.Prelude

import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.Message
import Arkham.Types.Target

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
