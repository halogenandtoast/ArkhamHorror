module Arkham.Effect.Effects.AlchemicalTransmutation
  ( AlchemicalTransmutation(..)
  , alchemicalTransmutation
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Message
import Arkham.ChaosToken
import Arkham.Window qualified as Window

newtype AlchemicalTransmutation = AlchemicalTransmutation EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alchemicalTransmutation :: EffectArgs -> AlchemicalTransmutation
alchemicalTransmutation =
  AlchemicalTransmutation . uncurry4 (baseAttrs "03032")

instance RunMessage AlchemicalTransmutation where
  runMessage msg e@(AlchemicalTransmutation attrs@EffectAttrs {..}) =
    case msg of
      RevealChaosToken _ iid token | InvestigatorTarget iid == effectTarget -> do
        when
          (chaosTokenFace token `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]
          )
          (pushAll
            [ If
              (Window.RevealChaosTokenEffect iid token effectId)
              [InvestigatorAssignDamage iid effectSource DamageAny 1 0]
            , DisableEffect effectId
            ]
          )
        pure e
      SkillTestEnds _ _ -> e <$ push (DisableEffect effectId)
      _ -> AlchemicalTransmutation <$> runMessage msg attrs
