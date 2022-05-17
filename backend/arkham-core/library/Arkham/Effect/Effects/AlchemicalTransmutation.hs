module Arkham.Effect.Effects.AlchemicalTransmutation
  ( AlchemicalTransmutation(..)
  , alchemicalTransmutation
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Attrs
import Arkham.Message
import Arkham.Target
import Arkham.Token

newtype AlchemicalTransmutation = AlchemicalTransmutation EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alchemicalTransmutation :: EffectArgs -> AlchemicalTransmutation
alchemicalTransmutation =
  AlchemicalTransmutation . uncurry4 (baseAttrs "03032")

instance HasModifiersFor env AlchemicalTransmutation

instance HasQueue env => RunMessage env AlchemicalTransmutation where
  runMessage msg e@(AlchemicalTransmutation attrs@EffectAttrs {..}) =
    case msg of
      RevealToken _ iid token | InvestigatorTarget iid == effectTarget -> do
        e <$ when
          (tokenFace token `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]
          )
          (pushAll
            [ InvestigatorAssignDamage iid effectSource DamageAny 1 0
            , DisableEffect effectId
            ]
          )
      SkillTestEnds _ -> e <$ push (DisableEffect effectId)
      _ -> AlchemicalTransmutation <$> runMessage msg attrs
