module Arkham.Types.Effect.Effects.AlchemicalTransmutation
  ( AlchemicalTransmutation(..)
  , alchemicalTransmutation
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Token

newtype AlchemicalTransmutation = AlchemicalTransmutation EffectAttrs
  deriving anyclass HasAbilities
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
