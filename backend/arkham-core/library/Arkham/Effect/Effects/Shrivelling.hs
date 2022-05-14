module Arkham.Effect.Effects.Shrivelling
  ( shrivelling
  , Shrivelling(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Attrs
import Arkham.EffectMetadata
import Arkham.Message
import Arkham.Target
import Arkham.Token
import Arkham.Window (Window)

newtype Shrivelling = Shrivelling EffectAttrs
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shrivelling :: EffectArgs -> Shrivelling
shrivelling = Shrivelling . uncurry4 (baseAttrs "01060")

instance HasModifiersFor env Shrivelling

intFromMetadata :: EffectMetadata Window a -> Int
intFromMetadata = \case
  EffectInt n -> n
  _ -> 0

instance HasQueue env => RunMessage env Shrivelling where
  runMessage msg e@(Shrivelling attrs@EffectAttrs {..}) = case msg of
    RevealToken _ iid token | InvestigatorTarget iid == effectTarget -> do
      let damage = maybe 1 intFromMetadata effectMetadata
      e <$ when
        (tokenFace token `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail])
        (pushAll
          [ InvestigatorAssignDamage iid effectSource DamageAny 0 damage
          , DisableEffect effectId
          ]
        )
    SkillTestEnds _ -> e <$ push (DisableEffect effectId)
    _ -> Shrivelling <$> runMessage msg attrs
