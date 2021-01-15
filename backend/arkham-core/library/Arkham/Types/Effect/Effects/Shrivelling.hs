module Arkham.Types.Effect.Effects.Shrivelling
  ( shrivelling
  , Shrivelling(..)
  ) where

import Arkham.Import

import Arkham.Types.Effect.Attrs

newtype Shrivelling = Shrivelling Attrs
  deriving newtype (Show, ToJSON, FromJSON)

shrivelling :: EffectArgs -> Shrivelling
shrivelling = Shrivelling . uncurry4 (baseAttrs "01060")

instance HasModifiersFor env Shrivelling where
  getModifiersFor = noModifiersFor

instance HasQueue env => RunMessage env Shrivelling where
  runMessage msg e@(Shrivelling attrs@Attrs {..}) = case msg of
    RevealToken _ iid token | InvestigatorTarget iid == effectTarget ->
      e <$ when
        (token `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail])
        (unshiftMessages
          [ InvestigatorAssignDamage iid effectSource DamageAny 0 1
          , DisableEffect effectId
          ]
        )
    SkillTestEnds _ -> e <$ unshiftMessage (DisableEffect effectId)
    _ -> Shrivelling <$> runMessage msg attrs
