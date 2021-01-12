module Arkham.Types.Effect.Effects.SongOfTheDead
  ( songOfTheDead
  , SongOfTheDead(..)
  )
where

import Arkham.Import

import Arkham.Types.Effect.Attrs

newtype SongOfTheDead = SongOfTheDead Attrs
  deriving newtype (Show, ToJSON, FromJSON)


songOfTheDead :: EffectArgs -> SongOfTheDead
songOfTheDead = SongOfTheDead . uncurry4 (baseAttrs "02112")

instance HasModifiersFor env SongOfTheDead where
  getModifiersFor = noModifiersFor

instance HasQueue env => RunMessage env SongOfTheDead where
  runMessage msg e@(SongOfTheDead attrs@Attrs {..}) = case msg of
    RevealToken _ iid token | InvestigatorTarget iid == effectTarget ->
      e <$ when
        (token `elem` [Skull])
        (unshiftMessages
          [ CreateSkillTestEffect
            (EffectModifiers $ toModifiers attrs [DamageDealt 2])
            source
            (InvestigatorTarget iid)
          , DisableEffect effectId
          ]
        )
    SkillTestEnds _ -> e <$ unshiftMessage (DisableEffect effectId)
    _ -> SongOfTheDead <$> runMessage msg attrs
