module Arkham.Types.Effect.Effects.SongOfTheDead2
  ( shrivelling
  , SongOfTheDead2(..)
  )
where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.Game.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Token

newtype SongOfTheDead2 = SongOfTheDead2 EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shrivelling :: EffectArgs -> SongOfTheDead2
shrivelling = SongOfTheDead2 . uncurry4 (baseAttrs "02112")

instance HasModifiersFor env SongOfTheDead2 where
  getModifiersFor = noModifiersFor

instance HasQueue env => RunMessage env SongOfTheDead2 where
  runMessage msg e@(SongOfTheDead2 attrs@EffectAttrs {..}) = case msg of
    RevealToken _ iid Skull | InvestigatorTarget iid == effectTarget ->
      e <$ unshiftMessage
        (CreateWindowModifierEffect
          EffectSkillTestWindow
          (EffectModifiers $ toModifiers attrs [DamageDealt 2])
          (toSource attrs)
          (InvestigatorTarget iid)
        )
    SkillTestEnds _ -> e <$ unshiftMessage (DisableEffect effectId)
    _ -> SongOfTheDead2 <$> runMessage msg attrs
