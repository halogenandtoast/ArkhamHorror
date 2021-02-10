module Arkham.Types.Effect.Effects.Lucky2
  ( lucky2
  , Lucky2(..)
  )
where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Effect.Attrs
import Arkham.Types.Effect.Helpers

newtype Lucky2 = Lucky2 EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lucky2 :: EffectArgs -> Lucky2
lucky2 = Lucky2 . uncurry4 (baseAttrs "01084")

instance HasModifiersFor env Lucky2 where
  getModifiersFor _ target (Lucky2 a@EffectAttrs {..}) | target == effectTarget =
    pure [toModifier a $ AnySkillValue 2]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env Lucky2 where
  runMessage msg e@(Lucky2 attrs) = case msg of
    CreatedEffect eid _ _ (InvestigatorTarget _) | eid == effectId attrs ->
      e <$ unshiftMessage RerunSkillTest
    SkillTestEnds _ -> e <$ unshiftMessage (DisableEffect $ effectId attrs)
    _ -> Lucky2 <$> runMessage msg attrs
