{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Effect.Attrs where

import Arkham.Import

data Attrs = Attrs
  { effectId :: EffectId
  , effectCardCode :: Maybe CardCode
  , effectTarget :: Target
  , effectSource :: Source
  , effectMetadata :: Maybe (EffectMetadata Message)
  }
  deriving stock (Show, Generic)

type EffectArgs = (EffectId, Maybe (EffectMetadata Message), Source, Target)

baseAttrs
  :: CardCode
  -> EffectId
  -> Maybe (EffectMetadata Message)
  -> Source
  -> Target
  -> Attrs
baseAttrs cardCode eid meffectMetadata source target = Attrs
  { effectId = eid
  , effectSource = source
  , effectTarget = target
  , effectCardCode = Just cardCode
  , effectMetadata = meffectMetadata
  }

instance ToJSON Attrs where
  toJSON = genericToJSON $ aesonOptions $ Just "effect"
  toEncoding = genericToEncoding $ aesonOptions $ Just "effect"

instance FromJSON Attrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "effect"

instance HasActions env Attrs where
  getActions _ _ _ = pure []

instance HasQueue env => RunMessage env Attrs where
  runMessage _ = pure

instance Entity Attrs where
  toSource = EffectSource . effectId
  isSource Attrs { effectId } (EffectSource eid) = effectId == eid
  isSource _ _ = False
  toTarget = EffectTarget . effectId
  isTarget Attrs { effectId } (EffectTarget eid) = effectId == eid
  isTarget _ _ = False
