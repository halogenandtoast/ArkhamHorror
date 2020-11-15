{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Effect.Attrs where

import Arkham.Import

data Attrs = Attrs
  { effectId :: EffectId
  , effectCardCode :: CardCode
  , effectTarget :: Target
  , effectSource :: Source
  }
  deriving stock (Show, Generic)

baseAttrs :: EffectId -> CardCode -> Source -> Target -> Attrs
baseAttrs eid cardCode source target = Attrs
  { effectId = eid
  , effectSource = source
  , effectTarget = target
  , effectCardCode = cardCode
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

isSource :: Attrs -> Source -> Bool
isSource Attrs { effectId } (EffectSource eid) = effectId == eid
isSource _ _ = False

toSource :: Attrs -> Source
toSource Attrs { effectId } = EffectSource effectId
