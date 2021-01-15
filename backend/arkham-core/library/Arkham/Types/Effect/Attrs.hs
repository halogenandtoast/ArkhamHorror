{-# LANGUAGE TemplateHaskell #-}

module Arkham.Types.Effect.Attrs
  ( module Arkham.Types.Effect.Attrs
  , module X
  ) where

import Arkham.Import

import Arkham.Types.Trait
import Arkham.Types.Effect.Window as X

data Attrs = Attrs
  { effectId :: EffectId
  , effectCardCode :: Maybe CardCode
  , effectTarget :: Target
  , effectSource :: Source
  , effectTraits :: HashSet Trait
  , effectMetadata :: Maybe (EffectMetadata Message)
  , effectWindow :: Maybe EffectWindow
  }
  deriving stock (Show, Generic)

makeLensesWith suffixedFields ''Attrs

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
  , effectTraits = mempty
  , effectWindow = Nothing
  }

instance ToJSON Attrs where
  toJSON = genericToJSON $ aesonOptions $ Just "effect"
  toEncoding = genericToEncoding $ aesonOptions $ Just "effect"

instance FromJSON Attrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "effect"

instance HasActions env Attrs where
  getActions _ _ _ = pure []

instance HasQueue env => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    EndPhase | EffectPhaseWindow `elem` effectWindow ->
      a <$ unshiftMessage (DisableEffect effectId)
    EndRound | EffectRoundWindow `elem` effectWindow ->
      a <$ unshiftMessage (DisableEffect effectId)
    SkillTestEnds _ | EffectSkillTestWindow `elem` effectWindow ->
      a <$ unshiftMessage (DisableEffect effectId)
    _ -> pure a

instance Entity Attrs where
  type EntityId Attrs = EffectId
  toId = effectId
  toSource = EffectSource . toId
  toTarget = EffectTarget . toId
  isSource Attrs { effectId } (EffectSource eid) = effectId == eid
  isSource _ _ = False
  isTarget Attrs { effectId } (EffectTarget eid) = effectId == eid
  isTarget _ _ = False
