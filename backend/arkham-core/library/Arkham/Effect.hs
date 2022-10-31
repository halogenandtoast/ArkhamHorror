{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Effect
  ( module Arkham.Effect
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Types
import Arkham.Effect.Effects
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Id
import Arkham.Message
import Arkham.Modifier
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Window ( Window )

createEffect
  :: MonadRandom m
  => CardCode
  -> Maybe (EffectMetadata Window Message)
  -> Source
  -> Target
  -> m (EffectId, Effect)
createEffect cardCode meffectMetadata source target = do
  eid <- getRandom
  pure (eid, lookupEffect cardCode eid meffectMetadata source target)

createTokenValueEffect
  :: MonadRandom m => Int -> Source -> Target -> m (EffectId, Effect)
createTokenValueEffect n source target = do
  eid <- getRandom
  pure (eid, buildTokenValueEffect eid n source target)

createWindowModifierEffect
  :: MonadRandom m
  => EffectWindow
  -> EffectMetadata Window Message
  -> Source
  -> Target
  -> m (EffectId, Effect)
createWindowModifierEffect effectWindow effectMetadata source target = do
  eid <- getRandom
  pure
    ( eid
    , buildWindowModifierEffect eid effectMetadata effectWindow source target
    )

createTokenEffect
  :: MonadRandom m
  => EffectMetadata Window Message
  -> Source
  -> Token
  -> m (EffectId, Effect)
createTokenEffect effectMetadata source token = do
  eid <- getRandom
  pure (eid, buildTokenEffect eid effectMetadata source token)

instance RunMessage Effect where
  runMessage msg (Effect a) = Effect <$> runMessage msg a

lookupEffect
  :: CardCode
  -> EffectId
  -> Maybe (EffectMetadata Window Message)
  -> Source
  -> Target
  -> Effect
lookupEffect cardCode eid mmetadata source target = case lookup cardCode allEffects of
  Nothing -> error $ "Unknown effect: " <> show cardCode
  Just (SomeEffect f) -> Effect $ f (eid, mmetadata, source, target)

buildTokenValueEffect :: EffectId -> Int -> Source -> Target -> Effect
buildTokenValueEffect eid n source = buildWindowModifierEffect
  eid
  (EffectModifiers [Modifier source $ TokenValueModifier n])
  EffectSkillTestWindow
  source

buildWindowModifierEffect
  :: EffectId
  -> EffectMetadata Window Message
  -> EffectWindow
  -> Source
  -> Target
  -> Effect
buildWindowModifierEffect eid metadata effectWindow source target =
  Effect $ windowModifierEffect' eid metadata effectWindow source target

buildTokenEffect
  :: EffectId -> EffectMetadata Window Message -> Source -> Token -> Effect
buildTokenEffect eid metadata source token =
  Effect $ tokenEffect' eid metadata source token

instance FromJSON Effect where
  parseJSON v = flip (withObject "Effect") v $ \o -> do
    cCode :: CardCode <- o .: "cardCode"
    case lookup cCode allEffects of
      Nothing -> error $ "Invalid effect: " <> show cCode
      Just (SomeEffect (_ :: EffectArgs ->a)) -> Effect <$> parseJSON @a v

allEffects :: HashMap CardCode SomeEffect
allEffects = mapFromList
  [ ("wmode", SomeEffect windowModifierEffect)
  , ("tokef", SomeEffect tokenEffect)
  ]
