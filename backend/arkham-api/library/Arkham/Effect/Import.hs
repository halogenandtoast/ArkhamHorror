module Arkham.Effect.Import (module X, module Arkham.Effect.Import) where

import Arkham.Effect.Types as X (
  EffectArgs,
  EffectAttrs,
  IsEffect,
  cardEffect,
  cardEffectWith,
  setEffectMeta,
 )

import Arkham.Effect.Runner as X (extraL)
import Arkham.EffectMetadata as X

import Arkham.Classes.Entity (Entity, EntityAttrs, toAttrs)
import Arkham.Classes.HasQueue (push)
import Arkham.Effect.Types qualified as Msg
import Arkham.Id
import Arkham.Message.Lifted (ReverseQueue)
import Arkham.Prelude

disable :: (ReverseQueue m, AsId a, IdOf a ~ EffectId) => a -> m ()
disable = push . Msg.disable

disableReturn :: (ReverseQueue m, Entity a, EntityAttrs a ~ EffectAttrs) => a -> m a
disableReturn a = disable (toAttrs a) >> pure a

finishedEffect :: EffectAttrs -> EffectAttrs
finishedEffect = Msg.finishedL .~ True

getEffectMetaDefault :: FromJSON a => a -> EffectAttrs -> a
getEffectMetaDefault a attrs = toResultDefault a $ attrs ^. extraL
