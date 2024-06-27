module Arkham.Effect.Import (module X, module Arkham.Effect.Import) where

import Arkham.Effect.Types as X (
  EffectArgs,
  EffectAttrs,
  IsEffect,
  cardEffect,
  cardEffectWith,
  setEffectMeta,
 )

import Arkham.Effect.Runner as X ()
import Arkham.EffectMetadata as X

import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Entity (toAttrs, Entity, EntityAttrs)
import Arkham.Effect.Types qualified as Msg
import Arkham.Message.Lifted (ReverseQueue)
import Arkham.Prelude

disable :: ReverseQueue m => EffectAttrs -> m ()
disable = push . Msg.disable

disableReturn :: (ReverseQueue m, Entity a, EntityAttrs a ~ EffectAttrs) => a -> m a
disableReturn a = disable (toAttrs a) >> pure a
