{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Event.Runner
  ( module X
  ) where

import Arkham.Event.Attrs as X

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Target

instance RunMessage EventAttrs where
  runMessage msg a@EventAttrs {..} = case msg of
    SetOriginalCardCode cardCode -> pure $ a & originalCardCodeL .~ cardCode
    InvestigatorEliminated iid
      | eventAttachedTarget == Just (InvestigatorTarget iid) -> a
      <$ push (Discard (EventTarget eventId))
    AttachEvent eid target | eid == eventId ->
      pure $ a & attachedTargetL ?~ target
    Ready (isTarget a -> True) -> pure $ a & exhaustedL .~ False
    Exhaust (isTarget a -> True) -> pure $ a & exhaustedL .~ True
    _ -> pure a
