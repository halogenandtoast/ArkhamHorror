{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Event.Runner
  ( module X
  ) where

import Arkham.Event.Types as X

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Message
import Arkham.Target

instance RunMessage EventAttrs where
  runMessage msg a@EventAttrs {..} = do
    result <- runEventMessage msg a
    pure $ if eventBeingPaidFor
      then case msg of
        SpendResources _ _ -> result & paymentMessagesL %~ (<> [msg])
        _ -> result
      else result

runEventMessage :: Message -> EventAttrs -> GameT EventAttrs
runEventMessage msg a@EventAttrs {..} = case msg of
  SetOriginalCardCode cardCode -> pure $ a & originalCardCodeL .~ cardCode
  InvestigatorEliminated iid
    | eventAttachedTarget == Just (InvestigatorTarget iid) -> a
    <$ push (Discard (EventTarget eventId))
  AttachEvent eid target | eid == eventId ->
    pure $ a & attachedTargetL ?~ target
  Discard target | eventAttachedTarget == Just target -> do
    pushAll
      $ [ UnsealToken token | token <- eventSealedTokens ]
      <> [Discard $ toTarget a]
    pure a
  Ready (isTarget a -> True) -> pure $ a & exhaustedL .~ False
  Exhaust (isTarget a -> True) -> pure $ a & exhaustedL .~ True
  PayCardCost _ card _ | toCardId a == toCardId card ->
    pure $ a & beingPaidForL .~ True
  CardEnteredPlay _ card | toCardId a == toCardId card ->
    pure $ a & beingPaidForL .~ False
  SealedToken token card | toCardId card == toCardId a ->
    pure $ a & sealedTokensL %~ (token :)
  UnsealToken token -> pure $ a & sealedTokensL %~ filter (/= token)
  _ -> pure a
