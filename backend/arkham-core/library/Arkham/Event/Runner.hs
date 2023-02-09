{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Event.Runner
  ( module X
  , discard
  ) where

import Arkham.Event.Types as X
import Arkham.Helpers.Message as X

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Message
import Arkham.Source
import Arkham.Target

discard :: EventAttrs -> Message
discard = Discard GameSource . toTarget

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
    | eventAttachedTarget a == Just (InvestigatorTarget iid) -> a
    <$ push (Discard GameSource (EventTarget eventId))
  Discard _ target | eventAttachedTarget a == Just target -> do
    pushAll
      $ [ UnsealToken token | token <- eventSealedTokens ]
      <> [Discard GameSource $ toTarget a]
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
  PlaceEvent _ eid placement | eid == eventId ->
    pure $ a & placementL .~ placement
  _ -> pure a
