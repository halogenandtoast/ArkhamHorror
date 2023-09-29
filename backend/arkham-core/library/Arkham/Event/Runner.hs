{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Event.Runner (
  module X,
) where

import Arkham.Prelude

import Arkham.Event.Types as X
import Arkham.Helpers.Event as X
import Arkham.Helpers.Message as X hiding (
  EnemyDefeated,
  InvestigatorEliminated,
  PlayCard,
  RevealLocation,
 )
import Arkham.Helpers.SkillTest as X
import Arkham.Source as X
import Arkham.Target as X

import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Deck qualified as Deck
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Message qualified as Msg
import Arkham.Placement

instance RunMessage EventAttrs where
  runMessage msg a@EventAttrs {..} = do
    result <- runEventMessage msg a
    pure
      $ if eventBeingPaidFor
        then case msg of
          SpendResources _ _ -> result & paymentMessagesL %~ (<> [msg])
          _ -> result
        else result

runEventMessage :: Message -> EventAttrs -> GameT EventAttrs
runEventMessage msg a@EventAttrs {..} = case msg of
  SetOriginalCardCode cardCode -> pure $ a & originalCardCodeL .~ cardCode
  Msg.InvestigatorEliminated iid | eventAttachedTarget a == Just (InvestigatorTarget iid) -> do
    push $ Discard GameSource (toTarget eventId)
    pure a
  Discard _ target | eventAttachedTarget a == Just target -> do
    pushAll
      $ [UnsealChaosToken token | token <- eventSealedChaosTokens]
      <> [Discard GameSource $ toTarget a]
    pure a
  Discard _ (AssetTarget aid) -> do
    case eventPlacement of
      AttachedToAsset aid' _ | aid == aid' -> do
        pushAll
          $ [UnsealChaosToken token | token <- eventSealedChaosTokens]
          <> [Discard GameSource $ toTarget a]
      _ -> pure ()
    pure a
  Ready (isTarget a -> True) -> pure $ a & exhaustedL .~ False
  Exhaust (isTarget a -> True) -> pure $ a & exhaustedL .~ True
  ExhaustThen (isTarget a -> True) msgs -> do
    unless eventExhausted $ pushAll msgs
    pure $ a & exhaustedL .~ True
  PayCardCost _ card _ | toCardId a == toCardId card -> do
    pure $ a & beingPaidForL .~ True
  CardEnteredPlay _ card | toCardId a == toCardId card -> do
    pure $ a & beingPaidForL .~ False
  SealedChaosToken token card | toCardId card == toCardId a -> do
    pure $ a & sealedChaosTokensL %~ (token :)
  UnsealChaosToken token -> pure $ a & sealedChaosTokensL %~ filter (/= token)
  RemoveAllChaosTokens face -> pure $ a & sealedChaosTokensL %~ filter ((/= face) . chaosTokenFace)
  PlaceEvent _ eid placement | eid == eventId -> do
    pure $ a & placementL .~ placement
  FinishedEvent eid | eid == eventId -> do
    mods <- liftA2 (<>) (getModifiers eid) (getModifiers $ toCardId $ toCard a)
    let
      modifyAfterPlay cur = \case
        SetAfterPlay n -> n
        _ -> cur

      afterPlay = foldl' modifyAfterPlay eventAfterPlay mods

    case eventPlacement of
      Unplaced -> case afterPlay of
        DiscardThis -> push $ Discard GameSource (toTarget a)
        RemoveThisFromGame -> push (RemoveEvent $ toId a)
        ShuffleThisBackIntoDeck -> push (ShuffleIntoDeck (Deck.InvestigatorDeck $ eventController a) (toTarget a))
      Limbo -> case afterPlay of
        DiscardThis -> push $ Discard GameSource (toTarget a)
        RemoveThisFromGame -> push (RemoveEvent $ toId a)
        ShuffleThisBackIntoDeck -> push (ShuffleIntoDeck (Deck.InvestigatorDeck $ eventController a) (toTarget a))
      _ -> pure ()
    pure a
  InvestigatorPlayEvent _ eid _ _ _ | eid == eventId -> do
    pure $ a & placementL .~ Limbo
  _ -> pure a
