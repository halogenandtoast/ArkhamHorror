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
import Arkham.Helpers.Query as X
import Arkham.Helpers.SkillTest as X
import Arkham.Source as X
import Arkham.Target as X

import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Window
import Arkham.Message qualified as Msg
import Arkham.Placement
import Arkham.Projection
import Arkham.Window (mkAfter)
import Arkham.Window qualified as Window

instance RunMessage EventAttrs where
  runMessage msg a@EventAttrs {..} = do
    result <- runEventMessage msg a
    pure
      $ if eventBeingPaidFor
        then case msg of
          SpendResources _ _ -> result & paymentMessagesL %~ (<> [msg])
          _ -> result
        else result

runEventMessage :: Runner EventAttrs
runEventMessage msg a@EventAttrs {..} = case msg of
  SetOriginalCardCode cardCode -> pure $ a & originalCardCodeL .~ cardCode
  Msg.InvestigatorEliminated iid | eventAttachedTarget a == Just (InvestigatorTarget iid) -> do
    push $ toDiscard GameSource eventId
    pure a
  Discard _ source (isTarget a -> True) -> do
    windows' <- windows [Window.WouldBeDiscarded (toTarget a)]
    pushAll
      $ windows'
      <> [Discarded (toTarget a) source (toCard a), RemoveFromPlay $ toSource a]
    pure a
  RemoveFromPlay source | isSource a source -> do
    let
      handleDiscard c = case c of
        PlayerCard pc -> AddToDiscard (fromJustNote "missing owner" $ toCardOwner c) pc
        EncounterCard ec -> AddToEncounterDiscard ec
        VengeanceCard vc -> handleDiscard vc

    windowMsg <-
      checkWindows
        ( (`Window.mkWindow` Window.LeavePlay (toTarget a))
            <$> [#when, #at, #after]
        )
    pushAll
      $ windowMsg
      : [UnsealChaosToken token | token <- eventSealedChaosTokens]
        <> map handleDiscard eventCardsUnderneath
        <> [RemovedFromPlay source]
    pure a
  Discard _ _ target | eventAttachedTarget a == Just target -> do
    push $ toDiscard GameSource a
    pure a
  Discard _ _ (AssetTarget aid) -> do
    case eventPlacement of
      AttachedToAsset aid' _ | aid == aid' -> do
        push $ toDiscard GameSource a
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
    case placement of
      InThreatArea iid -> do
        pushM $ checkWindows [mkAfter $ Window.EntersThreatArea iid (toCard a)]
      AttachedToEnemy eid' -> do
        p <- field EnemyPlacement eid'
        case p of
          InThreatArea iid -> do
            pushM $ checkWindows [mkAfter $ Window.EntersThreatArea iid (toCard a)]
          _ -> pure ()
      _ -> pure ()
    pure $ a & placementL .~ placement
  FinishedEvent eid | eid == eventId -> do
    mods <- liftA2 (<>) (getModifiers eid) (getModifiers $ toCardId $ toCard a)
    let
      modifyAfterPlay cur = \case
        SetAfterPlay n -> n
        _ -> cur

      afterPlay = foldl' modifyAfterPlay eventAfterPlay mods

    after <- checkWindows [mkAfter (Window.PlayEventDiscarding (eventController a) (toId a))]

    if LeaveCardWhereItIs `elem` mods
      then push $ RemoveEvent $ toId a
      else case eventPlacement of
        Unplaced -> case afterPlay of
          DiscardThis -> pushAll [after, toDiscardBy (eventController a) GameSource a]
          RemoveThisFromGame -> push (RemoveEvent $ toId a)
          ShuffleThisBackIntoDeck -> push (ShuffleIntoDeck (Deck.InvestigatorDeck $ eventController a) (toTarget a))
        Limbo -> case afterPlay of
          DiscardThis -> pushAll [after, toDiscardBy (eventController a) GameSource a]
          RemoveThisFromGame -> push (RemoveEvent $ toId a)
          ShuffleThisBackIntoDeck -> push (ShuffleIntoDeck (Deck.InvestigatorDeck $ eventController a) (toTarget a))
        _ -> pure ()
    pure a
  InvestigatorPlayEvent _ eid _ _ _ | eid == eventId -> do
    pure $ a & placementL .~ Limbo
  PlaceUnderneath (isTarget a -> True) cards -> do
    pure $ a & cardsUnderneathL <>~ cards
  AddToDiscard _ c -> do
    pure $ a & cardsUnderneathL %~ filter (/= toCard c)
  CommitCard _ card -> do
    pure $ a & cardsUnderneathL %~ filter (/= card)
  AddToHand _ cards -> do
    pure $ a & cardsUnderneathL %~ filter (`notElem` cards)
  ShuffleCardsIntoDeck _ cards ->
    pure $ a & cardsUnderneathL %~ filter (`notElem` cards)
  RemoveAllAttachments source target -> do
    case placementToAttached a.placement of
      Just attached | target == attached -> push $ toDiscard source a
      _ -> pure ()
    pure a
  _ -> pure a
