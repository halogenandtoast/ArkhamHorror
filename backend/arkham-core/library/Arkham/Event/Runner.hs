{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Event.Runner (
  module X,
) where

import Arkham.Prelude

import Arkham.Calculation as X
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
import Arkham.SkillTest.Base as X (SkillTestDifficulty (..))
import Arkham.Source as X
import Arkham.Target as X

import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Types (Field (..))
import {-# SOURCE #-} Arkham.GameEnv (getCard)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Window
import Arkham.Matcher (EnemyMatcher (..))
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
  AttachEvent eid target | eid == eventId -> do
    case target of
      LocationTarget lid -> push $ PlaceEvent eventOwner eid (AttachedToLocation lid)
      EnemyTarget enid -> push $ PlaceEvent eventOwner eid (AttachedToEnemy enid)
      CardIdTarget cid -> do
        card <- getCard cid
        case card.kind of
          EnemyType -> do
            enemy <- selectJust $ EnemyWithCardId cid
            push $ PlaceEvent eventOwner eid (AttachedToEnemy enemy)
          PlayerEnemyType -> do
            enemy <- selectJust $ EnemyWithCardId cid
            push $ PlaceEvent eventOwner eid (AttachedToEnemy enemy)
          _ -> error "Cannot attach event to that type"
      _ -> error "Cannot attach event to that type"
    pure a
  Msg.InvestigatorEliminated iid | eventAttachedTarget a == Just (InvestigatorTarget iid) || iid == a.controller -> do
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
  ReadyExhausted -> do
    push (Ready $ toTarget a)
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
  ReturnChaosTokens tokens -> pure $ a & sealedChaosTokensL %~ filter (`notElem` tokens)
  UnsealChaosToken token -> pure $ a & sealedChaosTokensL %~ filter (/= token)
  RemoveAllChaosTokens face -> pure $ a & sealedChaosTokensL %~ filter ((/= face) . chaosTokenFace)
  PlaceEvent iid eid placement | eid == eventId -> do
    for_ placement.attachedTo \target ->
      pushM $ checkAfter $ Window.AttachCard (Just iid) (toCard a) target
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
          ReturnThisToHand -> push (ReturnToHand (eventController a) (toTarget a))
        Limbo -> case afterPlay of
          DiscardThis -> pushAll [after, toDiscardBy (eventController a) GameSource a]
          RemoveThisFromGame -> push (RemoveEvent $ toId a)
          ShuffleThisBackIntoDeck -> push (ShuffleIntoDeck (Deck.InvestigatorDeck $ eventController a) (toTarget a))
          ReturnThisToHand -> push (ReturnToHand (eventController a) (toTarget a))
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
  SpendUses _ target useType' n | isTarget a target -> do
    pure $ a & tokensL . ix useType' %~ max 0 . subtract n
  _ -> pure a
