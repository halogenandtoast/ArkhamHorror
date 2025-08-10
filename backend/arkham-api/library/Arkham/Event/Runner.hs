{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Event.Runner (
  module X,
) where

import Arkham.Prelude

import Arkham.Calculation as X
import Arkham.Event.Types as X
import Arkham.Helpers.Effect as X
import Arkham.Helpers.Event as X
import Arkham.Helpers.Message as X hiding (
  EnemyDefeated,
  InvestigatorEliminated,
  PlayCard,
  RevealLocation,
 )
import Arkham.Helpers.Query as X
import Arkham.Helpers.SkillTest as X
import Arkham.Id as X
import Arkham.SkillTest.Base as X (SkillTestDifficulty (..))
import Arkham.Source as X
import Arkham.Target as X

import Arkham.Asset.Types (Field (AssetUses))
import Arkham.Asset.Uses
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Types (Field (..))
import {-# SOURCE #-} Arkham.GameEnv (getCard)
import Arkham.Helpers.Calculation (calculate)
import Arkham.Helpers.Customization
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Window
import Arkham.Matcher (EnemyMatcher (..))
import Arkham.Message qualified as Msg
import Arkham.Placement
import Arkham.Projection
import Arkham.Window (mkAfter)
import Arkham.Window qualified as Window
import Control.Lens (non)
import Data.IntMap.Strict qualified as IntMap

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
  IncreaseCustomization iid cardCode customization choices | toCardCode a == cardCode && a.owner == iid -> do
    case customizationIndex a customization of
      Nothing -> pure a
      Just i ->
        pure
          $ a
            { eventCustomizations =
                IntMap.alter (Just . maybe (1, choices) (bimap (+ 1) (const choices))) i eventCustomizations
            }
  SetOriginalCardCode cardCode -> pure $ a & originalCardCodeL .~ cardCode
  AttachEvent eid target | eid == eventId -> do
    case target of
      LocationTarget lid -> push $ PlaceEvent eid (AttachedToLocation lid)
      EnemyTarget enid -> push $ PlaceEvent eid (AttachedToEnemy enid)
      CardIdTarget cid -> do
        card <- getCard cid
        case card.kind of
          EnemyType -> do
            menemy <- selectOne $ EnemyWithCardId cid
            for_ menemy \enemy ->
              push $ PlaceEvent eid (AttachedToEnemy enemy)
          PlayerEnemyType -> do
            menemy <- selectOne $ EnemyWithCardId cid
            for_ menemy \enemy ->
              push $ PlaceEvent eid (AttachedToEnemy enemy)
          _ -> error "Cannot attach event to that type"
      _ -> error "Cannot attach event to that type"
    pure a
  Msg.InvestigatorEliminated iid
    | eventAttachedTarget a
        == Just (InvestigatorTarget iid)
        || (iid == a.controller && isInPlayPlacement a.placement) -> do
        push $ RemoveFromGame (toTarget a)
        pure a
  Discard _ source (isTarget a -> True) -> do
    pushAll
      $ windows [Window.WouldBeDiscarded (toTarget a)]
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
  SealedChaosToken token _ (isTarget a -> True) -> do
    pure $ a & sealedChaosTokensL %~ (token :)
  SealedChaosToken token _ _ -> do
    pure $ a & sealedChaosTokensL %~ filter (/= token)
  ReturnChaosTokens tokens -> pure $ a & sealedChaosTokensL %~ filter (`notElem` tokens)
  UnsealChaosToken token -> pure $ a & sealedChaosTokensL %~ filter (/= token)
  RemoveAllChaosTokens face -> pure $ a & sealedChaosTokensL %~ filter ((/= face) . chaosTokenFace)
  PlaceEvent eid placement | eid == eventId -> do
    let
      controller =
        case placement of
          AttachedToInvestigator iid' -> iid'
          InPlayArea iid' -> iid'
          _ -> a.controller
    for_ placement.attachedTo \target ->
      pushM $ checkAfter $ Window.AttachCard (Just a.controller) (toCard a) target
    case placement of
      InThreatArea iid' -> do
        pushM $ checkWindows [mkAfter $ Window.EntersThreatArea iid' (toCard a)]
      AttachedToEnemy eid' -> do
        p <- field EnemyPlacement eid'
        case p of
          InThreatArea iid' -> do
            pushM $ checkWindows [mkAfter $ Window.EntersThreatArea iid' (toCard a)]
          _ -> pure ()
      _ -> pure ()
    pure $ a & placementL .~ placement & controllerL .~ controller
  FinishedEvent eid | eid == eventId -> do
    mods <- liftA2 (<>) (getModifiers eid) (getModifiers $ toCardId $ toCard a)
    let
      modifyAfterPlay cur = \case
        SetAfterPlay n -> case cur of
          DevourThis {} -> cur
          _ -> n
        _ -> cur

      afterPlay = foldl' modifyAfterPlay eventAfterPlay mods

    after <- checkWindows [mkAfter (Window.PlayEventDiscarding eventController (toId a))]
    afterAfter <- checkWindows [mkAfter (Window.PlayEvent eventController (toId a))]

    if LeaveCardWhereItIs `elem` mods
      then push $ RemoveEvent $ toId a
      else case eventPlacement of
        Limbo -> case afterPlay of
          PlaceThisBeneath target -> pushAll [after, PlaceUnderneath target [toCard a], afterAfter]
          DiscardThis -> pushAll [after, toDiscardBy eventController GameSource a, afterAfter]
          ExileThis -> pushAll [after, Exile (toTarget a), afterAfter]
          RemoveThisFromGame -> push (RemoveEvent $ toId a)
          AbsoluteRemoveThisFromGame -> push (RemoveEvent $ toId a)
          ShuffleThisBackIntoDeck -> push (ShuffleIntoDeck (Deck.InvestigatorDeck eventController) (toTarget a))
          ReturnThisToHand -> push (ReturnToHand eventController (toTarget a))
          DevourThis iid' -> do
            c <- field EventCard a.id
            push $ RemovedFromPlay (toSource a)
            push $ Devoured iid' c
        _ -> case afterPlay of
          PlaceThisBeneath target -> pushAll [PlaceUnderneath target [toCard a]]
          AbsoluteRemoveThisFromGame -> push (RemoveEvent $ toId a)
          DevourThis iid' -> do
            c <- field EventCard a.id
            push $ RemovedFromPlay (toSource a)
            push $ Devoured iid' c
          _ -> pushAll [after, afterAfter] -- Changed to allow Fast Cards to be played
    pure a
  After (Revelation _iid (isSource a -> True)) -> do
    result <- runMessage (FinishedEvent a.id) a
    push $ ObtainCard (toCard result).id
    pure result
  InvestigatorPlayEvent _ eid _ _ _ | eid == eventId -> do
    pure $ a & placementL .~ Limbo
  PlaceUnderneath (isTarget a -> True) cards -> do
    pure $ a & cardsUnderneathL <>~ cards
  PlaceUnderneath _ cards -> do
    pure
      $ if toCard a `elem` cards
        then a & afterPlayL .~ AbsoluteRemoveThisFromGame
        else a
  AddToDiscard _ c -> do
    pure $ a & cardsUnderneathL %~ filter (/= toCard c)
  CommitCard _ card -> do
    pure $ a & cardsUnderneathL %~ filter (/= card)
  AddToHand _ cards -> do
    pure $ a & cardsUnderneathL %~ filter (`notElem` cards)
  Exile target | a `isTarget` target -> do
    pushAll [RemoveFromPlay $ toSource a, Exiled target (toCard a)]
    pure a
  ShuffleCardsIntoDeck _ cards ->
    pure $ a & cardsUnderneathL %~ filter (`notElem` cards)
  RemoveAllAttachments source target -> do
    case placementToAttached a.placement of
      Just attached | target == attached -> push $ toDiscard source a
      _ -> pure ()
    pure a
  SpendUses source target useType' n | isTarget a target -> do
    mods <- getModifiers a
    let otherSources = [s | ProvidesUses uType s <- mods, uType == useType']
    otherSourcePairs <- for otherSources \case
      AssetSource aid' -> do
        uses <- fieldMap AssetUses (findWithDefault 0 useType') aid'
        pure (AssetTarget aid', uses)
      EventSource eid' -> do
        uses <- fieldMap EventUses (findWithDefault 0 useType') eid'
        pure (EventTarget eid', uses)
      _ -> error $ "Unhandled source: " <> show source

    -- window should be independent of other sources since they are spent from this asset
    -- for_ eventController $ \controller ->
    --   pushM $ checkWindows [mkAfter $ Window.SpentUses controller source (toId a) useType' n]

    if null otherSourcePairs
      then push $ Do msg
      else do
        -- we may want a nicer way to handle this, but for the now the
        -- logic is to duplicate the choice for each use (the ui can only
        -- display it being clickable) and then to remove that choice from
        -- the list when used.
        player <- getPlayer a.controller
        push
          $ chooseN player n
          $ replicate (a.use useType') (targetLabel a [Do msg])
          <> concat
            [ replicate x (targetLabel otherTarget [Do $ SpendUses source otherTarget useType' n])
            | (otherTarget, x) <- otherSourcePairs
            ]
    pure a
  Do (SpendUses source target useType' n) | isTarget a target -> do
    pushM $ checkAfter $ Window.SpentToken source (toTarget a) useType' n
    runMessage (RemoveTokens source target useType' n) a
  MoveTokens s source _ tType n | isSource a source -> runMessage (RemoveTokens s (toTarget a) tType n) a
  MoveTokens _s (InvestigatorSource _) target Clue _ | isTarget a target -> pure a
  MoveTokens s _ target tType n | isTarget a target -> runMessage (PlaceTokens s (toTarget a) tType n) a
  RemoveTokens _ target tType n | isTarget a target -> pure $ a & tokensL %~ subtractTokens tType n
  PlaceTokens source target tType n | isTarget a target -> do
    pushM $ checkAfter $ Window.PlacedToken source target tType n
    when (tType == Doom && a.doom == 0) do
      pushM $ checkAfter $ Window.PlacedDoomCounterOnTargetWithNoDoom source target n
    if tokenIsUse tType
      then case eventPrintedUses of
        NoUses -> pure $ a & tokensL . at tType . non 0 %~ (+ n)
        Uses useType'' _ | tType == useType'' -> pure $ a & tokensL . at tType . non 0 %~ (+ n)
        UsesWithLimit useType'' _ pl | tType == useType'' -> do
          l <- calculate pl
          pure $ a & tokensL . at tType . non 0 %~ min l . (+ n)
        _ ->
          error
            $ "Trying to add the wrong use type, has "
            <> show eventPrintedUses
            <> ", but got: "
            <> show tType
      else do
        pushWhen (tType == Horror) $ checkDefeated source a
        pure $ a & tokensL %~ addTokens tType n
  UseAbility _ ab _ | isSource a ab.source || isProxySource a ab.source -> do
    push $ Do msg
    pure a
  InSearch msg'@(UseAbility _ ab _) | isSource a ab.source || isProxySource a ab.source -> do
    push $ Do msg'
    pure a
  InDiscard _ msg'@(UseAbility _ ab _) | isSource a ab.source || isProxySource a ab.source -> do
    push $ Do msg'
    pure a
  InHand _ msg'@(UseAbility _ ab _) | isSource a ab.source || isProxySource a ab.source -> do
    push $ Do msg'
    pure a
  _ -> pure a
