{-# OPTIONS_GHC -Wno-unused-record-wildcards -Wno-unused-imports -Wno-unused-matches -Wno-missing-signatures -Wno-orphans #-}
{-# LANGUAGE TypeAbstractions #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Investigator.Runner.Card where


import Arkham.Ability as X hiding (PaidCost)
import Arkham.ChaosToken as X
import Arkham.ClassSymbol as X
import Arkham.Classes as X
import Arkham.ForMovement
import Arkham.Helpers.Investigator as X
import Arkham.Helpers.Message as X hiding (
  InvestigatorDamage,
  InvestigatorDefeated,
  InvestigatorResigned,
 )
import Arkham.Helpers.Query as X
import Arkham.Id as X
import Arkham.Investigator.Types as X
import Arkham.Name as X
import Arkham.Source as X
import Arkham.Stats as X
import Arkham.Target as X
import Arkham.Trait as X hiding (Cosmos, Cultist, ElderThing, Haunted)
import Data.Aeson (Result (..))
import Data.Aeson.KeyMap qualified as KeyMap

import Arkham.Action (Action)
import Arkham.Action qualified as Action
import Arkham.Action.Additional
import Arkham.Actions (actionsToList)
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Campaign.Option
import Arkham.CampaignLog
import Arkham.Campaigns.EdgeOfTheEarth.Seal
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Capability
import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Card.Settings
import Arkham.Classes.HasGame
import Arkham.CommitRestriction
import Arkham.Constants
import Arkham.Cost qualified as Cost
import Arkham.Customization
import Arkham.Deck qualified as Deck
import Arkham.DefeatedBy
import Arkham.Discard
import Arkham.Discover
import Arkham.Draw.Types
import Arkham.Enemy.Types qualified as Field
import Arkham.Event.Types (Field (..))
import Arkham.Fight.Types
import {-# SOURCE #-} Arkham.Game (asIfTurn, withoutCanModifiers)
import Arkham.Game.Settings (settingsStrictAsIfAt)
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers
import Arkham.Helpers.Ability (
  getAbilityLimit,
  getCanAffordUseWith,
  getCanPerformAbility,
  isForcedAbility,
 )
import Arkham.Helpers.Action (
  additionalActionCovers,
  canDo_,
  getActions,
  getActionsWith,
  getAdditionalActions,
  getCanAfford,
 )
import Arkham.Helpers.Card (
  cardIsFast',
  drawThisCardFrom,
  extendedCardMatch,
  getCardEntityTarget,
  getModifiedCardCost,
  passesLimits,
 )
import Arkham.Helpers.Cost (getCanAffordCost, getSpendableResources, hasSkillTestCost)
import Arkham.Helpers.Criteria (passesCriteria)
import Arkham.Helpers.Deck qualified as Deck
import Arkham.Helpers.Discover
import Arkham.Helpers.Game (withAlteredGame)
import Arkham.Helpers.Location (
  getCanMoveTo,
  getCanMoveToMatchingLocations,
  isDiscoveringLastClue,
  withLocationOf,
 )
import Arkham.Helpers.Log (hasCampaignOption)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Playable (getIsPlayable, getIsPlayableWithResources, getPlayableCards)
import Arkham.Helpers.Ref (sourceToCard)
import Arkham.Helpers.SkillTest
import Arkham.Helpers.Slot (
  canPutIntoSlot,
  emptySlot,
  removeIfMatches,
  removeIfMatchesOnce,
  slotItems,
 )
import Arkham.Helpers.Source (sourceMatches, sourceTraits)
import Arkham.Helpers.Window (
  batchedTimings,
  checkAfter,
  checkWhen,
  checkWindows,
  frame,
  pushBatch,
  pushBatched,
  timings,
  windowMatches,
  wouldDo,
 )
import Arkham.Helpers.Window qualified as Helpers
import Arkham.History
import Arkham.I18n (countVar, ikey', withI18n)
import Arkham.Investigate.Types
import {-# SOURCE #-} Arkham.Investigator
import Arkham.Investigator.Types qualified as Attrs
import Arkham.Key
import Arkham.Keyword (Keyword (Starting))
import Arkham.Location.Types (Field (..))
import Arkham.Matcher (
  basic,
  AssetMatcher (..),
  CardMatcher (..),
  EnemyMatcher (..),
  EventMatcher (..),
  ExtendedCardMatcher (..),
  ForPlay (..),
  InvestigatorMatcher (..),
  LocationMatcher (..),
  ScenarioMatcher (..),
  SourceMatcher (..),
  WindowMatcher (AnyWindow),
  assetControlledBy,
  assetIs,
  at_,
  cardIs,
  colocatedWith,
  enemyEngagedWith,
  inHandOf,
  locationWithInvestigator,
  oneOf,
  orConnected,
  pattern AnyInPlayEnemy,
  pattern AssetWithAnyClues,
 )
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted (obtainCard, takeControlOfAsset)
import Arkham.Message.Lifted qualified as Lifted
import Arkham.Message.Lifted.Choose qualified as Choose
import Arkham.Message.Lifted.Move (moveTo, moveToEdit)
import Arkham.Modifier
import Arkham.Modifier qualified as Modifier
import Arkham.Movement
import Arkham.Phase
import Arkham.Placement
import Arkham.Plural
import Arkham.Prelude
import Arkham.Projection
import Arkham.ScenarioLogKey
import Arkham.Search hiding (drawnCardsL, foundCardsL)
import Arkham.Search qualified as Search
import Arkham.Skill.Types (Field (..))
import Arkham.SkillTest
import Arkham.Slot
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Token qualified as Token
import Arkham.Tracing
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window (Window (..), defaultWindows, mkAfter, mkWhen, mkWindow, primaryWindowTarget)
import Arkham.Window qualified as Window
import Arkham.Zone qualified as Zone
import Control.Lens (each, non, over, sumOf, _Just)
import Control.Monad.State.Strict (evalStateT, get, modify)
import Data.Data.Lens (biplate)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Monoid
import Data.Set qualified as Set
import Data.UUID (nil)
import Arkham.Investigator.Runner.Damage

handleReturnToHand a@InvestigatorAttrs{..} iid aid = do
  pushWhen (providedSlot a aid) $ RefillSlots a.id []
  pure $ a & (slotsL %~ removeFromSlots aid)

handleReturnToHandV2 a@InvestigatorAttrs{..} iid aid = do
  pushWhen (providedSlot a aid) $ RefillSlots a.id []
  pure a

handleReturnToHandV3 a@InvestigatorAttrs{..} iid cardId = do
  card <- getCard cardId
  pushAll [ObtainCard card.id, AddToHand iid [card]]
  pure a

handleReturnToHandV4 a@InvestigatorAttrs{..} iid matcher = do
  cards <- select matcher
  for_ cards \card ->
    pushAll [ObtainCard card.id, AddToHand iid [card]]
  pure a

handleShuffleDeck a@InvestigatorAttrs{..} iid = do
  deck' <- shuffle (unDeck investigatorDeck)
  pure $ a & deckL .~ Deck deck' & foundCardsL . at Zone.FromDeck .~ mempty

handleShuffleDiscardBackIn a@InvestigatorAttrs{..} iid = do
  mods <- getModifiers a
  if null investigatorDiscard || CardsCannotLeaveYourDiscardPile `elem` mods
    then pure a
    else do
      deck <- shuffle (investigatorDiscard <> coerce investigatorDeck)
      pure $ a & discardL .~ [] & deckL .~ Deck deck

handleChooseAndDiscardAsset a@InvestigatorAttrs{..} iid source assetMatcher = do
  discardableAssetIds <- select $ assetControlledBy iid <> DiscardableAsset <> assetMatcher
  player <- getPlayer iid
  pushWhen (notNull discardableAssetIds)
    $ chooseOrRunOne player
    $ targetLabels discardableAssetIds (Only . toDiscardBy iid source)
  pure a

handleAddToDiscard a@InvestigatorAttrs{..} iid pc0 = do
  -- Normalize ownership to this investigator's canonical id. The card may have
  -- been imported under an alternate investigator code (e.g. a Revised Core
  -- printing), which would otherwise leave a mismatched owner on the discarded
  -- copy and break later owner-routed operations.
  let pc = pc0 {pcOwner = Just investigatorId}
  let
    discardF = case cdWhenDiscarded (toCardDef pc) of
      ToDiscard -> discardL %~ nub . (pc :)
      ToBonded -> bondedCardsL %~ nub . (toCard pc :)
      ToSetAside -> id

  when (cdWhenDiscarded (toCardDef pc) == ToSetAside) do
    pushAll [ObtainCard (toCard pc).id, SetAsideCards [toCard pc]]

  pure
    $ a
    & (deckL %~ Deck . filter (/= pc) . unDeck)
    & (handL %~ filter (/= toCard pc))
    & (cardsUnderneathL %~ filter (/= toCard pc))
    & discardF
    & (foundCardsL . each %~ filter (/= PlayerCard pc))

handleDiscardFromHand a@InvestigatorAttrs{..} handDiscard msg = do
  discardableHand <-
    select $ inHandOf NotForPlay investigatorId
      <> CardWithoutModifier CannotLeaveYourHand
      <> basic handDiscard.filter
  when (handDiscard.amount > 0 || (handDiscard.strategy == DiscardAll && notNull discardableHand)) do
    wouldDiscard <- checkWhen $ Window.WouldDiscardFromHand investigatorId handDiscard.source
    pushAll [wouldDiscard, Do msg]
  pure a

handleDoDiscardFromHand a@InvestigatorAttrs{..} handDiscard = do
  player <- getPlayer investigatorId
  let mkMsg card = case handDiscard.destination of
        ToDiscardPile -> DiscardCard investigatorId handDiscard.source (toCardId card)
        ToTopOfDeck -> PutCardOnTopOfDeck investigatorId (Deck.InvestigatorDeck investigatorId) (toCard card)
        ToBottomOfDeck -> PutCardOnBottomOfDeck investigatorId (Deck.InvestigatorDeck investigatorId) (toCard card)
  let pushDiscardedCards cards = when (handDiscard.destination == ToDiscardPile) do
        for_ handDiscard.target \target ->
          push $ DiscardedCards investigatorId handDiscard.source target cards
  case discardableCards a of
    [] | handDiscard.strategy /= DiscardRandom -> pure ()
    cs -> case handDiscard.strategy of
      DiscardChoose -> do
        case handDiscard.filter of
          CardWithId _ -> do
            -- An explicit by-id discard names one specific card, so it must
            -- bypass the voluntary-weakness restriction baked into
            -- discardableCards (which otherwise hides weaknesses when the hand
            -- also holds non-weakness cards).
            let cs' = filterCards handDiscard.filter investigatorHand
            pushAll [mkMsg c | c <- cs']
            pushDiscardedCards cs'
          _ -> do
            let cs' = filterCards handDiscard.filter cs
            let n = min handDiscard.amount (length cs')
            pushWhen (n > 0)
              $ chooseN player n
              $ [ targetLabel c [mkMsg c]
                | c <- cs'
                ]
            pushDiscardedCards cs'
      DiscardAll -> do
        let cards' = filterCards handDiscard.filter cs
        cards <- cards' & filterM (`matches` CardWithoutModifier CannotLeaveYourHand)

        when (notNull cards) do
          push
            $ chooseOneAtATime player
            $ [ targetLabel c [mkMsg c]
              | c <- cards
              ]
          pushDiscardedCards cards
      DiscardRandom -> do
        -- only cards actually in hand
        let filtered' = filterCards handDiscard.filter investigatorHand
        filtered <- filtered' & filterM (`matches` CardWithoutModifier CannotLeaveYourHand)
        for_ (nonEmpty filtered) \targets -> do
          cards <- sampleN handDiscard.amount targets
          pushAll $ map mkMsg cards
          pushDiscardedCards cards
  push $ DoneDiscarding investigatorId
  pure $ a & discardingL ?~ handDiscard

handleDiscard a@InvestigatorAttrs{..} source cardId = do
  push (DiscardCard investigatorId source cardId)
  pure a

handleDiscardV2 a@InvestigatorAttrs{..} cardId = do
  pure $ a & foundCardsL . each %~ filter ((/= cardId) . toCardId)

handleDiscardCard a@InvestigatorAttrs{..} iid source cardId msg = do
  case find ((== cardId) . toCardId) investigatorHand of
    Just card -> do
      inMulligan <- getInMulligan
      beforeWindowMsg <- checkWindows [mkWhen (Window.Discarded (Just iid) source card)]
      afterWindowMsg <- checkWindows [mkAfter (Window.Discarded (Just iid) source card)]
      afterHandWindowMsg <- checkWindows [mkAfter (Window.DiscardedFromHand iid source card)]
      if inMulligan
        then push (Do msg)
        else pushAll [beforeWindowMsg, Do msg, afterWindowMsg, afterHandWindowMsg]
    Nothing -> do
      card <- getCard cardId
      beforeWindowMsg <- checkWindows [mkWhen (Window.Discarded (Just iid) source card)]
      afterWindowMsg <- checkWindows [mkAfter (Window.Discarded (Just iid) source card)]
      pushAll [beforeWindowMsg, Do msg, afterWindowMsg]
  pure a

handleDoDiscardCard a@InvestigatorAttrs{..} iid cardId = do
  case find ((== cardId) . toCardId) investigatorHand of
    Just card -> case card of
      PlayerCard pc -> do
        let
          updateHandDiscard handDiscard =
            handDiscard
              { discardAmount = max 0 (discardAmount handDiscard - 1)
              }
        push $ AddToDiscard iid pc
        pure $ a & handL %~ filter (/= card) & discardingL %~ fmap updateHandDiscard
      EncounterCard _ -> pure $ a & handL %~ filter (/= card) -- TODO: This should discard to the encounter discard
      VengeanceCard _ -> error "vengeance card"
    Nothing -> do
      push $ DiscardedCard cardId
      pure a

handleDoneDiscarding a@InvestigatorAttrs{..} iid = case investigatorDiscarding of
  Nothing -> pure a
  Just handDiscard -> do
    when (discardAmount handDiscard == 0)
      $ for_ (discardThen handDiscard) push
    pure $ a & discardingL .~ Nothing

handleRemoveCardFromHand a@InvestigatorAttrs{..} iid cardId = do
  pure $ a & handL %~ filter ((/= cardId) . toCardId)

handleShuffleIntoDeck a@InvestigatorAttrs{..} iid aid msg = do
  if null investigatorDeck
    then do
      mIsDefeated <- fieldMay AssetIsDefeated aid
      for_ mIsDefeated \isDefeated -> do
        when isDefeated $ Lifted.toDiscard GameSource aid
      pure a
    else do
      card <- field AssetCard aid
      obtainCard card
      push $ RemoveFromPlay (AssetSource aid)
      push $ ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [card]
      push $ After msg
      pushWhen (providedSlot a aid) $ RefillSlots a.id []
      pure $ a & (slotsL %~ removeFromSlots aid)

handleShuffleIntoDeckV2 a@InvestigatorAttrs{..} iid eid msg = do
  if null investigatorDeck
    then do
      placement <- field EventPlacement eid
      when (isOutOfPlayPlacement placement) (Lifted.toDiscard GameSource eid)
    else do
      card <- field EventCard eid
      obtainCard card
      push $ ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [card]
      push $ After msg
      pushWhen (providedSlot a eid) $ RefillSlots a.id []
  pure a

handleShuffleIntoDeckV3 a@InvestigatorAttrs{..} iid aid msg = do
  card <- field SkillCard aid
  obtainCard card
  if toCardCode card == "06113"
    then pure $ a & bondedCardsL %~ (card :)
    else do
      if null investigatorDeck
        then Lifted.addToDiscard iid (only card)
        else do
          Lifted.shuffleCardsIntoDeck iid (only card)
          push $ After msg
      pure a

handleShuffleIntoDeckV4 a@InvestigatorAttrs{..} iid cid = do
  card <- getCard cid
  push $ ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [card]
  pure a

handlePutOnTopOfDeck a@InvestigatorAttrs{..} iid cid = do
  card <- getCard cid
  push $ PutCardOnTopOfDeck iid (Deck.InvestigatorDeck iid) card
  pure a

handlePutOnBottomOfDeck a@InvestigatorAttrs{..} iid cid = do
  card <- getCard cid
  push $ PutCardOnBottomOfDeck iid (Deck.InvestigatorDeck iid) card
  pure a

-- | An investigator "owns" a card when its owner is the investigator's canonical
-- id, or any of the investigator's card codes (primary or alternate).
--
-- The alternate-code match handles alternate printings such as the Revised Core
-- Set, where a card imported from the Revised decklist carries the Revised
-- investigator code (e.g. Roland @01501@) while the investigator entity keeps its
-- canonical id (e.g. @01001@).
--
-- The canonical-id match handles the inverse case, where the investigator's
-- current form differs from its true identity: in Yithian form 'toCardDef'
-- resolves to Body of a Yithian (@04244@), so its 'cardCodes' no longer contain
-- the investigator's own id while the cards remain owned by that canonical id.
-- Plain 'InvestigatorId' equality treats these as distinct, so without this the
-- owner-routed discard would never match and the card would be lost.
investigatorOwnsCardCode :: InvestigatorAttrs -> InvestigatorId -> Bool
investigatorOwnsCardCode a iid =
  iid == investigatorId a || unInvestigatorId iid `elem` (toCardDef a).cardCodes

handleDiscarded a@InvestigatorAttrs{..} aid card = do
  -- TODO: This message is ugly, we should do something different
  -- TODO: There are a number of messages here that mean the asset is no longer in play, we should consolidate to a singular message
  let slotAssets = concatMap (concatMap slotItems) (Map.elems investigatorSlots)
  when (aid `elem` slotAssets) $ do
    -- if we are planning to discard another asset immediately, wait to refill slots
    mmsg <- peekMessage
    case mmsg of
      Just (Discard _ _ (AssetTarget aid')) | aid' `elem` slotAssets -> pure ()
      -- N.B. This is explicitly for Empower Self and it's possible we don't want to do this without checking
      _ -> push $ RefillSlots investigatorId []

  let shouldDiscard =
        maybe False (investigatorOwnsCardCode a) (pcOwner card)
          && card
          `notElem` investigatorDiscard
          && card
          `notElem` fromMaybe [] investigatorSideDeck

  -- Normalize ownership to this investigator's canonical id so the discarded
  -- copy doesn't retain a mismatched alternate-printing owner code.
  let discardedCard = card {pcOwner = Just investigatorId}
  pure $ a & (if shouldDiscard then discardL %~ (discardedCard :) else id) & (slotsL %~ removeFromSlots aid)
  -- Discarded _ _ (PlayerCard card) -> do
  --   let shouldDiscard = pcOwner card == Just investigatorId && card `notElem` investigatorDiscard
  --   if shouldDiscard
  --     then pure $ a & discardL %~ (card :) & handL %~ filter (/= PlayerCard card)
  --     else pure a

handleDiscardedV2 a@InvestigatorAttrs{..} aid = do
  pushWhen (providedSlot a aid) $ RefillSlots a.id []
  pure $ a & (slotsL %~ removeFromSlots aid)

handleDiscardedV3 a@InvestigatorAttrs{..} aid = do
  pushWhen (providedSlot a aid) $ RefillSlots a.id []
  pure a

handleInitDeck a@InvestigatorAttrs{..} iid murl = do
  pure $ a & deckUrlL .~ murl

handleUpgradeDeck a@InvestigatorAttrs{..} iid murl = do
  pure $ a & deckUrlL .~ murl & spentXpL .~ investigatorXp

handleObtainCard a@InvestigatorAttrs{..} cardId = do
  pure
    $ a
    & (handL %~ filter ((/= cardId) . toCardId))
    & (discardL %~ filter ((/= cardId) . toCardId))
    & (deckL %~ Deck . filter ((/= cardId) . toCardId) . unDeck)
    & (cardsUnderneathL %~ filter ((/= cardId) . toCardId))
    & (foundCardsL . each %~ filter ((/= cardId) . toCardId))
    & (bondedCardsL %~ filter ((/= cardId) . toCardId))
    & (decksL . each %~ filter ((/= cardId) . toCardId))

handleReplaceCard a@InvestigatorAttrs{..} cardId card = do
  let doReplace c = if c.id == cardId then card else c
  let
    doReplaceP c =
      case card of
        PlayerCard pc -> if c.id == cardId then pc else c
        _ -> c
  pure
    $ a
    & (handL %~ map doReplace)
    & (discardL %~ map doReplaceP)
    & (deckL %~ map doReplaceP)
    & (cardsUnderneathL %~ map doReplace)
    & (foundCardsL . each %~ map doReplace)
    & (bondedCardsL %~ map doReplace)
    & (decksL . each %~ map doReplace)

handlePutCampaignCardIntoPlay a@InvestigatorAttrs{..} iid cardDef = do
  let mcard = find ((== cardDef) . toCardDef) (unDeck investigatorDeck)
  case mcard of
    Nothing ->
      sendError $ "The game expected to find "
        <> toTitle cardDef
        <> " in the deck of "
        <> toTitle a
        <> ", but was unable to find it."
    Just card -> push $ PutCardIntoPlay iid (PlayerCard card) Nothing NoPayment []
  pure a

handleRemoveAllCopiesOfCardFromGame a@InvestigatorAttrs{..} iid cardCode = do
  assets <- select $ assetControlledBy iid
  for_ assets $ \assetId -> do
    cardCode' <- field AssetCardCode assetId
    when (cardCode == cardCode') (push $ RemoveFromGame (AssetTarget assetId))

  for_ (unDeck investigatorDeck) \card -> do
    when (cardCode == card.cardCode) (push $ RemoveCard card.id)

  for_ investigatorDiscard \card -> do
    when (cardCode == card.cardCode) (push $ RemoveCard card.id)

  for_ investigatorHand \card -> do
    when (cardCode == card.cardCode) (push $ RemoveCard card.id)

  pure a

handlePutCardIntoPlay a@InvestigatorAttrs{..} card = do
  pure
    $ a
    & (deckL %~ Deck . filter ((/= card) . PlayerCard) . unDeck)
    & (discardL %~ filter ((/= card) . PlayerCard))
    & (handL %~ filter (/= card))
    & (bondedCardsL %~ filter (/= card))

handleDiscardTopOfDeck a@InvestigatorAttrs{..} iid n source mTarget = do
  ok <- can.manipulate.deck iid
  if ok
    then do
      -- Push a WouldDiscardFromDeck checkpoint so cards like Little Sylvie can
      -- intercept and replace the upcoming `Do (DiscardTopOfDeck ...)` with a
      -- put-on-top-of-deck. If nothing reacts, the Do branch runs as before.
      wouldWindow <-
        checkWindows [mkWindow #when (Window.WouldDiscardFromDeck iid source)]
      pushAll [wouldWindow, Do (DiscardTopOfDeck iid n source mTarget)]
      pure a
    else pure a

handleDoDiscardTopOfDeck a@InvestigatorAttrs{..} iid n source mTarget = do
  ok <- can.manipulate.deck iid
  if ok
    then do
      let (cs, deck') = draw n investigatorDeck
          (cs', essenceOfTheDreams) = partition ((/= "06113") . toCardCode) cs
      windowMsgs <-
        if null deck'
          then
            pure
              <$> checkWindows
                ((`mkWindow` Window.DeckHasNoCards iid) <$> [#when, #after])
          else pure []
      discardedFromDeckWindow <-
        checkWindows [mkAfter (Window.DiscardedFromDeck iid source (toCard c)) | c <- cs']
      pushAll
        $ windowMsgs
        <> [DeckHasNoCards investigatorId mTarget | null deck']
        <> [ DiscardedTopOfDeck iid cs source target
           | target <- maybeToList mTarget
           ]
        <> [discardedFromDeckWindow | notNull cs']
      pure
        $ a
        & (deckL .~ deck')
        & (discardL %~ (reverse cs' <>))
        & (bondedCardsL <>~ map toCard essenceOfTheDreams)
        & (foundCardsL . each %~ filter (`notElem` map toCard cs'))
    else pure a

handleDrawStartingHand a@InvestigatorAttrs{..} iid = do
  modifiers' <- getModifiers (toTarget a)
  if any (`elem` modifiers') [CannotDrawCards, CannotManipulateDeck]
    then pure a
    else do
      let
        startingHandAmount = foldr applyModifier 5 modifiers'
        applyModifier (StartingHand m) n = max 0 (n + m)
        applyModifier _ n = n
        preExistingHand = map toCardId investigatorHand
      (discard, hand, deck) <- drawOpeningHand a startingHandAmount
      pure
        $ a
        & (discardL .~ discard)
        & (handL .~ hand)
        & (deckL .~ Deck deck)
        & (excludeFromMulliganL .~ preExistingHand)

handleDrawCards a@InvestigatorAttrs{..} iid cardDraw = do
  cid <- getRandom
  phase <- getPhase
  wouldDrawCard <-
    checkWindows $ mkWhen (Window.WouldDrawCard iid cid cardDraw.deck)
      : [mkWhen (Window.WouldDrawExactlyOneCard iid cid cardDraw.deck) | cardDraw.amount == 1]
  drawEncounterCardWindow <- checkWindows [mkWhen $ Window.WouldDrawEncounterCard a.id cid phase]
  if cardDrawAction cardDraw
    then do
      modifiers' <- getModifiers iid
      beforeWindowMsg <- checkWindows [mkWhen (Window.PerformAction iid #draw)]
      afterWindowMsg <- checkWindows [mkAfter (Window.PerformAction iid #draw)]
      pushAll
        $ [BeginAction, beforeWindowMsg]
        <> [drawEncounterCardWindow | cardDraw.isEncounterDraw]
        <> [TakeActions iid [#draw] (ActionCost 1)]
        <> [ Will (CheckAttackOfOpportunity iid False Nothing)
           | ActionDoesNotCauseAttacksOfOpportunity #draw `notElem` modifiers'
           ]
        <> [ CheckAttackOfOpportunity iid False Nothing
           | ActionDoesNotCauseAttacksOfOpportunity #draw `notElem` modifiers'
           ]
        <> [ wouldDrawCard
           , DoDrawCards iid
           , DrawEnded cid iid
           , afterWindowMsg
           , FinishAction
           , TakenActions iid [#draw]
           ]
    else
      pushAll $ wouldDrawCard
        : [drawEncounterCardWindow | cardDraw.isEncounterDraw] <> [DoDrawCards iid, DrawEnded cid iid]
  pure $ a & drawingL ?~ cardDraw

handleMoveTopOfDeckToBottom a@InvestigatorAttrs{..} iid n = do
  let (cards, deck) = draw n investigatorDeck
  pure $ a & deckL .~ Deck.withDeck (<> cards) deck

handleDoDrawCards a@InvestigatorAttrs{..} iid = do
  for_ (a ^. drawingL) \d -> do
    push $ Do (DrawCards iid d)
    for_ (cardDrawAndThen d) push
  pure $ a & drawingL .~ Nothing

handleReplaceCurrentCardDraw a@InvestigatorAttrs{..} iid drawing = do
  pure $ a & drawingL ?~ drawing

handleDoDrawCardsV2 a@InvestigatorAttrs{..} iid cardDraw = do
  case cardDraw.kind of
    StartingHandCardDraw -> do
      modifiers' <- getModifiers (toTarget a)
      if any (`elem` modifiers') [CannotDrawCards, CannotManipulateDeck]
        then pure a
        else do
          let
            startingHandAmount = foldr applyModifier 5 modifiers'
            applyModifier (StartingHand m) n = max 0 (n + m)
            applyModifier _ n = n
          (discard, hand, deck) <- drawOpeningHand a startingHandAmount
          pure
            $ a
            & (discardL .~ discard)
            & (handL .~ hand)
            & (deckL .~ Deck deck)
    StandardCardDraw -> do
      -- RULES: When a player draws two or more cards as the result of a single
      -- ability or game step, those cards are drawn simultaneously. If a deck
      -- empties middraw, reset the deck and complete the draw.

      -- RULES: If an investigator with an empty investigator deck needs to draw
      -- a card, that investigator shuffles his or her discard pile back into his
      -- or her deck, then draws the card, and upon completion of the entire draw
      -- takes one horror.

      let n = cardDrawAmount cardDraw

      modifiers' <- getModifiers (toTarget a)

      -- Shared finalization for a completed card draw. Takes all drawn cards
      -- (before the cardDraw.discard filter) and the remaining deck, then
      -- pushes the appropriate messages and returns the updated attrs.
      let
        finalizedDraw allBeforeFilter deck' = do
          let
            (discarded, allDrawn) =
              maybe
                ([], allBeforeFilter)
                (\mtch -> partition (`cardMatch` mtch) allBeforeFilter)
                cardDraw.discard
            doShuffleBackInEachWeakness = ShuffleBackInEachWeakness `elem` cardDrawRules cardDraw
            handleCard c = pure $ drawThisCardFrom iid c (Just cardDraw.deck)
          msgs <- if not doShuffleBackInEachWeakness then concatMapM handleCard allDrawn else pure []
          player <- getPlayer iid
          let
            weaknesses = map PlayerCard $ filter (`cardMatch` WeaknessCard) allDrawn
            msgs' =
              (<> msgs)
                $ guard (doShuffleBackInEachWeakness && notNull weaknesses)
                *> [ FocusCards weaknesses
                   , chooseOne
                       player
                       [ Label
                           "Shuffle Weaknesses back in"
                           [UnfocusCards, ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) weaknesses]
                       ]
                   ]
          windowMsgs <-
            if null deck'
              then pure <$> checkWindows ((`mkWindow` Window.DeckHasNoCards iid) <$> [#when, #after])
              else pure []
          let (before, _, after) = frame $ Window.DrawCards iid $ map toCard allDrawn
          checkHandSize <- hasModifier iid CheckHandSizeAfterDraw
          -- Cards with revelations won't be in hand after they resolve, so exclude them from the discard
          let
            toDrawDiscard = \case
              AfterDrawDiscard x -> Sum x
              _ -> mempty
            discardable = filter (`cardMatch` (DiscardableCard <> NotCard CardWithRevelation)) allDrawn
            discardAmount =
              min (length discardable) $ getSum (foldMap toDrawDiscard (toList $ cardDrawRules cardDraw))
            -- Only focus those that will still be in hand
            focusable = map toCard $ filter (`cardMatch` NotCard CardWithRevelation) allDrawn
          pushAll
            $ windowMsgs
            <> [DeckHasNoCards iid Nothing | null deck']
            <> [before]
            <> [DiscardCard iid (cardDrawSource cardDraw) card.id | card <- discarded]
            <> msgs'
            <> [after]
            <> [UpdateHistory iid $ HistoryItem HistoryCardsDrawn (length allDrawn)]
            <> ( guard (discardAmount > 0)
                   *> [ FocusCards focusable
                      , chooseN
                          player
                          discardAmount
                          [targetLabel card [DiscardCard iid (toSource a) card.id] | card <- discardable]
                      , UnfocusCards
                      ]
               )
            <> [CheckHandSize iid | checkHandSize]
          pure
            $ a
            & (handL %~ (<> map PlayerCard (filter (`cardMatch` NotCard CardWithRevelation) allDrawn)))
            & (deckL .~ Deck deck')
            & (drawnCardsL .~ mempty)
            & (foundCardsL . each %~ filter (`notElem` map toCard allDrawn))

      -- Continue an in-progress draw after a reshuffle / partial draw,
      -- preserving rules and filters from the original draw (e.g.
      -- AfterDrawDiscard) but resetting per-draw bookkeeping so the
      -- remaining cards are drawn cleanly. cardDrawAndThen is dropped
      -- because the original DoDrawCards already pushed it.
      let
        continueDraw amount =
          DrawCards iid
            $ cardDraw
              { cardDrawAmount = amount
              , cardDrawState = UnresolvedCardDraw
              , cardDrawAndThen = Nothing
              , cardDrawAlreadyDrawn = []
              , cardDrawAction = False
              }
      if null investigatorDeck
        then do
          -- What happens if the Yorick player has Graveyard Ghouls engaged
          -- with him or her and runs out of deck? "If an investigator with an
          -- empty investigator deck needs to draw a card, that investigator
          -- shuffles his or her discard pile back into his or her deck, then
          -- draws the card, and upon completion of the entire draw takes one
          -- horror. In this case, you cannot shuffle your discard pile into
          -- your deck, so you will neither draw 1 card, nor will you take 1
          -- horror."
          let canShuffle = not (null investigatorDiscard || CardsCannotLeaveYourDiscardPile `elem` modifiers')
          if canShuffle
            then do
              wouldDo
                (EmptyDeck iid (Just $ continueDraw n))
                (Window.DeckWouldRunOutOfCards iid)
                (Window.DeckHasNoCards iid)
              pure a
            else
              if null investigatorDrawnCards
                then pure a
                else finalizedDraw investigatorDrawnCards []
        else do
          let deck = unDeck investigatorDeck
          if length deck < n
            then do
              push $ continueDraw (n - length deck)
              pure $ a & deckL .~ mempty & drawnCardsL %~ (<> deck)
            else do
              let (drawn, deck') = splitAt n deck
              finalizedDraw (investigatorDrawnCards <> drawn) deck'

handleInvestigatorDrewPlayerCardFrom a@InvestigatorAttrs{..} iid card mDeck msg = do
  hasForesight <- hasModifier iid (Foresight $ toTitle card)
  let uiRevelation = getPlayer iid >>= (`sendRevelation` (toJSON $ toCard card))
  case toCardType card of
    PlayerEnemyType -> pure ()
    _ -> when (hasRevelation card) uiRevelation
  mWhenDraw <- for mDeck \deck ->
    checkWindows [mkWhen $ Window.DrawCard iid (toCard card) deck]
  if hasForesight
    then do
      canCancel <- PlayerCard card <=~> CanCancelRevelationEffect (InvestigatorWithId iid) #any
      availableResources <- getSpendableResources iid
      player <- getPlayer iid
      playable <-
        getIsPlayableWithResources
          iid
          GameSource
          (availableResources + 2)
          (Cost.UnpaidCost NoAction)
          (Window.defaultWindows iid)
          (toCard card)
      reduceCost <- costModifier iid iid (ReduceCostOf (CardWithId card.id) 2)
      pushAll
        $ FocusCards [toCard card]
        : [ chooseOrRunOne player
              $ [ Label
                    "Cancel card effects and discard it"
                    [ UnfocusCards
                    , CancelNext GameSource RevelationMessage
                    , DiscardCard iid GameSource card.id
                    ]
                | canCancel
                ]
              <> [ Label
                     "Immediately play that card at -2 cost"
                     [ UnfocusCards
                     , reduceCost
                     , PayCardCost iid (toCard card) (Window.defaultWindows iid)
                     ]
                 | playable
                 ]
              <> [Label "$label.drawNormally" $ maybeToList mWhenDraw <> [UnfocusCards, Do msg]]
          ]
    else pushAll $ FocusCards [toCard card] : maybeToList mWhenDraw <> [UnfocusCards, Do msg]
  pure a

handleDoInvestigatorDrewPlayerCardFrom a@InvestigatorAttrs{..} iid card mdeck = do
  afterDraw <-
    checkWindows [mkAfter $ Window.DrawCard iid (toCard card) (fromMaybe Deck.NoDeck mdeck)]
  inLimit <- passesLimits iid (toCard card)
  if hasRevelation card && inLimit
    then
      pushAll
        $ if toCardType card == PlayerTreacheryType
          then [DrewTreachery iid Nothing (toCard card), afterDraw]
          else [Revelation iid (CardIdSource card.id), afterDraw, ResolvedCard iid $ toCard card]
    else
      if toCardType card == PlayerEnemyType
        then pushAll [DrewPlayerEnemy iid (toCard card), afterDraw]
        else push afterDraw

  let
    cardFilter :: IsCard c => [c] -> [c]
    cardFilter = filter ((/= card.id) . toCardId)
  -- Cards drawn simultaneously are placed in hand up front (handleDoDrawCardsV2),
  -- then this deferred handler finalizes each draw. An effect resolving between
  -- those two steps can relocate a just-drawn card before we get here -- e.g. a
  -- sibling weakness's revelation that grants an action to play it (At a
  -- Crossroads), or a random discard triggered during that action. By then the
  -- card has settled into a final zone, and finalizing the draw would corrupt
  -- state: re-adding it to hand leaves a phantom duplicate of the in-play copy,
  -- and the zone strips below pull it back out of the discard, orphaning it. So
  -- only finalize while the card is still in flight; if it has settled in play, or
  -- (having been discarded out from under us) in the discard pile, leave it be.
  inPlay <- isJust <$> getCardEntityTarget (toCard card)
  let
    drawnFromDiscard = case mdeck of
      Just (Deck.InvestigatorDiscard _) -> True
      _ -> False
    settledElsewhere =
      inPlay || (not drawnFromDiscard && card.id `elem` map toCardId investigatorDiscard)
  doCheck <- hasModifier iid CheckHandSizeAfterDraw
  when doCheck $ push $ CheckHandSize iid
  pure
    $ if settledElsewhere
      then a
      else
        a
          & (handL %~ nub . (toCard card :))
          & (deckL %~ filter ((/= card.id) . toCardId))
          & (foundCardsL . each %~ cardFilter)
          & (cardsUnderneathL %~ cardFilter)
          & (discardL %~ cardFilter)
          & (bondedCardsL %~ cardFilter)

handleEmptyDeck a@InvestigatorAttrs{..} iid = do
  modifiers' <- getModifiers (toTarget a)
  pushWhen (CardsCannotLeaveYourDiscardPile `notElem` modifiers')
    $ ShuffleDiscardBackIn iid
  pure a

handleLoadDeck a@InvestigatorAttrs{..} iid deck = do
  let deck' = flip map (unDeck deck) \card -> card {pcOwner = Just iid}
  pure $ a & deckL .~ Deck deck'

handleLoadSideDeck a@InvestigatorAttrs{..} iid deck = do
  pure $ a & sideDeckL ?~ deck

handlePutCardOnTopOfDeck a@InvestigatorAttrs{..} iid card = do
  case card of
    PlayerCard pc ->
      pure
        $ a
        & (deckL %~ Deck . (pc :) . filter (/= pc) . unDeck)
        & handL
        %~ filter (/= card)
        & discardL
        %~ filter (/= pc)
        & (foundCardsL . each %~ filter (/= PlayerCard pc))
    EncounterCard _ ->
      error "Can not put encounter card on top of investigator deck"
    VengeanceCard _ ->
      error "Can not put vengeance card on top of investigator deck"

handlePutCardOnTopOfDeckV2 a@InvestigatorAttrs{..} card = case card of
  PlayerCard pc ->
    pure
      $ a
      & (deckL %~ Deck . filter (/= pc) . unDeck)
      & (handL %~ filter (/= card))
      & (discardL %~ filter (/= pc))
      & (foundCardsL . each %~ filter (/= PlayerCard pc))
  EncounterCard _ -> pure $ a & handL %~ filter (/= card)
  VengeanceCard vcard -> pure $ a & handL %~ filter (/= vcard)

handlePutCardOnBottomOfDeck a@InvestigatorAttrs{..} iid card = do
  case card of
    PlayerCard pc ->
      pure
        $ a
        & (deckL %~ Deck . (<> [pc]) . filter (/= pc) . unDeck)
        & (handL %~ filter (/= card))
        & (discardL %~ filter ((/= pc.id) . toCardId))
        & (foundCardsL . each %~ filter (/= PlayerCard pc))
    EncounterCard _ ->
      error "Can not put encounter card on bottom of investigator deck"
    VengeanceCard _ ->
      error "Can not put vengeance card on bottom of investigator deck"

handlePutCardOnBottomOfDeckV2 a@InvestigatorAttrs{..} card = case card of
  PlayerCard pc ->
    pure
      $ a
      & (deckL %~ Deck . filter (/= pc) . unDeck)
      & (handL %~ filter (/= card))
      & (discardL %~ filter (/= pc))
      & (foundCardsL . each %~ filter (/= PlayerCard pc))
  EncounterCard _ -> pure a
  VengeanceCard _ -> pure a

handleDrawToHandFrom a@InvestigatorAttrs{..} iid deck cards = do
  let (before, _, after) = frame $ Window.DrawCards iid $ map toCard cards
  push before
  for_ (reverse cards) \case
    PlayerCard pc -> push $ InvestigatorDrewPlayerCardFrom iid pc (Just deck)
    EncounterCard ec -> push $ InvestigatorDrewEncounterCard iid ec
    VengeanceCard {} -> error "Can not add vengeance card to hand"
  when (isNothing $ a ^. searchL) do
    push after
  assetIds <- catMaybes <$> for cards (selectOne . AssetWithCardId . toCardId)
  pure
    $ a
    & (cardsUnderneathL %~ filter (`notElem` cards))
    & (slotsL %~ flip (foldr removeFromSlots) assetIds)
    & (discardL %~ filter ((`notElem` cards) . PlayerCard))
    & (foundCardsL . each %~ filter (`notElem` cards))
    & (bondedCardsL %~ filter (`notElem` cards))
    & (searchL . _Just . Search.drawnCardsL %~ (<> cards))

handleDrawToHand a@InvestigatorAttrs{..} iid cards = do
  let (before, _, after) = frame $ Window.DrawCards iid $ map toCard cards
  push before
  for_ (reverse cards) \case
    PlayerCard pc -> push $ InvestigatorDrewPlayerCardFrom iid pc Nothing
    EncounterCard ec -> push $ InvestigatorDrewEncounterCard iid ec
    VengeanceCard {} -> error "Can not add vengeance card to hand"
  when (isNothing $ a ^. searchL) do
    push after
  assetIds <- catMaybes <$> for cards (selectOne . AssetWithCardId . toCardId)
  pure
    $ a
    & (cardsUnderneathL %~ filter (`notElem` cards))
    & (slotsL %~ flip (foldr removeFromSlots) assetIds)
    & (discardL %~ filter ((`notElem` cards) . PlayerCard))
    & (foundCardsL . each %~ filter (`notElem` cards))
    & (bondedCardsL %~ filter (`notElem` cards))
    & (searchL . _Just . Search.drawnCardsL %~ (<> cards))

handleAddToHand a@InvestigatorAttrs{..} iid cards msg = do
  for_ cards obtainCard
  push $ Do msg
  pure a

handleDoAddToHand a@InvestigatorAttrs{..} iid cards = do
  assetIds <- catMaybes <$> for cards (selectOne . AssetWithCardId . toCardId)
  for_ cards \card -> do
    inLimit <- passesLimits iid (toCard card)
    if hasRevelation card && inLimit
      then
        if toCardType card == PlayerTreacheryType
          then push $ DrewTreachery iid Nothing (toCard card)
          else pushAll [Revelation iid (CardIdSource card.id), ResolvedCard iid $ toCard card]
      else when (toCardType card == PlayerEnemyType) do
        push $ DrewPlayerEnemy iid (toCard card)
  pure
    $ a
    & (cardsUnderneathL %~ filter (`notElem` cards))
    & (slotsL %~ flip (foldr removeFromSlots) assetIds)
    & (discardL %~ filter ((`notElem` cards) . PlayerCard))
    & (foundCardsL . each %~ filter (`notElem` cards))
    & (bondedCardsL %~ filter (`notElem` cards))
    & (handL %~ (<> cards))
    & (decksL . each %~ filter (`notElem` cards))

handleShuffleCardsIntoDeck a@InvestigatorAttrs{..} iid = do
  -- can't shuffle zero cards
  pure a

handleShuffleCardsIntoDeckV2 a@InvestigatorAttrs{..} iid cards = do
  let (cards', essenceOfTheDreams) = partition ((/= "06113") . toCardCode) $ mapMaybe (preview _PlayerCard) cards
  deck <- shuffleM $ cards' <> filter (`notElem` cards') (unDeck investigatorDeck)
  pure
    $ a
    & deckL
    .~ Deck deck
    & handL
    %~ filter (`notElem` cards)
    & cardsUnderneathL
    %~ filter (`notElem` cards)
    & bondedCardsL
    %~ filter (`notElem` cards)
    & bondedCardsL
    <>~ map toCard essenceOfTheDreams
    & discardL
    %~ filter ((`notElem` cards) . PlayerCard)
    & (foundCardsL . each %~ filter (`notElem` cards))

handleAddFocusedToHand a@InvestigatorAttrs{..} iid' cardSource cardId = do
  let
    card =
      fromJustNote "missing card"
        $ find ((== cardId) . toCardId) (findWithDefault [] cardSource $ a ^. foundCardsL)
    foundCards' = Map.map (filter ((/= cardId) . toCardId)) (a ^. foundCardsL)
  push $ addToHand iid' card
  pure $ a & foundCardsL .~ foundCards' & (deckL %~ Deck . filter ((/= card) . toCard) . unDeck)

handleDrawFocusedToHand a@InvestigatorAttrs{..} iid' cardSource cardId = do
  let
    card =
      fromJustNote "missing card"
        $ find ((== cardId) . toCardId) (findWithDefault [] cardSource $ a ^. foundCardsL)
    foundCards' = Map.map (filter ((/= cardId) . toCardId)) (a ^. foundCardsL)
  push $ case zoneToDeck a.id cardSource of
    Nothing -> drawToHand iid' card
    Just deck -> drawToHandFrom iid' deck card
  pure $ a & foundCardsL .~ foundCards' & (deckL %~ Deck . filter ((/= card) . toCard) . unDeck)

handleAddFocusedToTopOfDeck a@InvestigatorAttrs{..} iid' cardId = do
  let
    card =
      fromJustNote "missing card"
        $ find ((== cardId) . toCardId) (concat $ toList $ a ^. foundCardsL)
        >>= toPlayerCard
    foundCards = Map.map (filter ((/= cardId) . toCardId)) $ a ^. foundCardsL
  push $ PutCardOnTopOfDeck iid' (Deck.InvestigatorDeck iid') (toCard card)
  pure $ a & foundCardsL .~ foundCards

handleShuffleAllFocusedIntoDeck a@InvestigatorAttrs{..} iid' = do
  let cards = findWithDefault [] Zone.FromDeck $ a ^. foundCardsL
  push $ ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid') cards
  pure $ a & foundCardsL %~ deleteMap Zone.FromDeck

handlePutAllFocusedIntoDiscard a@InvestigatorAttrs{..} iid' = do
  let cards = onlyPlayerCards $ findWithDefault [] Zone.FromDiscard $ a ^. foundCardsL
  let (cards', essenceOfTheDreams) = partition ((/= "06113") . toCardCode) cards
  pure
    $ a
    & foundCardsL
    %~ deleteMap Zone.FromDiscard
    & discardL
    <>~ cards'
    & bondedCardsL
    <>~ map toCard essenceOfTheDreams

handleRemoveFromDiscard a@InvestigatorAttrs{..} iid cardId = do
  pure $ a & discardL %~ filter ((/= cardId) . toCardId)

handleRemoveFromBearersDeckOrDiscard a@InvestigatorAttrs{..} card = do
  pure $ a & (discardL %~ filter (/= card)) & (deckL %~ Deck . filter (/= card) . unDeck)

handleRemovePlayerCardFromGame a@InvestigatorAttrs{..} card = do
  case preview _PlayerCard card of
    Just pc ->
      pure
        $ a
        & (discardL %~ filter (/= pc))
        & (handL %~ filter (/= card))
        & (deckL %~ Deck . filter (/= pc) . unDeck)
        & (foundCardsL . each %~ filter (/= card))
    Nothing ->
      -- encounter cards can only be in hand
      pure $ a & (handL %~ filter (/= card))

zoneToDeck :: InvestigatorId -> Zone.Zone -> Maybe Deck.DeckSignifier
zoneToDeck iid = \case
  Zone.FromDeck -> Just $ Deck.toDeck iid
  Zone.FromTopOfDeck {} -> Just $ Deck.toDeck iid
  Zone.FromBottomOfDeck {} -> Just $ Deck.toDeck iid
  Zone.FromHand -> Nothing
  Zone.FromDiscard -> Nothing
  Zone.FromPlay -> Nothing
  Zone.FromOutOfPlay {} -> Nothing
  Zone.FromCollection -> Nothing
