{-# OPTIONS_GHC -Wno-unused-record-wildcards -Wno-unused-imports -Wno-unused-matches -Wno-missing-signatures -Wno-orphans #-}
{-# LANGUAGE TypeAbstractions #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Investigator.Runner.Search where


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

handleRemoveCardFromSearch a@InvestigatorAttrs{..} iid cardId = do
  pure $ a & foundCardsL %~ Map.map (filter ((/= cardId) . toCardId))

handleDiscardUntilFirst a@InvestigatorAttrs{..} iid' source iid matcher = do
  (discards, remainingDeck) <- breakM (`extendedCardMatch` matcher) (unDeck investigatorDeck)
  let (discards', essenceOfTheDreams) = partition ((/= "06113") . toCardCode) discards
  case remainingDeck of
    [] -> do
      pushAll [RequestedPlayerCard iid' source Nothing discards, DeckHasNoCards iid Nothing]
      pure
        $ a
        & deckL
        .~ mempty
        & discardL
        %~ (reverse discards' <>)
        & bondedCardsL
        <>~ map toCard essenceOfTheDreams
    (x : xs) -> do
      push (RequestedPlayerCard iid' source (Just x) discards)
      pure
        $ a
        & deckL
        .~ Deck xs
        & discardL
        %~ (reverse discards <>)
        & bondedCardsL
        <>~ map toCard essenceOfTheDreams

handleRevealUntilFirst a@InvestigatorAttrs{..} iid source iid' matcher = do
  (revealed, remainingDeck) <- breakM ((<=~> matcher) . toCard) (unDeck investigatorDeck)
  case remainingDeck of
    [] -> do
      pushAll
        [ RevealedCards iid source (Deck.InvestigatorDeck iid') Nothing (map PlayerCard revealed)
        , DeckHasNoCards iid Nothing
        ]
      pure $ a & deckL .~ mempty
    (x : xs) -> do
      push
        $ RevealedCards
          iid
          source
          (Deck.InvestigatorDeck iid')
          (Just $ PlayerCard x)
          (map PlayerCard revealed)
      pure $ a & deckL .~ Deck xs

handleUpdateSearchReturnStrategy a@InvestigatorAttrs{..} iid zone returnStrategy = do
  let
    updateZone = \case
      (z@(FromTopOfDeck _), _) | zone == FromDeck -> (z, returnStrategy)
      (z@(FromBottomOfDeck _), _) | zone == FromDeck -> (z, returnStrategy)
      (z, _) | z == zone -> (z, returnStrategy)
      other -> other
  case investigatorSearch of
    Nothing -> pure a
    Just s -> pure $ a & searchL ?~ s {searchZones = map updateZone (searchZones s)}

handleEndSearch a@InvestigatorAttrs{..} iid iid' = do
  let cardSources = maybe [] searchZones investigatorSearch
  let
    foundKey = \case
      Zone.FromTopOfDeck _ -> Zone.FromDeck
      Zone.FromBottomOfDeck _ -> Zone.FromDeck
      other -> other
  player <- getPlayer iid
  for_ cardSources $ \(cardSource, returnStrategy) -> case returnStrategy of
    DiscardRest -> do
      let discards = findWithDefault [] Zone.FromDeck $ a ^. foundCardsL
      unless (null discards) do
        push
          $ chooseOneAtATime player
          $ map
            ( \case
                PlayerCard c -> targetLabel (toCardId c) [AddToDiscard iid c]
                EncounterCard c -> targetLabel (toCardId c) [AddToEncounterDiscard c]
                VengeanceCard _ -> error "not possible"
            )
            discards
    PutBackInAnyOrder -> do
      when
        (foundKey cardSource /= Zone.FromDeck)
        (error "Expects a deck: Investigator<PutBackInAnyOrder>")
      push
        $ chooseOneAtATime player
        $ mapTargetLabelWith
          toCardId
          (\c -> [AddFocusedToTopOfDeck iid (toTarget iid') (toCardId c)])
          (findWithDefault [] Zone.FromDeck $ a ^. foundCardsL)
    PutBackInAnyOrderBothTopAndBottom -> do
      when
        (foundKey cardSource /= Zone.FromDeck)
        (error "Expects a deck: Investigator<PutBackInAnyOrderBothTopAndBottom>")
      case findWithDefault [] Zone.FromDeck $ a ^. foundCardsL of
        [] -> pure ()
        remaining ->
          push
            $ chooseOneAtATime player
            $ mapTargetLabelWith
              toCardId
              ( \c ->
                  [ chooseOne
                      player
                      [ Label "$label.placeOnTop" [AddFocusedToTopOfDeck iid (toTarget iid') (toCardId c)]
                      , Label "$label.placeOnBottom" [PutCardOnBottomOfDeck iid (Deck.InvestigatorDeck iid') (toCard c)]
                      ]
                  ]
              )
              remaining
    ShuffleBackIn -> do
      when (foundKey cardSource /= Zone.FromDeck) (error "Expects a deck: Investigator<ShuffleBackIn>")
      for_ investigatorSearch \MkSearch {searchType} ->
        pushWhen (searchType == Searching) $ ShuffleDeck (Deck.InvestigatorDeck a.id)
    PutBack -> pure () -- Nothing moves while searching
    DoNothing -> pure () -- Nothing moves while searching
    RemoveRestFromGame -> do
      -- Try to obtain, then don't add back
      pushAll $ map (ObtainCard . toCardId) $ findWithDefault [] Zone.FromDeck (a ^. foundCardsL)

  push (SearchEnded $ toTarget iid)
  pure
    $ a
    & usedAbilitiesL
    %~ filter
      ( \UsedAbility {..} ->
          case abilityLimitType (abilityLimit usedAbility) of
            Just (PerSearch _) -> False
            _ -> True
      )

handleEndSearchV2 a@InvestigatorAttrs{..} iid = do
  pure
    $ a
    & usedAbilitiesL
    %~ filter
      ( \UsedAbility {..} ->
          case abilityLimitType (abilityLimit usedAbility) of
            Just (PerSearch _) -> False
            _ -> True
      )

handleSearchEnded a@InvestigatorAttrs{..} = do
  case investigatorSearch of
    Just search' -> do
      when (notNull $ search' ^. Search.drawnCardsL) do
        pushM
          $ checkWindows
            [mkAfter $ Window.DrawCards search'.investigator $ search' ^. Search.drawnCardsL]
    _ -> pure ()

  pure $ a & searchL .~ Nothing

handleCancelSearch a@InvestigatorAttrs{..} = pure $ a & searchL .~ Nothing

handleSearch a@InvestigatorAttrs{..} searchType iid iid' zones msg = do
  let deck = Deck.InvestigatorDeck iid'
  if searchType == Searching && any (zoneIsFromDeck . fst) zones
    then wouldDo msg (Window.WouldSearchDeck iid deck) (Window.SearchedDeck iid deck)
    else do
      batchId <- getRandom
      push $ DoBatch batchId msg

  pure a

handleResolveSearch a@InvestigatorAttrs{..} = do
  case investigatorSearch of
    Just
      ( MkSearch
          searchType
          iid
          source
          (InvestigatorTarget iid')
          searchZones
          cardMatcher
          foundStrategy
          foundCards
          _drawnCards
        ) -> do
        mods <- getModifiers iid
        let
          applyMod (AdditionalTargets n) = over biplate (+ n)
          applyMod _ = id
          foundStrategy' = foldr applyMod foundStrategy mods
        targetCards <- traverse (filterM (`extendedCardMatch` cardMatcher)) foundCards

        player <- getPlayer iid
        case foundStrategy' of
          AddToHandOrPlayFound who n -> do
            let windows' = [mkWhen Window.NonFast, mkWhen (Window.DuringTurn iid)]
            playableCards <- concatForM (mapToList targetCards) $ \(_, cards) ->
              filterM (getIsPlayable who source (UnpaidCost NoAction) windows') cards
            let choices = [card | (_, cards) <- mapToList targetCards, card <- cards]
            if null choices
              then Lifted.promptI iid "noCardsFound" Choose.nothing
              else Choose.chooseNM iid (min n (length choices)) do
                Choose.targets choices \card -> do
                  Choose.chooseOrRunOneM iid do
                    Choose.labeledI "addToHandSimple" $ Lifted.addToHand iid (only card)
                    when (card `elem` playableCards) do
                      Choose.labeledI "playCard" $ Lifted.playCardPayingCost iid card
          DrawOrCommitFound who n -> do
            -- [TODO] We need this to determine what state the skill test
            -- is in, if we are committing cards we need to use
            -- SkillTestCommitCard instead of CommitCard
            committable <- filterM (getIsCommittable who) $ concatMap snd $ mapToList targetCards
            let
              choices =
                [ targetLabel
                    card
                    [ if card `elem` committable
                        then
                          chooseOne
                            player
                            [ Label "$label.drawIt" [drawFoundToHand]
                            , Label "$label.commitToSkillTest" [CommitCard who card]
                            ]
                        else drawFoundToHand
                    ]
                | (zone, cards) <- mapToList targetCards
                , card <- cards
                , let drawFoundToHand = DrawFocusedToHand iid (toTarget who) zone (toCardId card)
                ]
            push
              $ if null choices
                then chooseOne player [Label "$label.noCardsFound" []]
                else chooseN player (min n (length choices)) choices
          RemoveFoundFromGame _ n -> do
            let
              choices =
                [ targetLabel (toCardId card) [RemovePlayerCardFromGame False card]
                | (_, cards) <- mapToList targetCards
                , card <- cards
                ]
            push
              $ if null choices
                then chooseOne player [Label "$label.noCardsFound" []]
                else chooseN player (min n (length choices)) choices
          AddFoundToHand who n -> do
            let
              choices =
                [ targetLabel
                    card
                    [AddFocusedToHand iid (toTarget who) zone (toCardId card)]
                | (zone, cards) <- mapToList targetCards
                , card <- cards
                ]
            push
              $ if null choices
                then chooseOne player [Label "$label.noCardsFound" []]
                else chooseN player (min n (length choices)) choices
          DrawFound who n -> do
            canModify <- can.draw.cards iid
            let
              choices =
                [ targetLabel
                    card
                    [DrawFocusedToHand iid (toTarget who) zone (toCardId card)]
                | canModify
                , (zone, cards) <- mapToList targetCards
                , card <- cards
                ]
            push
              $ if null choices
                then chooseOne player [Label "$label.noCardsFound" []]
                else chooseN player (min n (length choices)) choices
            let
              foundKey = \case
                Zone.FromTopOfDeck _ -> Zone.FromDeck
                Zone.FromBottomOfDeck _ -> Zone.FromDeck
                other -> other
              shouldShuffle = case searchType of
                Looking -> False
                Revealing -> any (\(z, zrs) -> foundKey z == FromDeck && zrs == ShuffleBackIn) searchZones
                Searching -> any (\(z, zrs) -> foundKey z == FromDeck && zrs == ShuffleBackIn) searchZones
            pushWhen shouldShuffle $ ShuffleDeck (Deck.InvestigatorDeck a.id)
          DrawFoundUpTo who n -> do
            let
              choices =
                [ targetLabel (toCardId card) [DrawFocusedToHand iid (toTarget who) zone (toCardId card)]
                | (zone, cards) <- mapToList targetCards
                , card <- cards
                ]
            push
              $ if null choices
                then chooseOne player [Label "$label.noCardsFound" []]
                else chooseUpToN player n "Do not draw more cards" choices
          PlayFound who n -> do
            let windows' = [mkWhen Window.NonFast, mkWhen (Window.DuringTurn iid)]
            playableCards <- for (mapToList targetCards) $ \(zone, cards) -> do
              cards' <- filterM (getIsPlayable who source (UnpaidCost NoAction) windows') cards
              pure (zone, cards')
            let
              choices =
                [ targetLabel (toCardId card) [PayCardCost iid card windows']
                | (_, cards) <- playableCards
                , card <- cards
                ]
            push $ chooseN player n $ if null choices then [Label "$label.noCardsFound" []] else choices
          PlayFoundNoCost who n -> do
            let windows' = [mkWhen Window.NonFast, mkWhen (Window.DuringTurn iid)]
            playableCards <- for (mapToList targetCards) $ \(zone, cards) -> do
              cards' <- filterM (getIsPlayable who source Cost.PaidCost windows') cards
              pure (zone, cards')
            let
              choices =
                [ targetLabel (toCardId card) [PutCardIntoPlay iid card Nothing NoPayment windows']
                | (_, cards) <- playableCards
                , card <- cards
                ]
            push $ chooseN player n $ if null choices then [Label "$label.noCardsFound" []] else choices
          DeferSearchedToTarget searchTarget _ -> do
            -- N.B. You must handle target duplication (see Mandy Thompson) yourself
            if all null (toList targetCards)
              then Lifted.promptI iid "noCardsFound" $ push $ SearchNoneFound iid searchTarget
              else
                pushAll
                  [ PreSearchFound iid (Just searchTarget) (Deck.InvestigatorDeck iid') (concat $ toList targetCards)
                  , After (PreSearchFound iid Nothing (Deck.InvestigatorDeck a.id) (concat $ toList targetCards))
                  , SearchFound iid searchTarget (Deck.InvestigatorDeck iid') (concat $ toList targetCards)
                  ]
          DrawAllFound who -> do
            let
              choices =
                [ targetLabel (toCardId card) [DrawFocusedToHand iid (toTarget who) zone (toCardId card)]
                | (zone, cards) <- mapToList targetCards
                , card <- cards
                ]

            let
              shouldShuffle = case searchType of
                Looking -> False
                Revealing -> True
                Searching -> True

            pushAll
              $ if null choices
                then
                  [ chooseOne player [Label "$label.noCardsFound" [ShuffleDeck (Deck.InvestigatorDeck a.id) | shouldShuffle]]
                  ]
                else
                  let cards = concat $ toList targetCards
                      (before, _, after) = frame $ Window.DrawCards iid cards
                   in [before, chooseOneAtATime player choices]
                        <> [ ShuffleDeck (Deck.InvestigatorDeck a.id) | shouldShuffle && length targetCards == length foundCards
                           ]
                        <> [after]
          ReturnCards -> do
            unless (all null (toList targetCards)) do
              pushAll
                [ PreSearchFound iid Nothing (Deck.InvestigatorDeck a.id) (concat $ toList targetCards)
                , After (PreSearchFound iid Nothing (Deck.InvestigatorDeck a.id) (concat $ toList targetCards))
                ]
    _ -> pure ()
  pure a
