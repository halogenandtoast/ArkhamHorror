{-# LANGUAGE TypeAbstractions #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Investigator.Runner (
  module Arkham.Investigator.Runner,
  module X,
) where

import Arkham.Prelude

import Arkham.Ability as X hiding (PaidCost)
import Arkham.ChaosToken as X
import Arkham.ClassSymbol as X
import Arkham.Classes as X
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
import Arkham.Trait as X hiding (Cultist, ElderThing)
import Data.Aeson (Result (..))
import Data.Aeson.KeyMap qualified as KeyMap

import Arkham.Action (Action)
import Arkham.Action qualified as Action
import Arkham.Action.Additional
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.CampaignLog
import Arkham.Campaigns.EdgeOfTheEarth.Seal
import Arkham.Capability
import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Card.Settings
import Arkham.Classes.HasGame
import Arkham.CommitRestriction
import Arkham.Constants
import Arkham.Cost qualified as Cost
import Arkham.Customization
import Arkham.Damage
import Arkham.DamageEffect
import Arkham.Deck qualified as Deck
import Arkham.DefeatedBy
import Arkham.Discard
import Arkham.Discover
import Arkham.Draw.Types
import Arkham.Enemy.Types qualified as Field
import Arkham.Event.Types (Field (..))
import Arkham.Fight.Types
import {-# SOURCE #-} Arkham.Game (asIfTurn, withoutCanModifiers)
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers
import Arkham.Helpers.Ability (getAbilityLimit, getCanAffordUseWith, isForcedAbility)
import Arkham.Helpers.Action (
  additionalActionCovers,
  canDo,
  getActions,
  getAdditionalActions,
  getCanAfford,
 )
import Arkham.Helpers.Card (drawThisCardFrom, extendedCardMatch, getModifiedCardCost, passesLimits)
import Arkham.Helpers.Cost (getCanAffordCost, getSpendableResources)
import Arkham.Helpers.Criteria (passesCriteria)
import Arkham.Helpers.Deck qualified as Deck
import Arkham.Helpers.Discover
import Arkham.Helpers.Game (withAlteredGame)
import Arkham.Helpers.Location (getCanMoveTo, getCanMoveToMatchingLocations)
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
import Arkham.Investigate.Types
import {-# SOURCE #-} Arkham.Investigator
import Arkham.Investigator.Types qualified as Attrs
import Arkham.Key
import Arkham.Location.Types (Field (..))
import Arkham.Matcher (
  AssetMatcher (..),
  CardMatcher (..),
  EnemyMatcher (..),
  EventMatcher (..),
  ExtendedCardMatcher (..),
  InvestigatorMatcher (..),
  LocationMatcher (..),
  SourceMatcher (..),
  TreacheryMatcher (..),
  assetControlledBy,
  assetIs,
  cardIs,
  colocatedWith,
  enemyEngagedWith,
  locationWithInvestigator,
  mapOneOf,
  oneOf,
  orConnected,
  treacheryInThreatAreaOf,
  pattern AnyInPlayEnemy,
  pattern AssetWithAnyClues,
 )
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted (obtainCard)
import Arkham.Message.Lifted qualified as Lifted
import Arkham.Message.Lifted.Choose qualified as Choose
import Arkham.Modifier
import Arkham.Modifier qualified as Modifier
import Arkham.Movement
import Arkham.Phase
import Arkham.Placement
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
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Treachery.Types (Field (..))
import Arkham.Window (Window (..), mkAfter, mkWhen, mkWindow)
import Arkham.Window qualified as Window
import Arkham.Zone qualified as Zone
import Control.Lens (each, non, over, _Just)
import Control.Monad.State.Strict (evalStateT, get, modify)
import Data.Data.Lens (biplate)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Monoid
import Data.Set qualified as Set
import Data.UUID (nil)

instance RunMessage Investigator where
  runMessage msg i@(Investigator (a :: original)) = do
    modifiers' <- getModifiers (toTarget i)
    let msg' = if Blank `elem` modifiers' then Blanked msg else msg
    case investigatorForm (toAttrs a) of
      TransfiguredForm inner -> withInvestigatorCardCode inner \(SomeInvestigator @a) ->
        Investigator
          . investigatorFromAttrs @original
          . toAttrs
          <$> runMessage @a msg' (investigatorFromAttrs @a (toAttrs a))
      _ -> Investigator <$> runMessage msg' a

instance RunMessage InvestigatorAttrs where
  runMessage = runInvestigatorMessage

longestUniqueStreak :: Ord a => [[a]] -> [[a]]
longestUniqueStreak = go []
 where
  go uniq [] = uniq
  go uniq (xs : xss) =
    let seen = mconcat uniq
     in if any (`notElem` seen) xs
          then go (xs : uniq) xss
          else go [] xss

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

overMetaKey
  :: forall a
   . (HasCallStack, ToJSON a, FromJSON a)
  => Key
  -> (a -> a -> a)
  -> a
  -> InvestigatorAttrs
  -> InvestigatorAttrs
overMetaKey k f a attrs = case attrs.meta of
  Object o -> case KeyMap.lookup k o of
    Just v -> case fromJSON @a v of
      Success a' -> attrs {investigatorMeta = Object $ KeyMap.insert k (toJSON $ f a' a) o}
      _ -> error $ "Could not insert meta key, meta is not an a: " <> show v
    Nothing -> attrs {investigatorMeta = Object $ KeyMap.insert k (toJSON a) o}
  Null -> attrs {investigatorMeta = object [k .= a]}
  _ -> error $ "Could not insert meta key, meta is not Null or Object: " <> show attrs.meta

setMetaKey :: (ToJSON a, HasCallStack) => Key -> a -> InvestigatorAttrs -> InvestigatorAttrs
setMetaKey k v attrs = case attrs.meta of
  Object o -> attrs {investigatorMeta = Object $ KeyMap.insert k (toJSON v) o}
  Null -> attrs {investigatorMeta = object [k .= v]}
  _ -> error $ "Could not insert meta key, meta is not Null or Object: " <> show attrs.meta

insertMetaKey :: HasCallStack => Key -> InvestigatorAttrs -> InvestigatorAttrs
insertMetaKey k attrs = case attrs.meta of
  Object o -> attrs {investigatorMeta = Object $ KeyMap.insert k (toJSON True) o}
  Null -> attrs {investigatorMeta = object [k .= True]}
  _ -> error $ "Could not insert meta key, meta is not Null or Object: " <> show attrs.meta

deleteMetaKey :: HasCallStack => Key -> InvestigatorAttrs -> InvestigatorAttrs
deleteMetaKey k attrs = case attrs.meta of
  Object o -> attrs {investigatorMeta = Object $ KeyMap.delete k o}
  Null -> attrs {investigatorMeta = object []}
  _ -> error $ "Could not delete meta key, meta is not Null or Object: " <> show attrs.meta

lookupMetaKeyWithDefault :: FromJSON a => Key -> a -> InvestigatorAttrs -> a
lookupMetaKeyWithDefault k def attrs = case attrs.meta of
  Object o -> maybe def (toResultDefault def) $ KeyMap.lookup k o
  Null -> def
  _ -> def

onlyCampaignAbilities :: UsedAbility -> Bool
onlyCampaignAbilities UsedAbility {..} = case abilityLimitType (abilityLimit usedAbility) of
  Just PerCampaign -> True
  _ -> False

-- There are a few conditions that can occur that mean we must need to use an ability.
-- No valid targets. For example Marksmanship
-- Can't afford card. For example On Your Own
getAllAbilitiesSkippable :: HasGame m => InvestigatorAttrs -> [Window] -> m Bool
getAllAbilitiesSkippable attrs windows = allM (getWindowSkippable attrs windows) windows

getWindowSkippable :: HasGame m => InvestigatorAttrs -> [Window] -> Window -> m Bool
getWindowSkippable
  attrs
  ws
  ( windowTiming &&& windowType ->
      (Timing.When, Window.PlayCard iid (Window.CardPlay card@(PlayerCard pc) asAction))
    ) | iid == toId attrs = do
    allModifiers <- getModifiers card
    cost <- getModifiedCardCost iid card
    let isFast = isJust $ cdFastWindow (toCardDef card) <|> listToMaybe [w | BecomesFast w <- allModifiers]
    andM
      [ withAlteredGame withoutCanModifiers
          $ getCanAffordCost (toId attrs) pc [#play] ws (ResourceCost cost)
      , if isFast || not asAction
          then pure True
          else getCanAffordCost (toId attrs) pc [#play] ws (ActionCost 1)
      ]
getWindowSkippable _ _ w@(windowTiming &&& windowType -> (Timing.When, Window.ActivateAbility iid _ ab)) = do
  let
    excludeOne [] = []
    excludeOne (uab : xs) | ab == usedAbility uab = do
      if usedTimes uab <= 1
        then xs
        else uab {usedTimes = usedTimes uab - 1} : xs
    excludeOne (uab : xs) = uab : excludeOne xs
  andM
    [ getCanAffordUseWith excludeOne CanNotIgnoreAbilityLimit iid ab [w]
    , withAlteredGame withoutCanModifiers
        $ passesCriteria iid Nothing ab.source ab.requestor [w] (abilityCriteria ab)
    ]
getWindowSkippable attrs ws (windowType -> Window.WouldPayCardCost iid _ _ card@(PlayerCard pc)) | iid == toId attrs = do
  allModifiers <- getModifiers card
  cost <- getModifiedCardCost iid card
  let isFast = isJust $ cdFastWindow (toCardDef card) <|> listToMaybe [w | BecomesFast w <- allModifiers]
  andM
    [ withAlteredGame withoutCanModifiers
        $ getCanAffordCost (toId attrs) pc [#play] ws (ResourceCost cost)
    , if isFast
        then pure True
        else getCanAffordCost (toId attrs) pc [#play] ws (ActionCost 1)
    ]
getWindowSkippable _ _ _ = pure True

getHealthDamageableAssets
  :: HasGame m
  => InvestigatorId
  -> AssetMatcher
  -> Source
  -> Int
  -> [Target]
  -> [Target]
  -> m (Set AssetId)
getHealthDamageableAssets _ _ _ 0 _ _ = pure mempty
getHealthDamageableAssets iid matcher source _ damageTargets horrorTargets = do
  allAssets <- select $ matcher <> AssetCanBeAssignedDamageBy iid <> AssetCanBeDamagedBySource source
  excludes <- do
    modifiers <- getModifiers iid
    excludeMatchers <- forMaybeM modifiers $ \case
      NoMoreThanOneDamageOrHorrorAmongst excludeMatcher -> do
        excludes <- selectMap AssetTarget excludeMatcher
        pure $ do
          guard $ any (`elem` excludes) (damageTargets <> horrorTargets)
          pure excludeMatcher
      _ -> pure Nothing
    case excludeMatchers of
      [] -> pure mempty
      xs -> select (AssetOneOf xs)
  pure $ setFromList $ filter (`notElem` excludes) allAssets

getSanityDamageableAssets
  :: HasGame m
  => InvestigatorId
  -> AssetMatcher
  -> Source
  -> Int
  -> [Target]
  -> [Target]
  -> m (Set AssetId)
getSanityDamageableAssets _ _ _ 0 _ _ = pure mempty
getSanityDamageableAssets iid matcher source _ damageTargets horrorTargets = do
  allAssets <- select $ matcher <> AssetCanBeAssignedHorrorBy iid <> AssetCanBeDamagedBySource source
  excludes <- do
    modifiers <- getModifiers iid
    excludeMatchers <- forMaybeM modifiers $ \case
      NoMoreThanOneDamageOrHorrorAmongst excludeMatcher -> do
        excludes <- selectMap AssetTarget excludeMatcher
        pure $ do
          guard $ any (`elem` excludes) (damageTargets <> horrorTargets)
          pure excludeMatcher
      _ -> pure Nothing
    case excludeMatchers of
      [] -> pure mempty
      xs -> select (AssetOneOf xs)
  pure $ setFromList $ filter (`notElem` excludes) allAssets

runWindow
  :: (HasGame m, HasQueue Message m) => InvestigatorAttrs -> [Window] -> [Ability] -> [Card] -> m ()
runWindow attrs windows actions playableCards = do
  let iid = toId attrs
  unless (null playableCards && null actions) $ do
    anyForced <- anyM (isForcedAbility iid) actions
    player <- getPlayer iid
    if anyForced
      then do
        let
          (isSilent, normal) = partition isSilentForcedAbility actions
          toForcedAbilities = map (flip (UseAbility iid) windows)
          toUseAbilities = map ((\f -> f windows [] []) . AbilityLabel iid)
        -- Silent forced abilities should trigger automatically
        pushAll
          $ toForcedAbilities isSilent
          <> [asWindowChoose windows $ chooseOne player (toUseAbilities normal) | notNull normal]
          <> [Do (CheckWindows windows) | null normal] -- if we have no normal windows the forced silent will not retrigger
      else do
        let globalSkip = attrs.settings.globalSettings.ignoreUnrelatedSkillTestTriggers
        let
          applySettingsFilter ab =
            if not globalSkip
              then pure True
              else
                getSkillTest >>= \case
                  Nothing -> pure True
                  Just st -> case ab.wantsSkillTest of
                    Nothing -> pure $ not $ globalSkip && isFastAbility ab
                    Just matcher -> skillTestMatches iid GameSource st matcher
        actions' <- filterM applySettingsFilter actions
        actionsWithMatchingWindows <-
          for actions' $ \ability@Ability {..} ->
            (ability,) <$> filterM (\w -> windowMatches iid abilitySource w abilityWindow) windows
        skippable <- getAllAbilitiesSkippable attrs windows
        unless (null playableCards && null actionsWithMatchingWindows) do
          push
            $ asWindowChoose windows
            $ chooseOne player
            $ [ targetLabel c [InitiatePlayCard iid c Nothing NoPayment windows True]
              | c <- playableCards
              ]
            <> map (\(ability, windows') -> AbilityLabel iid ability windows' [] []) actionsWithMatchingWindows
            <> [SkipTriggersButton iid | skippable]

runInvestigatorMessage :: Runner InvestigatorAttrs
runInvestigatorMessage msg a@InvestigatorAttrs {..} = runQueueT $ case msg of
  SealedChaosToken token miid (isTarget a -> True) -> do
    when (a.id `elem` miid) do
      whenWindow <- checkWindows [mkWhen (Window.ChaosTokenSealed a.id token)]
      afterWindow <- checkWindows [mkAfter (Window.ChaosTokenSealed a.id token)]
      pushAll [whenWindow, afterWindow]
    pure $ a & sealedChaosTokensL %~ (token :)
  SealedChaosToken token miid _ -> do
    when (a.id `elem` miid) do
      whenWindow <- checkWindows [mkWhen (Window.ChaosTokenSealed a.id token)]
      afterWindow <- checkWindows [mkAfter (Window.ChaosTokenSealed a.id token)]
      pushAll [whenWindow, afterWindow]
    pure $ a & sealedChaosTokensL %~ filter (/= token)
  UnsealChaosToken token -> pure $ a & sealedChaosTokensL %~ filter (/= token)
  ReturnChaosTokensToPool tokens -> pure $ a & sealedChaosTokensL %~ filter (`notElem` tokens)
  RemoveAllChaosTokens face -> do
    pure $ a & sealedChaosTokensL %~ filter ((/= face) . chaosTokenFace)
  UpdateGlobalSetting iid s | iid == a.id -> do
    pure $ a & settingsL %~ updateGlobalSetting s
  UpdateCardSetting iid cCode s | iid == a.id -> do
    pure $ a & settingsL %~ updateCardSetting cCode s
  EndOfGame _ -> do
    pure $ a & placementL .~ Unplaced
  RecordForInvestigator iid key | iid == toId a -> do
    send $ "Record \"" <> format investigatorName <> " " <> format key <> "\""
    pure $ a & (logL . recordedL %~ insertSet key) . (logL . orderedKeysL %~ (<> [key]))
  EndCheckWindow -> do
    depth <- getWindowDepth
    let
      filterAbility UsedAbility {..} = do
        getAbilityLimit (toId a) usedAbility <&> \case
          NoLimit -> False
          PlayerLimit PerWindow _ -> depth >= usedDepth
          GroupLimit PerWindow _ -> depth >= usedDepth
          _ -> True

    usedAbilities <-
      map
        ( \u ->
            if usedDepth u > depth
              then u {usedThisWindow = False}
              else if usedDepth u == depth && depth > 0 then u {usedThisWindow = True} else u
        )
        <$> filterM filterAbility investigatorUsedAbilities
    pure $ a & usedAbilitiesL .~ usedAbilities
  EndOfScenario {} -> do
    pure $ a & handL .~ mempty & defeatedL .~ False & resignedL .~ False
  ResetGame ->
    pure
      $ (cbCardBuilder (investigator id (toCardDef a) (getAttrStats a)) nullCardId investigatorPlayerId)
        { Attrs.investigatorId = investigatorId
        , investigatorXp = investigatorXp
        , investigatorPhysicalTrauma = investigatorPhysicalTrauma
        , investigatorMentalTrauma = investigatorMentalTrauma
        , investigatorTokens =
            addTokens Horror investigatorMentalTrauma $ addTokens Token.Damage investigatorPhysicalTrauma mempty
        , investigatorStartsWith = investigatorStartsWith
        , investigatorStartsWithInHand = investigatorStartsWithInHand
        , investigatorSupplies = investigatorSupplies
        , investigatorUsedAbilities = filter onlyCampaignAbilities investigatorUsedAbilities
        , investigatorLog = investigatorLog
        , investigatorSideDeck = investigatorSideDeck
        , investigatorTaboo = investigatorTaboo
        , investigatorMutated = investigatorMutated
        , investigatorSlots = defaultSlots a.id
        , investigatorDeckUrl = investigatorDeckUrl
        , investigatorKilled = investigatorKilled
        , investigatorDrivenInsane = investigatorDrivenInsane
        , investigatorSettings = investigatorSettings
        }
  AddDeckBuildingAdjustment iid adjustment | iid == investigatorId -> do
    pure $ a & deckBuildingAdjustmentsL %~ (adjustment :)
  SetupInvestigator iid | iid == investigatorId -> do
    (startsWithMsgs, deck') <-
      foldM
        ( \(msgs, currentDeck) cardDef -> do
            let (before, after) = break ((== cardDef) . toCardDef) (unDeck currentDeck)
            case after of
              (card : rest) ->
                pure
                  ( PutCardIntoPlay
                      investigatorId
                      (toCard card)
                      Nothing
                      NoPayment
                      (Window.defaultWindows investigatorId)
                      : msgs
                  , Deck (before <> rest)
                  )
              _ | investigatorId `elem` ["05046", "05047", "05048", "05049"] -> do
                card <- setOwner investigatorId =<< genCard cardDef
                pure
                  ( PutCardIntoPlay
                      investigatorId
                      card
                      Nothing
                      NoPayment
                      (Window.defaultWindows investigatorId)
                      : msgs
                  , currentDeck
                  )
              _ -> pure (msgs, currentDeck)
        )
        ([], investigatorDeck)
        investigatorStartsWith
    let (permanentCards, deck'') = partition (cdPermanent . toCardDef) (unDeck deck')
    let deck''' =
          filter
            ( and
                . sequence
                  [ (`notElem` investigatorStartsWithInHand) . toCardDef
                  , not
                      . ( `cardMatch`
                            oneOf [cardIs Treacheries.falseAwakening, cardIs Treacheries.falseAwakeningPointOfNoReturn]
                        )
                  ]
            )
            deck''

    let bonded = nub $ concatMap (cdBondedWith . toCardDef) (unDeck investigatorDeck)

    bondedCards <- concatForM bonded $ \(n, cCode) -> do
      case lookupCardDef cCode of
        Nothing -> error "missing card"
        Just def -> do
          cs <- replicateM n (genCard def)
          traverse (Arkham.Card.setTaboo investigatorTaboo <=< setOwner iid) cs

    pushAll
      $ startsWithMsgs
      <> [ PutCardIntoPlay
             investigatorId
             (PlayerCard card)
             Nothing
             NoPayment
             (Window.defaultWindows investigatorId)
         | card <- permanentCards
         ]
      <> [TakeStartingResources investigatorId]
    pure $ a & (deckL .~ Deck deck''') & bondedCardsL .~ bondedCards
  ReturnToHand iid (AssetTarget aid) | iid == investigatorId -> do
    pushWhen (providedSlot a aid) $ RefillSlots a.id []
    pure $ a & (slotsL %~ removeFromSlots aid)
  ReturnToHand iid (EventTarget aid) | iid == investigatorId -> do
    pushWhen (providedSlot a aid) $ RefillSlots a.id []
    pure a
  ReturnToHand iid (CardIdTarget cardId) | iid == investigatorId -> do
    card <- getCard cardId
    pushAll [ObtainCard card.id, AddToHand iid [card]]
    pure a
  CheckAdditionalActionCosts iid target action msgs | iid == investigatorId -> do
    mods <- getModifiers a
    targetMods <- getModifiers target
    let
      additionalCosts =
        mapMaybe
          \case
            AdditionalActionCostOf (IsAction action') n | action == action' -> Just (ActionCost n)
            _ -> Nothing
          mods
          <> mapMaybe
            \case
              AdditionalCostToInvestigate c | action == #investigate -> Just c
              _ -> Nothing
            targetMods
    if null additionalCosts
      then pushAll msgs
      else do
        canPay <- getCanAffordCost iid a [] [mkWhen Window.NonFast] (mconcat additionalCosts)
        when canPay do
          -- we pass an empty list of actions here because we already have
          -- moved the costs to the pay for ability's additional costs
          pushAll $ [PayForAbility (abilityEffect a [] $ mconcat additionalCosts) []] <> msgs
    pure a
  TakeStartingResources iid | iid == investigatorId -> do
    mods <- getModifiers a
    let
      base = fromMaybe 5 $ listToMaybe [n | BaseStartingResources n <- mods]
      startingResources =
        if CannotGainResources `elem` mods
          then 0
          else max 0 $ getSum $ mconcat $ Sum base : [Sum n | StartingResources n <- mods]
      startingClues = getSum $ mconcat [Sum n | StartingClues n <- mods]
    pure $ a & tokensL %~ (setTokens Resource startingResources . setTokens Clue startingClues)
  InvestigatorMulligan iid | iid == investigatorId -> do
    unableToMulligan <- hasModifier a CannotMulligan
    hand <- field InvestigatorHand iid
    player <- getPlayer iid
    push
      $ if null hand || unableToMulligan
        then FinishedWithMulligan investigatorId
        else
          chooseOne player
            $ Label "Done With Mulligan" [FinishedWithMulligan investigatorId]
            : [ targetLabel
                  (toCardId card)
                  [DiscardCard iid GameSource (toCardId card), InvestigatorMulligan iid]
              | card <- hand
              , cdCanReplace (toCardDef card)
              ]
    pure a
  BeginTrade iid _source (AssetTarget aid) iids | iid == investigatorId -> do
    player <- getPlayer iid
    push $ chooseOne player [targetLabel iid' [TakeControlOfAsset iid' aid] | iid' <- iids]
    pure a
  BeginTrade iid source (ResourceTarget _) iids | iid == investigatorId -> do
    player <- getPlayer iid
    push
      $ chooseOne player
      $ [targetLabel iid' [TakeResources iid' 1 source False, SpendResources iid 1] | iid' <- iids]
    pure a
  PlaceSwarmCards iid eid n | iid == investigatorId && n > 0 -> do
    let cards = map toCard . take n $ unDeck investigatorDeck
    for_ cards $ push . PlacedSwarmCard eid
    pure $ a & (deckL %~ filter ((`notElem` cards) . PlayerCard))
  SetRole iid role | iid == investigatorId -> do
    pure $ a {investigatorClass = role}
  AllRandomDiscard source matcher | not (a ^. defeatedL || a ^. resignedL) -> do
    push $ toMessage $ randomDiscardMatching investigatorId source matcher
    pure a
  FinishedWithMulligan iid | iid == investigatorId -> do
    modifiers' <- getModifiers (toTarget a)
    let
      allowedMulligans =
        foldl'
          ( \total -> \case
              Mulligans n -> max 0 (total + n)
              _ -> total
          )
          1
          modifiers'
      startingHandAmount =
        foldl'
          ( \total -> \case
              StartingHand n -> max 0 (total + n)
              _ -> total
          )
          5
          modifiers'
      additionalStartingCards = concat $ mapMaybe (preview _AdditionalStartingCards) modifiers'
    -- investigatorHand is dangerous, but we want to use it here because we're
    -- only affecting cards actually in hand [I think]
    (discard, hand, deck) <-
      if any (`elem` modifiers') [CannotDrawCards, CannotManipulateDeck]
        then pure (investigatorDiscard, investigatorHand, unDeck investigatorDeck)
        else drawOpeningHand a (startingHandAmount - length investigatorHand)
    window <- checkWindows [mkAfter (Window.DrawingStartingHand iid)]
    additionalHandCards <-
      (additionalStartingCards <>) <$> traverse genCard investigatorStartsWithInHand

    -- need the virtual hand to get correct length
    hand' <- field InvestigatorHand iid

    pushAll
      $ [ShuffleDiscardBackIn iid, window]
      <> [ InvestigatorMulligan iid
         | (a ^. mulligansTakenL + 1) < allowedMulligans
         , startingHandAmount - length hand' > 0
         ]
    pure
      $ a
      & (discardL .~ discard)
      & (handL .~ hand <> additionalHandCards)
      & (deckL .~ Deck deck)
      & (mulligansTakenL +~ 1)
  ForInvestigator iid BeginGame | iid == investigatorId -> do
    -- if we have any cards with revelations on them, we need to trigger them
    let revelationCards = filter (hasRevelation . toCardDef) investigatorHand
    let
      choices = mapMaybe cardChoice revelationCards
      cardChoice = \case
        card@(PlayerCard card') -> do
          if hasRevelation card'
            then
              if toCardType card' == PlayerTreacheryType
                then Just (card, DrewTreachery iid Nothing card)
                else Just (card, Revelation iid $ CardIdSource card'.id)
            else
              if toCardType card' == PlayerEnemyType
                then Just (card, DrewPlayerEnemy iid card)
                else Nothing
        _ -> Nothing

    when (notNull choices) do
      player <- getPlayer iid
      push $ chooseOrRunOneAtATime player [targetLabel (toCardId card) [msg'] | (card, msg') <- choices]
    pure a
  ShuffleDeck (Deck.InvestigatorDeck iid) | iid == investigatorId -> do
    deck' <- shuffleM (unDeck investigatorDeck)
    pure $ a & deckL .~ Deck deck' & foundCardsL . at Zone.FromDeck .~ mempty
  ShuffleDiscardBackIn iid | iid == investigatorId -> do
    modifiers' <- getModifiers (toTarget a)
    if null investigatorDiscard || CardsCannotLeaveYourDiscardPile `elem` modifiers'
      then pure a
      else do
        deck <- shuffleM (investigatorDiscard <> coerce investigatorDeck)
        pure $ a & discardL .~ [] & deckL .~ Deck deck
  Resign iid | iid == investigatorId -> do
    pushAll $ resolve (Msg.InvestigatorResigned iid)
    pushM $ checkWhen $ Window.InvestigatorResigned iid
    pure $ a & endedTurnL .~ True
  Msg.InvestigatorDefeated source iid | iid == investigatorId -> do
    -- a card effect defeats an investigator directly
    windowMsg <-
      checkWindows [mkWhen $ Window.InvestigatorWouldBeDefeated (DefeatedByOther source) (toId a)]
    pushAll [windowMsg, InvestigatorIsDefeated source iid]
    pure a
  InvestigatorIsDefeated source iid | iid == investigatorId -> do
    isLead <- (== iid) <$> getLead
    modifiedHealth <- field InvestigatorHealth (toId a)
    modifiedSanity <- field InvestigatorSanity (toId a)
    let
      defeatedByHorror = investigatorSanityDamage a >= modifiedSanity
      defeatedByDamage = investigatorHealthDamage a >= modifiedHealth
      defeatedBy = case (defeatedByHorror, defeatedByDamage) of
        (True, True) -> DefeatedByDamageAndHorror source
        (True, False) -> DefeatedByHorror source
        (False, True) -> DefeatedByDamage source
        (False, False) -> DefeatedByOther source
      physicalTrauma = if investigatorHealthDamage a >= modifiedHealth then 1 else 0
      mentalTrauma = if investigatorSanityDamage a >= modifiedSanity then 1 else 0
    windowMsg <- checkWindows [mkAfter $ Window.InvestigatorDefeated defeatedBy iid]
    killed <- hasModifier a KilledIfDefeated
    becomeHomunculus <- hasModifier a BecomeHomunculusWhenDefeated

    pushAll
      $ windowMsg
      : CheckTrauma iid
      : [ChooseLeadInvestigator | isLead]
        <> [InvestigatorKilled (toSource a) iid | killed]
        <> [BecomeHomunculus iid | not killed && becomeHomunculus]
        <> [InvestigatorWhenEliminated (toSource a) iid Nothing]
    pure
      $ a
      & (defeatedL .~ True)
      & (endedTurnL .~ True)
      & (physicalTraumaL +~ physicalTrauma)
      & (mentalTraumaL +~ mentalTrauma)
  Msg.InvestigatorResigned iid | iid == investigatorId -> do
    pushAll [InvestigatorWhenEliminated (toSource a) iid (Just $ Do msg)]
    pure $ a & endedTurnL .~ True
  Do (Msg.InvestigatorResigned iid) | iid == investigatorId -> do
    isLead <- (== iid) <$> getLead
    pushWhen isLead ChooseLeadInvestigator
    pure $ a & resignedL .~ True
  -- InvestigatorWhenEliminated is handled by the scenario
  InvestigatorEliminated iid | iid == investigatorId -> do
    mlid <- field InvestigatorLocation iid
    for_ mlid $ \lid -> do
      assets <- select $ assetControlledBy iid <> AssetWithAnyClues
      for_ assets \asset -> do
        assetClues <- field AssetClues asset
        push $ MoveTokens GameSource (AssetSource asset) (toTarget lid) Clue assetClues
      pushAll
        $ PlaceTokens (toSource a) (toTarget lid) Clue (investigatorClues a)
        : [PlaceKey (toTarget lid) k | k <- toList investigatorKeys]
    -- if this investigator was the target of an enemy attack we need to remove them
    let
      isAttackingThisInvestigator = \case
        EnemyAttack details -> any (isTarget iid) details.targets
        _ -> False
    let
      isNotEliminatedChoice = \case
        TargetLabel _ msgs -> none isAttackingThisInvestigator msgs
        _ -> True
    let removeEliminatedChoices = filter isNotEliminatedChoice
    lift $ mapQueue \case
      Ask who (ChooseOneAtATime choices) -> Ask who (ChooseOneAtATime $ removeEliminatedChoices choices)
      other -> other
    pure
      $ a
      & (tokensL %~ (removeAllTokens Clue . removeAllTokens Resource))
      & (keysL .~ mempty)
      & (handL .~ mempty)
      & (discardL .~ mempty)
      & (deckL .~ mempty)
      & (eliminatedL .~ True)
  RemoveAllClues _ (InvestigatorTarget iid) | iid == investigatorId -> do
    pure $ a & tokensL %~ removeAllTokens Clue
  RemoveAllDoom _ (InvestigatorTarget iid) | iid == investigatorId -> do
    pure $ a & tokensL %~ removeAllTokens Doom
  RemovedFromPlay source@(AssetSource aid) -> do
    pushWhen (providedSlot a aid) $ RefillSlots a.id []
    pure
      $ a
      & (slotsL . each %~ filter (not . isSlotSource source))
      & (slotsL %~ removeFromSlots aid)
  SlotSourceRemovedFromPlay source@(AssetSource aid) -> do
    pushWhen (providedSlot a aid) $ RefillSlots a.id []
    pure
      $ a
      & (slotsL . each %~ filter (not . isSlotSource source))
      & (slotsL %~ removeFromSlots aid)
  RemovedFromPlay source@(EventSource aid) -> do
    pushWhen (providedSlot a aid) $ RefillSlots a.id []
    pure $ a & slotsL . each %~ filter (not . isSlotSource source)
  TakeControlOfAsset iid aid | iid == investigatorId -> do
    a <$ push (InvestigatorPlayAsset iid aid)
  TakeControlOfAsset iid aid | iid /= investigatorId -> do
    pure $ a & (slotsL %~ removeFromSlots aid)
  ChooseAndDiscardAsset iid source assetMatcher | iid == investigatorId -> do
    discardableAssetIds <- select $ assetControlledBy iid <> DiscardableAsset <> assetMatcher
    player <- getPlayer iid
    pushWhen (notNull discardableAssetIds)
      $ chooseOrRunOne player
      $ targetLabels discardableAssetIds (Only . toDiscardBy iid source)
    pure a
  AttachAsset aid _ -> pure $ a & (slotsL %~ removeFromSlots aid)
  PlaceAsset aid placement -> do
    case placement of
      InPlayArea iid | iid == investigatorId -> do
        push $ InvestigatorPlayAsset iid aid
        pure a
      InThreatArea iid | iid == investigatorId -> do
        push $ InvestigatorPlayAsset iid aid
        pure a
      AtLocation _ -> do
        isServitor <- aid <=~> assetIs Assets.summonedServitor
        if isServitor
          then pure a
          else pure $ a & (slotsL %~ removeFromSlots aid)
      _ -> pure $ a & (slotsL %~ removeFromSlots aid)
  PlaceKey (isTarget a -> True) (UnrevealedKey k) -> pure $ a & keysL %~ insertSet k
  PlaceKey (isTarget a -> True) k -> pure $ a & keysL %~ insertSet k
  PlaceKey (isTarget a -> False) k -> pure $ a & keysL %~ deleteSet k
  PlaceSeal (isTarget a -> True) k -> pure $ a & sealsL %~ insertSet k
  PlaceSeal (isTarget a -> False) k -> pure $ a & sealsL %~ Set.filter ((/= k.kind) . (.kind))
  ActivateSeal k -> pure $ a & sealsL %~ Set.map (\s -> if s.kind == k then s {sealActive = True} else s)
  AllCheckHandSize | not (a ^. defeatedL || a ^. resignedL) -> do
    handSize <- getHandSize a
    inHandCount <- getInHandCount a
    pushWhen (inHandCount > handSize) $ CheckHandSize investigatorId
    pure a
  CheckHandSize iid | iid == investigatorId -> do
    handSize <- getHandSize a
    inHandCount <- getInHandCount a
    when (inHandCount > handSize) $ do
      send $ format a <> " must discard down to " <> tshow handSize <> " cards"
      pushAll [SetActiveInvestigator iid, Do msg]
    pure a
  Do (CheckHandSize iid) | iid == investigatorId -> do
    handSize <- getHandSize a
    inHandCount <- getInHandCount a
    -- investigatorHand: can only discard cards actually in hand
    player <- getPlayer iid
    let viable = filter (isNothing . cdCardSubType . toCardDef) $ onlyPlayerCards investigatorHand

    pushWhen (inHandCount > handSize && notNull viable)
      $ chooseOne player
      $ [ targetLabel (toCardId card) [DiscardCard iid GameSource (toCardId card), Do (CheckHandSize iid)]
        | card <- viable
        ]
    pure a
  AddToDiscard iid pc | iid == investigatorId -> do
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
  DiscardFromHand handDiscard | handDiscard.investigator == investigatorId -> do
    when (handDiscard.amount > 0 || (handDiscard.strategy == DiscardAll && notNull investigatorHand)) do
      wouldDiscard <- checkWhen $ Window.WouldDiscardFromHand investigatorId handDiscard.source
      pushAll [wouldDiscard, Do msg]
    pure a
  Do (DiscardFromHand handDiscard) | handDiscard.investigator == investigatorId -> do
    player <- getPlayer investigatorId
    case discardableCards a of
      [] | handDiscard.strategy /= DiscardRandom -> pure ()
      cs -> case handDiscard.strategy of
        DiscardChoose -> do
          let n = min handDiscard.amount (length cs)
          case handDiscard.filter of
            CardWithId _ ->
              pushAll
                [DiscardCard investigatorId handDiscard.source c.id | c <- filterCards handDiscard.filter cs]
            _ ->
              pushWhen (n > 0)
                $ chooseN player n
                $ [ targetLabel c [DiscardCard investigatorId handDiscard.source c.id]
                  | c <- filterCards handDiscard.filter cs
                  ]
        DiscardAll -> do
          let cards = filterCards handDiscard.filter cs

          when (notNull cards) do
            push
              $ chooseOneAtATime player
              $ [ targetLabel c [DiscardCard investigatorId handDiscard.source c.id]
                | c <- cards
                ]
        DiscardRandom -> do
          -- only cards actually in hand
          let filtered = filterCards handDiscard.filter investigatorHand
          for_ (nonEmpty filtered) $ \targets -> do
            cards <- sampleN handDiscard.amount targets
            pushAll $ map (DiscardCard investigatorId handDiscard.source . toCardId) cards
    push $ DoneDiscarding investigatorId
    pure $ a & discardingL ?~ handDiscard
  Discard _ source (CardIdTarget cardId) | isJust (find ((== cardId) . toCardId) investigatorHand) -> do
    push (DiscardCard investigatorId source cardId)
    pure a
  Discard _ _ (SearchedCardTarget cardId) -> do
    pure $ a & foundCardsL . each %~ filter ((/= cardId) . toCardId)
  DiscardHand iid source | iid == investigatorId -> do
    liftRunMessage (DiscardFromHand $ discardAll iid source AnyCard) a
  DiscardCard iid source cardId | iid == investigatorId -> do
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
  Do (DiscardCard iid _source cardId) | iid == investigatorId -> do
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
  DoneDiscarding iid | iid == investigatorId -> case investigatorDiscarding of
    Nothing -> pure a
    Just handDiscard -> do
      when (discardAmount handDiscard == 0)
        $ for_ (discardThen handDiscard) push
      pure $ a & discardingL .~ Nothing
  RemoveCardFromHand iid cardId | iid == investigatorId -> do
    pure $ a & handL %~ filter ((/= cardId) . toCardId)
  RemoveCardFromSearch iid cardId | iid == investigatorId -> do
    pure $ a & foundCardsL %~ Map.map (filter ((/= cardId) . toCardId))
  ShuffleIntoDeck (Deck.InvestigatorDeck iid) (AssetTarget aid) | iid == investigatorId -> do
    if null investigatorDeck
      then do
        isDefeated <- field AssetIsDefeated aid
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
  ShuffleIntoDeck (Deck.InvestigatorDeck iid) (EventTarget eid) | iid == investigatorId -> do
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
  ShuffleIntoDeck (Deck.InvestigatorDeck iid) (SkillTarget aid) | iid == investigatorId -> do
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
  Discarded (AssetTarget aid) _ (PlayerCard card) -> do
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
          pcOwner card
            == Just investigatorId
            && card
            `notElem` investigatorDiscard
            && card
            `notElem` fromMaybe [] investigatorSideDeck

    pure $ a & (if shouldDiscard then discardL %~ (card :) else id) & (slotsL %~ removeFromSlots aid)
  -- Discarded _ _ (PlayerCard card) -> do
  --   let shouldDiscard = pcOwner card == Just investigatorId && card `notElem` investigatorDiscard
  --   if shouldDiscard
  --     then pure $ a & discardL %~ (card :) & handL %~ filter (/= PlayerCard card)
  --     else pure a
  Discarded (AssetTarget aid) _ (EncounterCard _) -> do
    pushWhen (providedSlot a aid) $ RefillSlots a.id []
    pure $ a & (slotsL %~ removeFromSlots aid)
  Discarded (EventTarget aid) _ _ -> do
    pushWhen (providedSlot a aid) $ RefillSlots a.id []
    pure a
  Exile (CardIdTarget cid) -> do
    for_ (find ((== cid) . toCardId) investigatorHand) \card -> do
      push $ Exiled (CardIdTarget cid) card
    pure a
  Exiled (AssetTarget aid) _ -> do
    pushWhen (providedSlot a aid) $ RefillSlots a.id []
    pure $ a & (slotsL %~ removeFromSlots aid)
  Exiled (EventTarget aid) _ -> do
    pushWhen (providedSlot a aid) $ RefillSlots a.id []
    pure a
  Exiled (CardIdTarget cid) _ -> pure $ a & handL %~ filter ((/= cid) . toCardId)
  RemoveFromGame (AssetTarget aid) -> do
    pushWhen (providedSlot a aid) $ RefillSlots a.id []
    pure $ a & (slotsL %~ removeFromSlots aid)
  RemoveFromGame (EventTarget aid) -> do
    pushWhen (providedSlot a aid) $ RefillSlots a.id []
    pure a
  RemoveFromGame (CardIdTarget cid) -> pure $ a & cardsUnderneathL %~ filter ((/= cid) . toCardId)
  -- Ch1ooseFightEnemy iid source mTarget skillType enemyMatcher isAction | iid == investigatorId -> do
  ChooseFightEnemy choose | choose.investigator == investigatorId -> do
    modifiers <- getModifiers a
    let source = choose.source
    let enemyMatcher = choose.matcher
    let
      isOverride = \case
        EnemyFightActionCriteria override -> Just override
        CanModify (EnemyFightActionCriteria override) -> Just override
        _ -> Nothing
      overrides = mapMaybe isOverride modifiers
      mustChooseMatchers = fold [mx | MustChooseEnemy mx <- modifiers]
      applyMatcherModifiers :: ModifierType -> EnemyMatcher -> EnemyMatcher
      applyMatcherModifiers (Modifier.AlternateFightField someField) original = case someField of
        SomeField Field.EnemyEvade -> original <> EnemyWithEvade
        SomeField Field.EnemyFight -> original <> EnemyWithFight
        _ -> original
      applyMatcherModifiers _ n = n
      canFightMatcher = case overrides of
        [] -> if choose.overriden then AnyInPlayEnemy else CanFightEnemy source
        [o] -> CanFightEnemyWithOverride o
        _ -> error "multiple overrides found"
    enemyIds <-
      withAlteredGame withoutCanModifiers
        $ asIfTurn investigatorId
        $ select
        $ foldr applyMatcherModifiers (canFightMatcher <> enemyMatcher <> mustChooseMatchers) modifiers

    canMoveToConnected <- case source.asset of
      Just aid -> aid <=~> AssetWithCustomization InscriptionOfTheHunt
      _ -> pure False
    locationIds <-
      withAlteredGame withoutCanModifiers
        $ asIfTurn investigatorId
        $ select
        $ LocationWithModifier CanBeAttackedAsIfEnemy
        <> if canMoveToConnected
          then orConnected (locationWithInvestigator investigatorId)
          else locationWithInvestigator investigatorId
    player <- getPlayer investigatorId
    let choices = enemyIds <> map coerce locationIds
    -- we might have killed the enemy via a reaction before getting here
    unless (null choices) do
      push
        $ chooseOne
          player
          [ FightLabel
              eid
              $ ChoseEnemy choose.skillTest investigatorId source eid
              : [ FightEnemy eid choose
                | not choose.onlyChoose
                ]
          | eid <- enemyIds <> map coerce locationIds
          ]
    pure a
  EngageEnemy iid eid _ True | iid == investigatorId -> do
    modifiers' <- getModifiers (toTarget a)
    beforeWindowMsg <- checkWindows [mkWhen (Window.PerformAction iid #engage)]
    afterWindowMsg <- checkWindows [mkAfter (Window.PerformAction iid #engage)]

    pushAll
      $ [ BeginAction
        , beforeWindowMsg
        , TakeActions iid [#engage] (ActionCost 1)
        ]
      <> [ CheckAttackOfOpportunity iid False
         | ActionDoesNotCauseAttacksOfOpportunity #engage `notElem` modifiers'
         ]
      <> [EngageEnemy iid eid Nothing False, afterWindowMsg, FinishAction, TakenActions iid [#engage]]

    pure a
  FightEnemy eid choose | choose.investigator == investigatorId && choose.isAction -> do
    let iid = investigatorId
    handleSkillTestNesting_ choose.skillTest msg do
      modifiers' <- getModifiers (toTarget a)
      beforeWindowMsg <- checkWindows [mkWhen $ Window.PerformAction iid #fight]
      afterWindowMsg <- checkWindows [mkAfter $ Window.PerformAction iid #fight]
      let
        performedActions = setFromList @(Set Action) $ concat investigatorActionsPerformed
        -- takenActions = setFromList @(Set Action) investigatorActionsTaken
        applyFightCostModifiers :: Cost -> ModifierType -> Cost
        applyFightCostModifiers costToEnter (AdditionalActionCostOf actionTarget n) =
          case actionTarget of
            FirstOneOfPerformed as
              | #fight `elem` as
              , null (performedActions `intersect` setFromList as) -> do
                  increaseActionCost costToEnter n
            IsAction Action.Fight -> increaseActionCost costToEnter n
            _ -> costToEnter
        applyFightCostModifiers costToEnter _ = costToEnter
      pushAll
        [ BeginAction
        , beforeWindowMsg
        , TakeActions iid [#fight] (foldl' applyFightCostModifiers (ActionCost 1) modifiers')
        , FightEnemy eid choose {chooseFightIsAction = False}
        , afterWindowMsg
        , FinishAction
        , TakenActions iid [#fight]
        ]
    pure a
  FightEnemy eid choose | choose.investigator == investigatorId && not choose.isAction -> do
    handleSkillTestNesting_ choose.skillTest msg do
      push (AttackEnemy eid choose)
    pure a
  FailedAttackEnemy iid eid | iid == investigatorId -> do
    doesNotDamageOtherInvestigators <- hasModifier a DoesNotDamageOtherInvestigator
    unless doesNotDamageOtherInvestigators $ do
      investigatorIds <- select $ InvestigatorEngagedWith $ EnemyWithId eid
      case investigatorIds of
        [x] | x /= iid -> push (InvestigatorDamageInvestigator iid x)
        _ -> pure ()
    pure a
  PlaceAdditionalDamage target source damage horror | isTarget a target -> do
    push $ Msg.InvestigatorDamage (toId a) source damage horror
    pure a
  InvestigatorDamageInvestigator iid xid | iid == investigatorId -> do
    damage <- damageValueFor 1 iid DamageForInvestigator
    push $ InvestigatorAssignDamage xid (InvestigatorSource iid) DamageAny damage 0
    pure a
  InvestigatorDamageEnemy iid eid source | iid == investigatorId -> do
    cannotDamage <- hasModifier iid CannotDealDamage
    unless cannotDamage $ do
      damage <- damageValueFor 1 iid DamageForEnemy
      -- if the source was a basic attack, we need to say the investigator did
      -- the damage to trigger the correct windows
      let
        source' =
          case source of
            AbilitySource _ 100 -> toSource iid
            UseAbilitySource _ _ 100 -> toSource iid
            _ -> source
      push $ EnemyDamage eid $ attack source' damage
    pure a
  ChooseEvadeEnemy choose | choose.investigator == investigatorId -> do
    modifiers <- getModifiers a
    let source = choose.source
    let mTarget = choose.target
    let skillType = choose.skillType
    let enemyMatcher = choose.matcher
    let isAction = choose.isAction
    let
      isOverride = \case
        EnemyEvadeActionCriteria override -> Just override
        CanModify (EnemyEvadeActionCriteria override) -> Just override
        _ -> Nothing
      overrides = mapMaybe isOverride modifiers
      mustChooseMatchers = fold [mx | MustChooseEnemy mx <- modifiers]
      applyMatcherModifiers :: ModifierType -> EnemyMatcher -> EnemyMatcher
      applyMatcherModifiers (Modifier.AlternateEvadeField someField) original = case someField of
        SomeField Field.EnemyEvade -> original <> EnemyWithEvade
        _ -> original
      applyMatcherModifiers _ n = n
      canEvadeMatcher = case overrides of
        [] -> if choose.overriden then AnyInPlayEnemy else CanEvadeEnemy source
        [o] -> CanEvadeEnemyWithOverride o
        _ -> error "multiple overrides found"
    enemyIds <-
      select
        $ foldr
          applyMatcherModifiers
          (canEvadeMatcher <> enemyMatcher <> mustChooseMatchers)
          modifiers
    player <- getPlayer a.id
    push
      $ chooseOne
        player
        [ EvadeLabel
            eid
            [ ChosenEvadeEnemy choose.skillTest source eid
            , EvadeEnemy choose.skillTest a.id eid source mTarget skillType isAction
            ]
        | eid <- enemyIds
        ]
    pure a
  ChooseEngageEnemy iid source mTarget enemyMatcher isAction | iid == investigatorId -> do
    modifiers <- getModifiers (InvestigatorTarget iid)
    let
      isOverride = \case
        EnemyEngageActionCriteria override -> Just override
        CanModify (EnemyEngageActionCriteria override) -> Just override
        _ -> Nothing
      overrides = mapMaybe isOverride modifiers
      canEngageMatcher = case overrides of
        [] -> CanEngageEnemy source
        [o] -> CanEngageEnemyWithOverride o
        _ -> error "multiple overrides found"
    enemyIds <- select $ canEngageMatcher <> enemyMatcher
    player <- getPlayer iid
    push
      $ chooseOne
        player
        [ EngageLabel eid [EngageEnemy iid eid mTarget isAction]
        | eid <- enemyIds
        ]
    pure a
  EvadeEnemy sid iid eid source mTarget skillType True | iid == investigatorId -> do
    handleSkillTestNesting_ sid msg do
      modifiers' <- getModifiers (toTarget a)
      beforeWindowMsg <- checkWindows [mkWhen $ Window.PerformAction iid #evade]
      afterWindowMsg <- checkWindows [mkAfter $ Window.PerformAction iid #evade]
      let
        performedActions = setFromList @(Set Action) $ concat investigatorActionsPerformed
        -- takenActions = setFromList @(Set Action) investigatorActionsTaken
        applyEvadeCostModifiers :: Cost -> ModifierType -> Cost
        applyEvadeCostModifiers costToEnter (AdditionalActionCostOf actionTarget n) =
          case actionTarget of
            FirstOneOfPerformed as
              | #evade `elem` as
              , null (performedActions `intersect` setFromList as) ->
                  increaseActionCost costToEnter n
            IsAction Action.Evade -> increaseActionCost costToEnter n
            _ -> costToEnter
        applyEvadeCostModifiers costToEnter _ = costToEnter
      pushAll
        [ BeginAction
        , beforeWindowMsg
        , TakeActions iid [#evade] (foldl' applyEvadeCostModifiers (ActionCost 1) modifiers')
        , EvadeEnemy sid iid eid source mTarget skillType False
        , afterWindowMsg
        , FinishAction
        , TakenActions iid [#evade]
        ]
    pure a
  EvadeEnemy sid iid eid source mTarget skillType False | iid == investigatorId -> do
    handleSkillTestNesting_ sid msg do
      attemptWindow <- checkWindows [mkWhen $ Window.AttemptToEvadeEnemy sid iid eid]
      pushAll [attemptWindow, TryEvadeEnemy sid iid eid source mTarget skillType, AfterEvadeEnemy iid eid]
    pure a
  MoveAction iid lid cost True | iid == investigatorId -> do
    beforeWindowMsg <- checkWindows [mkWhen (Window.PerformAction iid #move)]
    afterWindowMsg <- checkWindows [mkAfter (Window.PerformAction iid #move)]
    pushAll
      [ BeginAction
      , beforeWindowMsg
      , TakeActions iid [#move] cost
      , MoveAction iid lid cost False
      , afterWindowMsg
      , FinishAction
      , TakenActions iid [#move]
      ]
    pure a
  MoveAction iid lid _cost False | iid == investigatorId -> do
    from <- fromMaybe (LocationId nil) <$> field InvestigatorLocation iid
    afterWindowMsg <- Helpers.checkWindows [mkAfter $ Window.MoveAction iid from lid]
    canMove <- withoutModifier a CannotMove
    pushAll $ (guard canMove *> resolve (Move $ move a iid lid)) <> [afterWindowMsg]
    pure a
  Move movement | isTarget a (moveTarget movement) -> do
    scenarioEffect <- sourceMatches movement.source SourceIsScenarioCardEffect
    canMove <-
      if scenarioEffect
        then withoutModifier a CannotMove
        else withoutModifiers a [CannotMove, CannotMoveExceptByScenarioCardEffects]
    when canMove do
      case moveDestination movement of
        ToLocationMatching matcher -> do
          lids <- getCanMoveToMatchingLocations investigatorId (moveSource movement) matcher
          player <- getPlayer investigatorId
          push
            $ chooseOrRunOne player
            $ [targetLabel lid [Move $ movement {moveDestination = ToLocation lid}] | lid <- lids]
        ToLocation destinationLocationId -> do
          batchId <- getRandom

          let
            source = moveSource movement
            iid = investigatorId
          mFromLocation <- field InvestigatorLocation iid

          case moveMeans movement of
            Towards -> do
              player <- getPlayer iid
              let loc = fromJustNote "must have a starting location for OneAtATime" mFromLocation
              matchingClosestLocationIds <- select $ ClosestPathLocation loc destinationLocationId
              if destinationLocationId `elem` matchingClosestLocationIds
                then
                  push $ chooseOne player [targetLabel destinationLocationId [Move $ movement {moveMeans = Direct}]]
                else do
                  push
                    $ chooseOne
                      player
                      [ targetLabel
                          lid
                          [Move $ movement {moveDestination = ToLocation lid, moveMeans = Direct}]
                      | lid <- matchingClosestLocationIds
                      ]
            OneAtATime -> do
              player <- getPlayer iid
              let loc = fromJustNote "must have a starting location for OneAtATime" mFromLocation
              matchingClosestLocationIds <- select $ ClosestPathLocation loc destinationLocationId
              if destinationLocationId `elem` matchingClosestLocationIds
                then
                  push $ chooseOne player [targetLabel destinationLocationId [Move $ movement {moveMeans = Direct}]]
                else do
                  push
                    $ chooseOne
                      player
                      [ targetLabel
                          lid
                          [Move $ movement {moveDestination = ToLocation lid, moveMeans = Direct}, Move movement]
                      | lid <- matchingClosestLocationIds
                      ]
            Direct -> do
              imods <- getModifiers investigatorId
              leaveCosts <- case mFromLocation of
                Nothing -> pure mempty
                Just lid ->
                  if movePayAdditionalCosts movement
                    then do
                      mods' <- getModifiers lid
                      pure $ mconcat [c | AdditionalCostToLeave c <- mods']
                    else pure mempty

              enterCosts <- do
                if movePayAdditionalCosts movement
                  then do
                    revealed' <- field LocationRevealed destinationLocationId
                    baseEnter <- mwhen (not revealed') <$> field LocationCostToEnterUnrevealed destinationLocationId

                    mods' <- getModifiers destinationLocationId
                    pcosts <-
                      filterM ((destinationLocationId <=~>) . fst) [(ma, c) | AdditionalCostToEnterMatching ma c <- imods]
                    pure $ concatMap snd pcosts
                      <> mconcat [c | AdditionalCostToEnter c <- mods']
                      <> moveAdditionalEnterCosts movement
                      <> baseEnter
                  else pure mempty

              let
                (whenMoves, atIfMoves, afterMoves) = timings (Window.Moves iid source mFromLocation destinationLocationId)
                (mWhenLeaving, mAtIfLeaving, mAfterLeaving) = case mFromLocation of
                  Just from ->
                    batchedTimings batchId (Window.Leaving iid from) & \case
                      (whens, atIfs, afters) -> (Just whens, Just atIfs, Just afters)
                  Nothing -> (Nothing, Nothing, Nothing)
                mWouldMove = case mFromLocation of
                  Just from ->
                    batchedTimings batchId (Window.WouldMove iid source from destinationLocationId) & \case
                      (whens, _, _) -> Just whens
                  Nothing -> Nothing
                (whenEntering, atIfEntering, _) = batchedTimings batchId (Window.Entering iid destinationLocationId)

              -- Windows we need to check as understood:
              -- according to Empirical Hypothesis ruling the order should be like:
              -- when {leaving} -> atIf {leaving} -> after {leaving} -> before {entering} -> atIf {entering} / when {move} -> atIf {move} -> Reveal Location -> after but before enemy engagement {entering} -> Check Enemy Engagement -> after {entering, move}
              -- move but before enemy engagement is handled in MoveTo

              mRunWouldMove <- case mWouldMove of
                Just wouldMove -> Just <$> checkWindows [wouldMove]
                Nothing -> pure Nothing
              mRunWhenLeaving <- case mWhenLeaving of
                Just whenLeaving -> Just <$> checkWindows [whenLeaving]
                Nothing -> pure Nothing
              mRunAtIfLeaving <- case mAtIfLeaving of
                Just atIfLeaving -> Just <$> checkWindows [atIfLeaving]
                Nothing -> pure Nothing
              mRunAfterLeaving <- case mAfterLeaving of
                Just afterLeaving -> Just <$> checkWindows [afterLeaving]
                Nothing -> pure Nothing
              runWhenMoves <- checkWindows [whenMoves]
              runAtIfMoves <- checkWindows [atIfMoves]
              runWhenEntering <- checkWindows [whenEntering]
              runAtIfEntering <- checkWindows [atIfEntering]
              runAfterMoves <- checkWindows [afterMoves]

              pushBatched batchId
                $ maybeToList mRunWouldMove
                <> maybeToList mRunWhenLeaving
                <> maybeToList mRunAtIfLeaving
                <> [ PayAdditionalCost iid batchId leaveCosts
                   , WhenCanMove
                       iid
                       $ [MoveFrom source iid fromLocationId | fromLocationId <- maybeToList mFromLocation]
                       <> [ runWhenEntering
                          , runAtIfEntering
                          , PayAdditionalCost iid batchId enterCosts
                          , runWhenMoves
                          , runAtIfMoves
                          , MoveTo movement
                          , runAfterMoves
                          ]
                       <> maybeToList mRunAfterLeaving
                   ]
            Place -> do
              -- like Direct, but no moves windows and no costs

              let
                (whenEntering, atIfEntering, afterEntering) = batchedTimings batchId (Window.Entering iid destinationLocationId)
                (mWhenLeaving, mAtIfLeaving, mAfterLeaving) = case mFromLocation of
                  Just from ->
                    batchedTimings batchId (Window.Leaving iid from) & \case
                      (whens, atIfs, afters) -> (Just whens, Just atIfs, Just afters)
                  Nothing -> (Nothing, Nothing, Nothing)
              mRunWhenLeaving <- for mWhenLeaving \whenLeaving -> checkWindows [whenLeaving]
              mRunAtIfLeaving <- for mAtIfLeaving \atIfLeaving -> checkWindows [atIfLeaving]
              mRunAfterLeaving <- for mAfterLeaving \afterLeaving -> checkWindows [afterLeaving]
              runWhenEntering <- checkWindows [whenEntering]
              runAtIfEntering <- checkWindows [atIfEntering]
              runAfterEntering <- checkWindows [afterEntering]

              pushBatched batchId
                $ maybeToList mRunWhenLeaving
                <> maybeToList mRunAtIfLeaving
                <> [ runWhenEntering
                   , runAtIfEntering
                   , MoveTo movement
                   , runAfterEntering
                   ]
                <> maybeToList mRunAfterLeaving
    pure a
  WhenCanMove iid msgs | iid == investigatorId -> do
    canMove <- withoutModifier a CannotMove
    when canMove $ pushAll msgs
    pure a
  Will (PassedSkillTest iid _ _ (InvestigatorTarget iid') _ n) | iid == iid' && iid == investigatorId -> do
    pushM $ checkWindows [mkWhen (Window.WouldPassSkillTest iid n)]
    pure a
  Will (FailedSkillTest iid _ _ (InvestigatorTarget iid') _ n) | iid == iid' && iid == toId a -> do
    pushM $ checkWindows [mkWhen (Window.WouldFailSkillTest iid n)]
    pure a
  CancelDamage iid n | iid == investigatorId -> lift do
    withQueue_ \queue -> flip map queue $ \case
      Msg.InvestigatorDamage iid' s damage' horror' ->
        Msg.InvestigatorDamage iid' s (max 0 (damage' - n)) horror'
      InvestigatorDoAssignDamage iid' s t matcher' damage' horror' aa b ->
        InvestigatorDoAssignDamage iid' s t matcher' (max 0 (damage' - n)) horror' aa b
      other -> other
    pure a
  CancelHorror iid n | iid == investigatorId -> lift do
    withQueue_ \queue -> flip map queue $ \case
      Msg.InvestigatorDamage iid' s damage' horror' ->
        Msg.InvestigatorDamage iid' s damage' (max 0 (horror' - n))
      InvestigatorDoAssignDamage iid' s t matcher' damage' horror' aa b ->
        InvestigatorDoAssignDamage iid' s t matcher' damage' (max 0 (horror' - n)) aa b
      other -> other
    pure a
  InvestigatorDirectDamage iid source damage horror | iid == toId a -> do
    unless (investigatorDefeated || investigatorResigned) do
      mods <- getModifiers a
      let horrorToCancel =
            if any (`elem` mods) [CannotCancelHorror, CannotCancelHorrorFrom source]
              then 0
              else sum [n | WillCancelHorror n <- mods]
      let horror' = max 0 (horror - horrorToCancel)
      pushAll
        $ [ CheckWindows
              $ mkWhen (Window.WouldTakeDamageOrHorror source (toTarget a) damage horror')
              : [mkWhen (Window.WouldTakeDamage source (toTarget a) damage DamageDirect) | damage > 0]
                <> [mkWhen (Window.WouldTakeHorror source (toTarget a) horror') | horror' > 0]
          | damage > 0 || horror' > 0
          ]
        <> [ InvestigatorDoAssignDamage
               iid
               source
               DamageAny
               (AssetWithModifier CanBeAssignedDirectDamage)
               damage
               horror'
               []
               []
           ]
    pure a
  InvestigatorAssignDamage iid source strategy damage horror | iid == toId a -> do
    unless (investigatorDefeated || investigatorResigned) do
      mods <- getModifiers a
      if TreatAllDamageAsDirect `elem` mods
        then push $ InvestigatorDirectDamage iid source damage horror
        else do
          let horrorToCancel =
                if any (`elem` mods) [CannotCancelHorror, CannotCancelHorrorFrom source]
                  then 0
                  else sum [n | WillCancelHorror n <- mods]
          let horror' = max 0 (horror - horrorToCancel)
          pushAll
            $ [ CheckWindows
                  $ mkWhen (Window.WouldTakeDamageOrHorror source (toTarget a) damage horror')
                  : [mkWhen (Window.WouldTakeDamage source (toTarget a) damage strategy) | damage > 0]
                    <> [mkWhen (Window.WouldTakeHorror source (toTarget a) horror') | horror' > 0]
              | damage > 0 || horror' > 0
              ]
            <> [InvestigatorDoAssignDamage iid source strategy AnyAsset damage horror' [] []]
    pure a
  InvestigatorDoAssignDamage iid source damageStrategy _ 0 0 damageTargets horrorTargets | iid == toId a -> do
    let
      damageEffect = case source of
        EnemyAttackSource _ -> AttackDamageEffect
        _ -> NonAttackDamageEffect
      damageMap = frequencies damageTargets
      horrorMap = frequencies horrorTargets
      placedWindows =
        [ Window.PlacedDamage source target damage
        | (target, damage) <- mapToList damageMap
        ]
          <> [ Window.PlacedHorror source target horror
             | (target, horror) <- mapToList horrorMap
             ]
      checkAssets = nub $ keys horrorMap <> keys damageMap
    whenPlacedWindowMsg <- checkWindows $ map mkWhen placedWindows
    whenAssignedWindowMsg <- checkWhen $ Window.AssignedHorror source iid horrorTargets
    when
      ( damageStrategy
          == DamageFromHastur
          && toTarget a
          `elem` horrorTargets
          && investigatorSanityDamage a
          > investigatorSanity
      )
      do
        push $ InvestigatorDirectDamage iid source 1 0
    pushAll
      $ whenPlacedWindowMsg
      : [ CheckWindows
            $ [ mkWhen (Window.DealtDamage source damageEffect target damage)
              | target <- nub damageTargets
              , let damage = count (== target) damageTargets
              ]
            <> [ mkWhen (Window.DealtHorror source target horror)
               | target <- nub horrorTargets
               , let horror = count (== target) horrorTargets
               ]
        ]
        <> [whenAssignedWindowMsg | notNull horrorTargets]
        <> [CheckDefeated source (toTarget aid) | aid <- checkAssets]
        <> [ CheckWindows
               $ [ mkAfter (Window.DealtDamage source damageEffect target damage)
                 | target <- nub damageTargets
                 , let damage = count (== target) damageTargets
                 ]
               <> [ mkAfter (Window.DealtHorror source target horror)
                  | target <- nub horrorTargets
                  , let horror = count (== target) horrorTargets
                  ]
               <> [mkAfter (Window.AssignedHorror source iid horrorTargets) | notNull horrorTargets]
           ]
    pure a
  InvestigatorDoAssignDamage iid source DamageEvenly matcher health 0 damageTargets horrorTargets | iid == toId a -> do
    healthDamageableAssets <-
      toList <$> getHealthDamageableAssets iid matcher source health damageTargets horrorTargets
    healthDamageableInvestigators <- select $ InvestigatorCanBeAssignedDamageBy iid

    mustBeDamagedFirstBeforeInvestigator <- forMaybeM healthDamageableAssets $ \aid -> do
      mods <- getModifiers aid
      let n = sum [x | NonDirectDamageMustBeAssignToThisN x <- mods]
      let mustAssignRemaining = n > 0 && health <= n && count (== toTarget aid) damageTargets < n
      pure $ guard (NonDirectDamageMustBeAssignToThisFirst `elem` mods || mustAssignRemaining) $> aid

    let
      onlyAssets = any (`elem` mustBeDamagedFirstBeforeInvestigator) healthDamageableAssets
      allowedDamage =
        findFewestOccurrences
          damageTargets
          ( map toTarget healthDamageableAssets
              <> [InvestigatorTarget iid | not onlyAssets]
              <> (if not onlyAssets then map toTarget healthDamageableInvestigators else [])
          )
      assignRestOfHealthDamage =
        InvestigatorDoAssignDamage
          investigatorId
          source
          DamageEvenly
          matcher
          (health - 1)
          0
      -- N.B. we have to add to the end of targets to handle the drop logic
      damageAsset aid =
        ComponentLabel
          (AssetComponent aid DamageToken)
          [ Msg.AssignAssetDamageWithCheck aid source 1 0 False
          , assignRestOfHealthDamage (damageTargets <> [AssetTarget aid]) mempty
          ]
      damageInvestigator iid' =
        ComponentLabel
          (InvestigatorComponent iid' DamageToken)
          [ Msg.InvestigatorDamage iid' source 1 0
          , assignRestOfHealthDamage
              (damageTargets <> [InvestigatorTarget iid'])
              mempty
          ]
      healthDamageMessages =
        [ damageInvestigator investigatorId
        | InvestigatorTarget investigatorId `elem` allowedDamage
        ]
          <> map damageAsset (filter ((`elem` allowedDamage) . toTarget) healthDamageableAssets)
          <> map damageInvestigator (filter ((`elem` allowedDamage) . toTarget) healthDamageableInvestigators)
    player <- getPlayer iid
    push $ chooseOne player healthDamageMessages
    pure a
  InvestigatorDoAssignDamage iid source DamageEvenly matcher 0 sanity damageTargets horrorTargets | iid == toId a -> do
    sanityDamageableAssets <-
      toList <$> getSanityDamageableAssets iid matcher source sanity damageTargets horrorTargets
    sanityDamageableInvestigators <- select $ InvestigatorCanBeAssignedHorrorBy iid
    mustBeDamagedFirstBeforeInvestigator <-
      select
        ( AssetCanBeAssignedHorrorBy iid
            <> AssetWithModifier NonDirectHorrorMustBeAssignToThisFirst
            <> AssetCanBeDamagedBySource source
        )
    let
      onlyAssets = any (`elem` mustBeDamagedFirstBeforeInvestigator) sanityDamageableAssets
      allowedDamage =
        findFewestOccurrences
          horrorTargets
          $ map toTarget sanityDamageableAssets
          <> [InvestigatorTarget iid | not onlyAssets]
          <> (if not onlyAssets then map toTarget sanityDamageableInvestigators else [])
      assignRestOfSanityDamage =
        InvestigatorDoAssignDamage
          investigatorId
          source
          DamageEvenly
          matcher
          0
          (sanity - 1)
      -- N.B. we have to add to the end of targets to handle the drop logic
      damageAsset aid =
        ComponentLabel
          (AssetComponent aid HorrorToken)
          [ Msg.AssignAssetDamageWithCheck aid source 0 1 False
          , assignRestOfSanityDamage mempty (horrorTargets <> [AssetTarget aid])
          ]
      damageInvestigator iid' =
        ComponentLabel
          (InvestigatorComponent iid' HorrorToken)
          [ Msg.InvestigatorDamage iid' source 0 1
          , assignRestOfSanityDamage
              mempty
              (horrorTargets <> [InvestigatorTarget iid'])
          ]
      sanityDamageMessages =
        [ damageInvestigator investigatorId
        | InvestigatorTarget investigatorId `elem` allowedDamage
        ]
          <> map damageAsset (filter ((`elem` allowedDamage) . toTarget) sanityDamageableAssets)
          <> map damageInvestigator (filter ((`elem` allowedDamage) . toTarget) sanityDamageableInvestigators)
    player <- getPlayer iid
    push $ chooseOne player sanityDamageMessages
    pure a
  InvestigatorDoAssignDamage iid _ DamageEvenly _ _ _ _ _ | iid == investigatorId -> do
    error "DamageEvenly only works with just horror or just damage, but not both"
  InvestigatorDoAssignDamage iid source SingleTarget matcher health sanity damageTargets horrorTargets | iid == toId a -> do
    healthDamageableAssets <-
      getHealthDamageableAssets iid matcher source health damageTargets horrorTargets
    healthDamageableInvestigators <- select $ InvestigatorCanBeAssignedDamageBy iid
    sanityDamageableAssets <-
      getSanityDamageableAssets iid matcher source sanity damageTargets horrorTargets
    sanityDamageableInvestigators <- select $ InvestigatorCanBeAssignedHorrorBy iid

    mustBeAssignedDamageFirstBeforeInvestigator <- forMaybeM (toList healthDamageableAssets) \aid -> do
      mods <- getModifiers aid
      let n = sum [x | NonDirectDamageMustBeAssignToThisN x <- mods]
      let mustAssignRemaining = n > 0 && health <= n && count (== toTarget aid) damageTargets < n
      pure $ guard (NonDirectDamageMustBeAssignToThisFirst `elem` mods || mustAssignRemaining) $> aid

    mustBeAssignedHorrorFirstBeforeInvestigator <-
      select
        $ AssetCanBeAssignedHorrorBy iid
        <> AssetWithModifier NonDirectHorrorMustBeAssignToThisFirst
        <> AssetCanBeDamagedBySource source

    let
      damageableAssets = toList $ healthDamageableAssets `union` sanityDamageableAssets
      damageableInvestigators = nub $ healthDamageableInvestigators <> sanityDamageableInvestigators
      onlyAssets =
        (any (`elem` mustBeAssignedDamageFirstBeforeInvestigator) damageableAssets && health > 0)
          || (any (`elem` mustBeAssignedHorrorFirstBeforeInvestigator) damageableAssets && sanity > 0)
      continue h s t =
        InvestigatorDoAssignDamage
          iid
          source
          SingleTarget
          matcher
          (max 0 $ health - h)
          (max 0 $ sanity - s)
          (damageTargets <> [t | h > 0])
          (horrorTargets <> [t | s > 0])
      toAssetMessage (asset, (h, s)) =
        TargetLabel
          (AssetTarget asset)
          [ Msg.AssignAssetDamageWithCheck asset source (min h health) (min s sanity) False
          , continue h s (AssetTarget asset)
          ]
      toInvestigatorMessage (hank, (h, s)) =
        TargetLabel
          (InvestigatorTarget hank)
          [ Msg.InvestigatorDamage hank source (min h health) (min s sanity)
          , continue h s (InvestigatorTarget hank)
          ]
    assetsWithCounts <- for damageableAssets $ \asset -> do
      health' <- fieldMap AssetRemainingHealth (fromMaybe 0) asset
      sanity' <- fieldMap AssetRemainingSanity (fromMaybe 0) asset
      pure (asset, (health', sanity'))
    investigatorsWithCounts <- for damageableInvestigators $ \hank -> do
      health' <- field InvestigatorRemainingHealth hank
      sanity' <- field InvestigatorRemainingSanity hank
      pure (hank, (health', sanity'))

    player <- getPlayer iid
    push
      $ chooseOne player
      $ [ targetLabel
            a
            [ Msg.InvestigatorDamage investigatorId source health sanity
            , continue health sanity (toTarget a)
            ]
        | not onlyAssets
        ]
      <> map toAssetMessage assetsWithCounts
      <> (if not onlyAssets then map toInvestigatorMessage investigatorsWithCounts else [])
    pure a
  InvestigatorDoAssignDamage iid source strategy matcher health sanity damageTargets horrorTargets | iid == toId a -> do
    healthDamageMessages <-
      if health > 0
        then do
          healthDamageableAssets <-
            toList <$> getHealthDamageableAssets iid matcher source health damageTargets horrorTargets
          healthDamageableInvestigators <- select $ InvestigatorCanBeAssignedDamageBy iid
          let
            assignRestOfHealthDamage rest =
              InvestigatorDoAssignDamage investigatorId source strategy matcher rest sanity
            damageAsset aid applyAll =
              AssetDamageLabel
                aid
                [ Msg.AssignAssetDamageWithCheck aid source (if applyAll then health else 1) 0 False
                , assignRestOfHealthDamage
                    (if applyAll then 0 else health - 1)
                    (AssetTarget aid : damageTargets)
                    horrorTargets
                ]
            damageInvestigator iid' applyAll =
              DamageLabel
                iid'
                [ Msg.InvestigatorDamage iid' source (if applyAll then health else 1) 0
                , assignRestOfHealthDamage
                    (if applyAll then 0 else health - 1)
                    (toTarget iid' : damageTargets)
                    horrorTargets
                ]
          let
            go = \case
              DamageAssetsFirst -> do
                let
                  targetCount =
                    if null healthDamageableAssets
                      then 1 + length healthDamageableInvestigators
                      else length healthDamageableAssets
                  applyAll = targetCount == 1
                pure
                  $ [damageInvestigator iid applyAll | null healthDamageableAssets]
                  <> map (`damageInvestigator` applyAll) healthDamageableInvestigators
                  <> map (`damageAsset` applyAll) healthDamageableAssets
              DamageDirect -> pure [damageInvestigator iid True]
              DamageFromHastur -> go DamageAny
              DamageAny -> do
                mustBeAssignedDamage <- flip filterM healthDamageableAssets \aid -> do
                  mods <- getModifiers aid
                  let n = sum [x | NonDirectDamageMustBeAssignToThisN x <- mods]
                  pure $ n > 0 && health <= n && count (== toTarget aid) damageTargets < n

                mustBeAssignedDamageFirstBeforeInvestigator <- flip filterM healthDamageableAssets \aid -> do
                  mods <- getModifiers aid
                  pure $ NonDirectDamageMustBeAssignToThisFirst `elem` mods

                let
                  targetCount =
                    if
                      | null mustBeAssignedDamage && null mustBeAssignedDamageFirstBeforeInvestigator ->
                          1 + length healthDamageableAssets + length healthDamageableInvestigators
                      | null mustBeAssignedDamage -> length healthDamageableAssets + length healthDamageableInvestigators
                      | otherwise -> length mustBeAssignedDamage

                let applyAll = null mustBeAssignedDamage && targetCount == 1

                pure
                  $ [ damageInvestigator iid applyAll
                    | null mustBeAssignedDamage && null mustBeAssignedDamageFirstBeforeInvestigator
                    ]
                  <> map
                    (`damageAsset` applyAll)
                    (if null mustBeAssignedDamage then healthDamageableAssets else mustBeAssignedDamage)
                  <> map
                    (`damageInvestigator` applyAll)
                    (guard (null mustBeAssignedDamage) *> healthDamageableInvestigators)
              DamageFirst def -> do
                validAssets <-
                  List.intersect healthDamageableAssets
                    <$> select (matcher <> assetControlledBy iid <> assetIs def)
                pure
                  $ if null validAssets
                    then
                      damageInvestigator iid False
                        : map (`damageAsset` False) healthDamageableAssets
                          <> map (`damageInvestigator` False) healthDamageableInvestigators
                    else map (`damageAsset` False) validAssets
              SingleTarget -> error "handled elsewhere"
              DamageEvenly -> error "handled elsewhere"
          go strategy
        else pure []
    sanityDamageMessages <-
      if sanity > 0
        then do
          sanityDamageableAssets <-
            toList <$> getSanityDamageableAssets iid matcher source sanity damageTargets horrorTargets
          sanityDamageableInvestigators <- select $ InvestigatorCanBeAssignedHorrorBy iid
          let
            assignRestOfSanityDamage rest =
              InvestigatorDoAssignDamage investigatorId source strategy matcher health rest
            damageInvestigator iid' applyAll =
              HorrorLabel
                iid'
                [ Msg.InvestigatorDamage iid' source 0 (if applyAll then sanity else 1)
                , assignRestOfSanityDamage
                    (if applyAll then 0 else sanity - 1)
                    damageTargets
                    (toTarget iid' : horrorTargets)
                ]
            damageAsset aid applyAll =
              AssetHorrorLabel
                aid
                [ Msg.AssignAssetDamageWithCheck aid source 0 (if applyAll then sanity else 1) False
                , assignRestOfSanityDamage
                    (if applyAll then 0 else sanity - 1)
                    damageTargets
                    (toTarget aid : horrorTargets)
                ]
          let
            go = \case
              DamageAssetsFirst -> do
                let
                  targetCount =
                    if null sanityDamageableAssets
                      then 1 + length sanityDamageableInvestigators
                      else length sanityDamageableAssets
                  applyAll = targetCount == 1

                pure $ [damageInvestigator iid applyAll | null sanityDamageableAssets]
                  <> map (`damageAsset` applyAll) sanityDamageableAssets
                  <> map (`damageInvestigator` applyAll) sanityDamageableInvestigators
              DamageDirect -> pure [damageInvestigator iid True]
              DamageFromHastur -> go DamageAny
              DamageAny -> do
                mustBeAssignedHorrorFirstBeforeInvestigator <- flip filterM sanityDamageableAssets \aid -> do
                  mods <- getModifiers aid
                  pure $ NonDirectHorrorMustBeAssignToThisFirst `elem` mods
                let
                  targetCount =
                    if null mustBeAssignedHorrorFirstBeforeInvestigator
                      then 1 + length sanityDamageableAssets + length sanityDamageableInvestigators
                      else length sanityDamageableAssets + length sanityDamageableInvestigators

                let applyAll = targetCount == 1

                pure $ [damageInvestigator iid applyAll | null mustBeAssignedHorrorFirstBeforeInvestigator]
                  <> map (`damageAsset` applyAll) sanityDamageableAssets
                  <> map (`damageInvestigator` applyAll) sanityDamageableInvestigators
              DamageFirst def -> do
                validAssets <-
                  List.intersect sanityDamageableAssets
                    <$> select (matcher <> assetControlledBy iid <> assetIs def)
                pure
                  $ if null validAssets
                    then
                      damageInvestigator iid False
                        : map (`damageAsset` False) sanityDamageableAssets
                          <> map (`damageInvestigator` False) sanityDamageableInvestigators
                    else map (`damageAsset` False) validAssets
              SingleTarget -> error "handled elsewhere"
              DamageEvenly -> error "handled elsewhere"
          go strategy
        else pure []
    player <- getPlayer iid
    push $ chooseOne player $ healthDamageMessages <> sanityDamageMessages
    pure a
  Investigate investigation | investigation.investigator == investigatorId && investigation.isAction -> do
    handleSkillTestNesting_ investigation.skillTest msg do
      let (beforeWindowMsg, _, afterWindowMsg) = frame (Window.PerformAction investigatorId #investigate)
      modifiers <- getModifiers @LocationId investigation.location
      modifiers' <- getModifiers a
      let
        investigateCost = foldr applyModifier 1 modifiers
        applyModifier (AdditionalActionCostOf (IsAction Action.Investigate) m) n = max 0 (n + m)
        applyModifier _ n = n
      pushAll
        $ [ BeginAction
          , beforeWindowMsg
          , TakeActions investigatorId [#investigate] (ActionCost investigateCost)
          ]
        <> [ CheckAttackOfOpportunity investigatorId False
           | ActionDoesNotCauseAttacksOfOpportunity #investigate `notElem` modifiers'
           ]
        <> [ toMessage $ investigation {investigateIsAction = False}
           , afterWindowMsg
           , FinishAction
           , TakenActions investigatorId [#investigate]
           ]
    pure a
  GainClues iid source n | iid == investigatorId -> do
    window <- checkWindows ((`mkWindow` Window.GainsClues iid source n) <$> [#when, #after])
    pushAll [window, PlaceTokens source (toTarget iid) Clue n, After (GainClues iid source n)]
    pure a
  FlipClues target n | isTarget a target -> do
    pure $ a & tokensL %~ flipClues n
  DiscoverClues iid d | iid == investigatorId -> do
    mods <- getModifiers iid
    lid <- fromJustNote "missing location" <$> getDiscoverLocation iid d

    let additionalDiscoveredAt =
          Map.fromListWith (<>) [(olid, Sum x) | DiscoveredCluesAt olid x <- mods, olid /= lid]
    let additionalDiscovered = getSum $ fold [Sum x | d.isInvestigate == IsInvestigate, DiscoveredClues x <- mods]

    let
      total lid' n = do
        let
          getMaybeMax :: ModifierType -> Maybe Int -> Maybe Int
          getMaybeMax (MaxCluesDiscovered x) Nothing = Just x
          getMaybeMax (MaxCluesDiscovered x) (Just x') = Just $ min x x'
          getMaybeMax _ x = x
        mMax :: Maybe Int <- foldr getMaybeMax Nothing <$> getModifiers lid'
        pure $ maybe n (min n) mMax

    canDiscoverClues <- getCanDiscoverClues d.isInvestigate iid lid
    if canDiscoverClues
      then do
        base <- total lid (d.count + additionalDiscovered)
        discoveredClues <- min base <$> field LocationClues lid
        checkWindowMsg <- checkWindows [mkWhen (Window.WouldDiscoverClues iid lid d.source discoveredClues)]

        otherWindows <- forMaybeM (mapToList additionalDiscoveredAt) \(lid', n) -> runMaybeT do
          liftGuardM $ getCanDiscoverClues d.isInvestigate iid lid'
          discoveredClues' <- lift $ min <$> total lid' (getSum n) <*> field LocationClues lid'
          guard (discoveredClues' > 0)
          lift $ checkWindows [mkWhen (Window.WouldDiscoverClues iid lid' d.source discoveredClues')]
        pushAll $ checkWindowMsg : otherWindows <> [DoStep 1 msg]
      else do
        tokens <- field LocationTokens lid
        putStrLn $ "Can't discover clues in " <> tshow lid <> ": " <> tshow tokens

    pure a
  DoStep 1 (DiscoverClues iid d) | iid == investigatorId -> do
    mods <- getModifiers iid
    let additionalDiscoveredAt = Map.fromListWith (<>) [(olid, Sum x) | DiscoveredCluesAt olid x <- mods]
    let additionalDiscovered = getSum (fold [Sum x | d.isInvestigate == IsInvestigate, DiscoveredClues x <- mods])

    lid <- fromJustNote "missing location" <$> getDiscoverLocation iid d

    let
      total lid' n = do
        let
          getMaybeMax :: ModifierType -> Maybe Int -> Maybe Int
          getMaybeMax (MaxCluesDiscovered x) Nothing = Just x
          getMaybeMax (MaxCluesDiscovered x) (Just x') = Just $ min x x'
          getMaybeMax _ x = x
        mMax :: Maybe Int <- foldr getMaybeMax Nothing <$> getModifiers lid'
        pure $ maybe n (min n) mMax

    canDiscoverClues <- getCanDiscoverClues d.isInvestigate iid lid
    if canDiscoverClues
      then do
        base <- total lid (d.count + additionalDiscovered)
        discoveredClues <- min base <$> field LocationClues lid
        pushAll
          [ Do
              $ DiscoverClues iid
              $ d
                { discoverCount = discoveredClues
                , discoverThen = guard (discoveredClues >= d.discoverCount) *> d.discoverThen
                }
          ]
      else do
        tokens <- field LocationTokens lid
        putStrLn $ "Can't discover clues in " <> tshow lid <> ": " <> tshow tokens

    for_ (mapToList additionalDiscoveredAt) \(lid', n) -> do
      whenM (getCanDiscoverClues d.isInvestigate iid lid') do
        discoveredClues' <- min <$> total lid' (getSum n) <*> field LocationClues lid'
        when (discoveredClues' > 0) do
          checkWindowMsg' <- checkWindows [mkWhen (Window.DiscoverClues iid lid' d.source discoveredClues')]
          pushAll
            [ checkWindowMsg'
            , Do
                $ DiscoverClues iid
                $ d {discoverLocation = DiscoverAtLocation lid', discoverCount = discoveredClues'}
            ]
    pure a
  Do (DiscoverClues iid d) | iid == investigatorId -> do
    lid <- fromJustNote "missing location" <$> getDiscoverLocation iid d
    canDiscoverClues <- getCanDiscoverClues d.isInvestigate iid lid
    if canDiscoverClues
      then do
        locationClues <- field LocationClues lid
        let lastClue = locationClues - d.count <= 0 && locationClues /= 0
        let clueCount = min locationClues d.count
        locationWindowsBefore <- checkWindows [mkWhen (Window.DiscoverClues iid lid d.source clueCount)]
        locationWindowsAfter <-
          checkWindows $ mkAfter (Window.DiscoverClues iid lid d.source clueCount)
            : mkAfter (Window.GainsClues iid d.source clueCount)
            : [mkAfter (Window.DiscoveringLastClue iid lid) | lastClue]

        pushAll
          $ [ locationWindowsBefore
            , UpdateHistory iid (HistoryItem HistoryCluesDiscovered $ singletonMap lid clueCount)
            , RemoveClues d.source (LocationTarget lid) clueCount
            , After $ GainClues iid d.source clueCount
            , locationWindowsAfter
            ]
          <> d.discoverThen
        send $ format a <> " discovered " <> pluralize clueCount "clue"
        pure $ a & tokensL %~ addTokens Clue clueCount
      else pure a
  InvestigatorDiscardAllClues _ iid | iid == investigatorId -> do
    pure $ a & tokensL %~ removeAllTokens Clue
  MoveAllCluesTo source target | not (isTarget a target) -> do
    when (investigatorClues a > 0) (push $ PlaceTokens source target Clue $ investigatorClues a)
    assets <- select $ assetControlledBy a.id <> AssetWithAnyClues
    for_ assets \aid -> do
      clues <- field AssetClues aid
      push $ MoveTokens source (AssetSource aid) target Clue clues
    pure $ a & tokensL %~ removeAllTokens Clue
  InitiatePlayCardAsChoose iid card choices msgs chosenCardStrategy payment windows' asAction | iid == toId a -> do
    event <- selectJust $ EventWithCardId $ toCardId card
    player <- getPlayer iid
    push
      $ chooseOne player
      $ [ targetLabel
            (toCardId choice)
            [ ReturnToHand iid (toTarget event)
            , InitiatePlayCardAs iid card choice msgs chosenCardStrategy payment windows' asAction
            ]
        | choice <- choices
        ]
    pure a
  InitiatePlayCardAs iid card choice msgs chosenCardStrategy payment windows' asAction | iid == toId a -> do
    let
      choiceDef = toCardDef choice
      choiceAsCard =
        (lookupPlayerCard choiceDef $ toCardId card)
          { pcOriginalCardCode = toCardCode card
          , pcCustomizations = choice.customizations
          , pcMutated = choice.mutated
          , pcOwner = Just iid
          }
      chosenCardMsgs = case chosenCardStrategy of
        LeaveChosenCard -> []
        RemoveChosenCardFromGame -> [RemovePlayerCardFromGame True choice]

    pushAll
      $ chosenCardMsgs
      <> msgs
      <> [InitiatePlayCard iid (PlayerCard choiceAsCard) Nothing payment windows' asAction]
    pure $ a & handL %~ (PlayerCard choiceAsCard :) . filter (/= card)
  InitiatePlayCard iid card mtarget payment windows' asAction | iid == investigatorId -> do
    -- we need to check if the card is first an AsIfInHand card, if it is, then we let the owning entity handle this message
    modifiers' <- getModifiers (toTarget a)
    let
      shouldSkip = flip any modifiers' $ \case
        AsIfInHand card' -> card == card'
        _ -> False
    unless shouldSkip $ do
      afterPlayCard <- checkWindows [mkAfter (Window.PlayCard iid $ Window.CardPlay card asAction)]
      if cdSkipPlayWindows (toCardDef card)
        then push $ PlayCard iid card mtarget payment windows' asAction
        else
          pushAll
            [ CheckWindows [mkWhen (Window.PlayCard iid $ Window.CardPlay card asAction)]
            , PlayCard iid card mtarget payment windows' asAction
            , afterPlayCard
            , ResolvedPlayCard iid card
            ]
    pure a
  CardEnteredPlay _ card -> do
    pure
      $ a
      & (handL %~ filter (/= card))
      & (discardL %~ filter ((/= card) . PlayerCard))
      & (deckL %~ Deck . filter ((/= card) . PlayerCard) . unDeck)
      & (cardsUnderneathL %~ filter ((/= card) . toCard))
      & (foundCardsL . each %~ filter (/= card))
  InitDeck iid murl _ | iid == investigatorId -> do
    pure $ a & deckUrlL .~ murl
  UpgradeDeck iid murl _ | iid == investigatorId -> do
    pure $ a & deckUrlL .~ murl
  ObtainCard cardId -> do
    pure
      $ a
      & (handL %~ filter ((/= cardId) . toCardId))
      & (discardL %~ filter ((/= cardId) . toCardId))
      & (deckL %~ Deck . filter ((/= cardId) . toCardId) . unDeck)
      & (cardsUnderneathL %~ filter ((/= cardId) . toCardId))
      & (foundCardsL . each %~ filter ((/= cardId) . toCardId))
      & (bondedCardsL %~ filter ((/= cardId) . toCardId))
      & (decksL . each %~ filter ((/= cardId) . toCardId))
  ReplaceCard cardId card -> do
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
  PutCampaignCardIntoPlay iid cardDef | iid == investigatorId -> do
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
  InvestigatorPlayAsset iid aid | iid == investigatorId -> do
    -- It might seem weird that we remove the asset from the slots here since
    -- we haven't added it yet, however in the case that an asset adds some
    -- additional slot we'll have called RefillSlots already, and then this
    -- will have taken up the necessary slot so we remove it here so it can be
    -- placed correctly later
    pushAll
      [ InvestigatorClearUnusedAssetSlots iid [aid]
      , InvestigatorAdjustAssetSlots iid aid
      , Do (InvestigatorPlayAsset iid aid)
      ]
    pure $ a & slotsL %~ removeFromSlots aid
  InvestigatorAdjustAssetSlots iid aid | iid == investigatorId -> do
    slots <- field AssetSlots aid
    assetCard <- field AssetCard aid
    let
      adjustableSlotsFor = \case
        AdjustableSlot _ _ stypes _ -> stypes
        _ -> []
      adjustableSlots sType =
        filterM
          ( \(sType', slot) ->
              andM
                [pure $ sType /= sType', pure $ sType `elem` adjustableSlotsFor slot, canPutIntoSlot assetCard slot]
          )
          $ concatMap (\(t, bs) -> (t,) <$> bs)
          $ mapToList (a ^. slotsL)
    choices <- concatForM slots $ \sType -> do
      slots' <- adjustableSlots sType
      for slots' \(sType', slot) -> do
        card <- sourceToCard (slotSource slot)
        pure
          $ Label
            ("Change slot from " <> toTitle card <> " to " <> tshow sType)
            [InvestigatorAdjustSlot iid slot sType' sType]

    when (notNull choices) do
      player <- getPlayer iid
      push $ chooseSome player "Do not change slots" choices

    pure a
  InvestigatorAdjustSlot iid slot fromSlotType toSlotType | iid == investigatorId -> do
    push $ RefillSlots iid []
    pure
      $ a
      & (slotsL %~ ix fromSlotType %~ filter (/= slot))
      & (slotsL %~ at toSlotType . non [] %~ (emptySlot slot :))
  InvestigatorClearUnusedAssetSlots iid xs | iid == investigatorId -> do
    updatedSlots <- for (mapToList investigatorSlots) \(slotType, slots) -> do
      slots' <- flip evalStateT [] do
        for slots \slot -> do
          case slotItems slot of
            [] -> pure slot
            assets -> do
              ignored <- flip filterM assets \aid -> do
                cardId <- field AssetCardId aid
                orM
                  [ fieldMap AssetSlots (notElem slotType) aid
                  , hasModifier (AssetTarget aid) (DoNotTakeUpSlot slotType)
                  , hasModifier (CardIdTarget cardId) (DoNotTakeUpSlot slotType)
                  ]
              assetsToRemove :: [[AssetId]] <-
                forMaybeM assets \aid -> runMaybeT do
                  guard $ aid `notElem` ignored
                  seenCount <- count (== aid) <$> get
                  assetSlots <- field AssetSlots aid
                  let slotCount = count (== slotType) assetSlots
                  let lessSlots = if seenCount == slotCount then 1 else 0
                  when (lessSlots == 0) $ modify (aid :)
                  pure $ replicate lessSlots aid
              pure $ foldr removeIfMatchesOnce (foldr removeIfMatches slot ignored) (concat assetsToRemove)
      pure (slotType, slots')
    push $ RefillSlots iid xs
    pure $ a & slotsL .~ mapFromList updatedSlots
  Do (InvestigatorPlayAsset iid aid) | iid == investigatorId -> do
    -- this asset might already be slotted so check first
    fitsSlots <- fitsAvailableSlots aid a
    case fitsSlots of
      FitsSlots -> push (InvestigatorPlayedAsset iid aid)
      MissingSlots missingSlotTypes -> do
        assetsThatCanProvideSlots <-
          select
            $ assetControlledBy iid
            <> DiscardableAsset
            <> AssetOneOf (map AssetInSlot missingSlotTypes)

        -- N.B. This is explicitly for Empower Self and it's possible we don't want to do this without checking
        let assetsInSlotsOf aid' = nub $ concat $ filter (elem aid') $ map slotItems $ concat $ toList (a ^. slotsL)

        player <- getPlayer iid
        push
          $ if null assetsThatCanProvideSlots
            then InvestigatorPlayedAsset iid aid
            else
              chooseOne player
                $ [ targetLabel
                      aid'
                      $ map (toDiscardBy iid GameSource) assets
                      <> [ InvestigatorPlayAsset iid aid
                         ]
                  | aid' <- assetsThatCanProvideSlots
                  , let assets = assetsInSlotsOf aid'
                  ]
    pure a
  InvestigatorPlayedAsset iid aid | iid == investigatorId -> do
    slotTypes <- field AssetSlots aid
    assetCard <- field AssetCard aid

    canHoldMap :: Map SlotType [SlotType] <- do
      mods <- getModifiers a
      let
        canHold = \case
          SlotCanBe slotType canBeSlotType -> insertWith (<>) slotType [canBeSlotType]
          _ -> id
      pure $ foldr canHold mempty mods
    -- we need to figure out which slots are or aren't available
    -- we've claimed we can play this, but we might need to change the slotType
    let
      slotsWithoutAsset = removeFromSlots aid a.slots
      handleSlotType :: HasGame m => Map SlotType [Slot] -> SlotType -> m (Map SlotType [Slot])
      handleSlotType slots' sType = do
        available <- availableSlotTypesFor sType canHoldMap assetCard slotsWithoutAsset
        case nub available of
          [] -> pure slots' -- if no available slots we must have to put this into play
          [sType'] -> do
            slots'' <- placeInAvailableSlot aid assetCard (slots' ^. at sType' . non [])
            pure $ slots' & ix sType' .~ slots''
          xs | sType `elem` xs -> do
            slots'' <- placeInAvailableSlot aid assetCard (slots' ^. at sType . non [])
            pure $ slots' & ix sType .~ slots''
          _ -> error "Too many slots found, we expect at max two and one must be the original slot"

    slots <- foldM handleSlotType slotsWithoutAsset slotTypes

    pure $ a & handL %~ filter (/= assetCard) & slotsL .~ slots
  RemoveCampaignCard cardDef -> do
    pure
      $ a
      & (deckL %~ Deck . filter ((/= toCardCode cardDef) . toCardCode) . unDeck)
  RemoveAllCopiesOfCardFromGame iid cardCode | iid == investigatorId -> do
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
  PutCardIntoPlay _ card _ _ _ -> do
    pure
      $ a
      & (deckL %~ Deck . filter ((/= card) . PlayerCard) . unDeck)
      & (discardL %~ filter ((/= card) . PlayerCard))
      & (handL %~ filter (/= card))
  Msg.InvestigatorDamage iid source damage horror | iid == investigatorId -> do
    mods <- getModifiers a
    let damage' = damage + sum [x | DamageTaken x <- mods]
    let horror' = horror + sum [x | HorrorTaken x <- mods]
    if CancelOneDamageOrHorror `elem` mods
      then
        if
          | horror' == 0 && damage' > 0 ->
              push $ DoStep 1 $ Msg.InvestigatorDamage iid source (damage' - 1) horror'
          | damage' == 0 && horror' > 0 ->
              push $ DoStep 1 $ Msg.InvestigatorDamage iid source damage' (horror' - 1)
          | otherwise -> Choose.chooseOneM iid do
              Choose.labeled "Cancel 1 damage"
                $ push
                $ DoStep 1
                $ Msg.InvestigatorDamage iid source (damage' - 1) horror'
              Choose.labeled "Cancel 1 horror"
                $ push
                $ DoStep 1
                $ Msg.InvestigatorDamage iid source damage' (horror' - 1)
      else push $ DoStep 1 $ Msg.InvestigatorDamage iid source damage' horror'
    pure a
  DoStep 1 (Msg.InvestigatorDamage iid _ damage horror) | iid == investigatorId -> do
    pure $ a & assignedHealthDamageL +~ max 0 damage & assignedSanityDamageL +~ max 0 horror
  DrivenInsane iid | iid == investigatorId -> do
    pure $ a & mentalTraumaL .~ investigatorSanity & drivenInsaneL .~ True & defeatedL .~ True
  CheckDefeated source (isTarget a -> True) | not (a ^. defeatedL || a ^. resignedL) -> do
    facingDefeat <- getFacingDefeat a
    if facingDefeat
      then do
        modifiedHealth <- field InvestigatorHealth (toId a)
        modifiedSanity <- field InvestigatorSanity (toId a)
        let
          defeatedByHorror =
            investigatorSanityDamage a
              + investigatorAssignedSanityDamage
              >= modifiedSanity
          defeatedByDamage =
            investigatorHealthDamage a
              + investigatorAssignedHealthDamage
              >= modifiedHealth
          defeatedBy = case (defeatedByHorror, defeatedByDamage) of
            (True, True) -> DefeatedByDamageAndHorror source
            (True, False) -> DefeatedByHorror source
            (False, True) -> DefeatedByDamage source
            (False, False) -> DefeatedByOther source
        windowMsg <- checkWindows [mkWhen $ Window.InvestigatorWouldBeDefeated defeatedBy (toId a)]
        pushAll
          [ windowMsg
          , AssignDamage (InvestigatorTarget $ toId a)
          , InvestigatorWhenDefeated source investigatorId
          ]
      else do
        push $ AssignDamage (InvestigatorTarget $ toId a)
        when (investigatorAssignedHealthDamage > 0 || investigatorAssignedSanityDamage > 0) do
          pushM
            $ Helpers.checkWindows
            $ [ mkAfter $ Window.PlacedToken source (toTarget a) Damage investigatorAssignedHealthDamage
              | investigatorAssignedHealthDamage > 0
              ]
            <> [ mkAfter $ Window.PlacedToken source (toTarget a) Horror investigatorAssignedSanityDamage
               | investigatorAssignedSanityDamage > 0
               ]

    pure a
  AssignDamage target | isTarget a target -> do
    push $ AssignedDamage target

    pure
      $ a
      & tokensL
      %~ ( addTokens Token.Damage investigatorAssignedHealthDamage
             . addTokens Horror investigatorAssignedSanityDamage
         )
      & (assignedHealthDamageL .~ 0)
      & (assignedSanityDamageL .~ 0)
      & (unhealedHorrorThisRoundL +~ investigatorAssignedSanityDamage)
  CancelAssignedDamage target damageReduction horrorReduction | isTarget a target -> do
    pure
      $ a
      & (assignedHealthDamageL %~ max 0 . subtract damageReduction)
      & (assignedSanityDamageL %~ max 0 . subtract horrorReduction)
  ApplyHealing source -> do
    cannotHealHorror <- hasModifier a CannotHealHorror
    cannotHealDamage <- hasModifier a CannotHealDamage
    let health = if cannotHealDamage then 0 else findWithDefault 0 source investigatorAssignedHealthHeal
    let sanity = if cannotHealHorror then 0 else findWithDefault 0 source investigatorAssignedSanityHeal

    when (health > 0 || sanity > 0) do
      pushM
        $ checkWindows
        $ [mkWhen (Window.Healed DamageType (toTarget a) source health) | health > 0]
        <> [mkWhen (Window.Healed HorrorType (toTarget a) source sanity) | sanity > 0]
      push $ Do msg
    pure a
  Do (ApplyHealing source) -> do
    cannotHealHorror <- hasModifier a CannotHealHorror
    cannotHealDamage <- hasModifier a CannotHealDamage
    let health = if cannotHealDamage then 0 else findWithDefault 0 source investigatorAssignedHealthHeal
    let sanity = if cannotHealHorror then 0 else findWithDefault 0 source investigatorAssignedSanityHeal

    let overHealDamage = max 0 (health - a.healthDamage)
    let overHealSanity = max 0 (sanity - a.sanityDamage)

    pushWhen (overHealDamage > 0) $ ExcessHealDamage a.id source overHealDamage
    pushWhen (overHealSanity > 0) $ ExcessHealHorror a.id source overHealSanity

    when (health > 0 || sanity > 0) do
      pushM
        $ checkWindows
        $ [mkAfter (Window.Healed DamageType (toTarget a) source health) | health > 0]
        <> [mkAfter (Window.Healed HorrorType (toTarget a) source sanity) | sanity > 0]
      push $ AssignedHealing (toTarget a)

    let trueHealth = min health a.healthDamage
    let trueSanity = min sanity a.sanityDamage

    a' <-
      if trueHealth > 0
        then liftRunMessage (RemoveTokens source (toTarget a) #damage trueHealth) a
        else pure a
    a'' <-
      if trueSanity > 0
        then liftRunMessage (RemoveTokens source (toTarget a) #horror trueSanity) a'
        else pure a'

    pure
      $ a''
      & (unhealedHorrorThisRoundL %~ min 0 . subtract sanity)
      & (assignedHealthHealL %~ deleteMap source)
      & (assignedSanityHealL %~ deleteMap source)
  HealDamage (InvestigatorTarget iid) source amount' | iid == investigatorId -> do
    mods <- getModifiers a
    cannotHealDamage <- hasModifier a CannotHealDamage
    let canHealAtFullSources = [sourceMatcher | CanHealAtFull sourceMatcher DamageType <- mods]
    canHealAtFull <-
      if null canHealAtFullSources
        then pure False
        else sourceMatches source (mconcat canHealAtFullSources)
    unless cannotHealDamage do
      let n = sum [x | HealingTaken x <- mods]
      let amount = amount' + n
      whenWindow <- checkWindows [mkWhen $ Window.Healed DamageType (toTarget a) source amount]
      dmgTreacheries <-
        selectWithField TreacheryCard $ treacheryInThreatAreaOf iid <> TreacheryWithModifier IsPointOfDamage
      if null dmgTreacheries
        then do
          let remainingDamage = investigatorHealthDamage a - sum (toList investigatorAssignedHealthHeal)
          when (remainingDamage > 0 || canHealAtFull) do
            pushAll [whenWindow, Do msg]
        else do
          player <- getPlayer iid
          push
            $ chooseOne player
            $ [ Label
                  ("Heal " <> toTitle c)
                  $ toDiscardBy iid source t
                  : [HealDamage (InvestigatorTarget iid) source (amount - 1) | amount - 1 > 0]
              | (t, c) <- dmgTreacheries
              ]
            <> [Label "Heal remaining damage normally" [whenWindow, Do msg] | investigatorHealthDamage a > 0]
    pure a
  Do (HealDamage (InvestigatorTarget iid) source amount) | iid == investigatorId -> do
    cannotHealDamage <- hasModifier a CannotHealDamage
    unless cannotHealDamage do
      pushAll [HealDamageDelayed (InvestigatorTarget iid) source amount, ApplyHealing source]
    -- if cannotHealDamage
    --   then pure a
    --   else do
    --     push $ AssignedHealing (toTarget a)
    --     afterWindow <- checkWindows [mkAfter $ Window.Healed DamageType (toTarget a) source amount]
    --     push afterWindow
    --     liftRunMessage (RemoveTokens source (toTarget a) #damage amount) a
    pure a
  HealDamageDelayed (isTarget a -> True) source n -> do
    cannotHealDamage <- hasModifier a CannotHealDamage
    if cannotHealDamage
      then pure a
      else pure $ a & assignedHealthHealL %~ insertWith (+) source n
  HealHorrorWithAdditional (InvestigatorTarget iid) _source amount | iid == investigatorId -> do
    -- exists to have no callbacks, and to be resolved with AdditionalHealHorror
    cannotHealHorror <- hasModifier a CannotHealHorror
    if cannotHealHorror
      then pure a
      else do
        let totalHealed = min amount (investigatorSanityDamage a)
        pure $ a & (horrorHealedL .~ totalHealed)
  AdditionalHealHorror (InvestigatorTarget iid) source additional | iid == investigatorId -> do
    -- exists to have Callbacks for the total, get from investigatorHorrorHealed
    -- TODO: HERE  MAYBE
    cannotHealHorror <- hasModifier a CannotHealHorror
    if cannotHealHorror
      then pure $ a & horrorHealedL .~ 0
      else do
        push $ HealHorror (toTarget iid) source (view horrorHealedL a + additional)
        pure $ a & (horrorHealedL .~ 0)
  HealHorror (InvestigatorTarget iid) source amount' | iid == investigatorId -> do
    mods <- getModifiers a
    let n = sum [x | HealingTaken x <- mods]
    let amount = amount' + n
    cannotHealHorror <- hasModifier a CannotHealHorror
    unless cannotHealHorror
      $ pushAll [HealHorrorDelayed (InvestigatorTarget iid) source amount, ApplyHealing source]
    pure a
  HealHorrorDelayed target@(isTarget a -> True) source n | n > 0 -> do
    cannotHealHorror <- hasModifier a CannotHealHorror

    -- afterWindow <- checkWindows [mkAfter $ Window.Healed #horror (toTarget a) source n]

    unless cannotHealHorror do
      hrrTreacheries <-
        selectWithField TreacheryCard $ treacheryInThreatAreaOf investigatorId
          <> TreacheryWithModifier IsPointOfHorror
      mods <- getModifiers a
      let onlyTargets = [targetLabel t [HealHorror t source 1] | CannotHealHorrorOnOtherCards t <- mods]
      let additionalTargets =
            guard (null onlyTargets)
              *> [targetLabel t [HealHorror t source 1] | HealHorrorAsIfOnInvestigator t x <- mods, x > 0]

      let remainingHorror = length hrrTreacheries + investigatorSanityDamage a - sum (toList investigatorAssignedSanityHeal)
      if null additionalTargets && null onlyTargets
        then do
          let canHealAtFullSources = [sourceMatcher | CanHealAtFull sourceMatcher DamageType <- mods]
          canHealAtFull <-
            if null canHealAtFullSources
              then pure False
              else sourceMatches source (mconcat canHealAtFullSources)
          when (remainingHorror > 0 || canHealAtFull) do
            push $ Do msg
        else do
          player <- getPlayer a.id
          pushAll
            [ chooseOne player
                $ [HorrorLabel a.id [Do $ HealHorrorDelayed target source 1] | remainingHorror > 0, null onlyTargets]
                <> additionalTargets
                <> onlyTargets
            , HealHorrorDelayed target source (n - 1)
            ]

    pure a
  Do (HealHorrorDelayed (isTarget a -> True) source n) -> do
    let iid = investigatorId
    cannotHealHorror <- hasModifier a CannotHealHorror
    unless cannotHealHorror do
      hrrTreacheries <-
        selectWithField TreacheryCard $ treacheryInThreatAreaOf iid <> TreacheryWithModifier IsPointOfHorror
      if null hrrTreacheries
        then push $ Do (HealHorror (InvestigatorTarget iid) source n)
        else do
          player <- getPlayer iid
          push
            $ chooseOne player
            $ [ Label
                  ("Heal " <> toTitle c)
                  $ toDiscardBy iid source t
                  : [Do (HealHorrorDelayed (InvestigatorTarget iid) source (n - 1)) | n - 1 > 0]
              | (t, c) <- hrrTreacheries
              ]
            <> [ Label "Heal remaining horror normally" [Do (HealHorror (toTarget iid) source n)]
               | investigatorSanityDamage a > 0
               ]
    pure a
  Do (HealHorror (isTarget a -> True) source n) -> do
    cannotHealHorror <- hasModifier a CannotHealHorror
    if cannotHealHorror
      then pure a
      else pure $ a & assignedSanityHealL %~ insertWith (+) source n
  MoveTokens s _ (isTarget a -> True) tType amount -> liftRunMessage (PlaceTokens s (toTarget a) tType amount) a
  MoveTokens s (isSource a -> True) _target tType amount | amount > 0 -> do
    case tType of
      Clue -> do
        push $ ForInvestigator investigatorId msg
        pure a
      _ -> liftRunMessage (RemoveTokens s (toTarget a) tType amount) a
  ForInvestigator iid (MoveTokens s source@(isSource a -> True) target Clue amount) | amount > 0 && iid == investigatorId -> do
    assetsWithClues <- selectWithField AssetClues (assetControlledBy a.id <> AssetWithAnyClues)
    let total = sum (map snd assetsWithClues) + investigatorClues a
    if total == amount
      then do
        for_ assetsWithClues \(aid, n) -> do
          push $ RemoveTokens s (AssetTarget aid) Clue n
          push $ PlaceTokens s target Clue n
        when (investigatorClues a > 0) do
          push $ RemoveTokens s (toTarget a) Clue (investigatorClues a)
          push $ PlaceTokens s target Clue (investigatorClues a)
      else do
        if null assetsWithClues
          then do
            push $ RemoveTokens s (toTarget a) Clue amount
            push $ PlaceTokens s target Clue (min amount (investigatorClues a))
          else do
            player <- getPlayer iid
            push
              $ chooseOne player
              $ [ ClueLabel
                    a.id
                    [ RemoveTokens s (toTarget a) Clue 1
                    , PlaceTokens s target Clue 1
                    , ForInvestigator iid (MoveTokens s source target Clue (amount - 1))
                    ]
                ]
              <> [ targetLabel
                     aid
                     [ RemoveTokens s (toTarget aid) Clue 1
                     , PlaceTokens s target Clue 1
                     , ForInvestigator iid (MoveTokens s source target Clue (amount - 1))
                     ]
                 | (aid, _) <- assetsWithClues
                 ]
    pure a
  MoveTokensNoDefeated s _ target tType n | isTarget a target -> do
    liftRunMessage (PlaceTokens s (toTarget a) tType n) a
  MoveTokensNoDefeated s source _ tType n | isSource a source -> do
    liftRunMessage (RemoveTokens s (toTarget a) tType n) a
  MoveTokens s (ResourceSource iid) _ _ n | iid == investigatorId -> liftRunMessage (RemoveTokens s (toTarget a) #resource n) a
  MoveTokens s _ (ResourceTarget iid) _ n | iid == investigatorId -> liftRunMessage (PlaceTokens s (toTarget a) #resource n) a
  ReassignHorror (isSource a -> True) _ n -> do
    pure $ a & assignedSanityDamageL %~ max 0 . subtract n
  HealHorrorDirectly (InvestigatorTarget iid) source amount | iid == investigatorId -> do
    -- USE ONLY WHEN NO CALLBACKS
    a' <-
      if amount > 0 then liftRunMessage (RemoveTokens source (toTarget a) #horror amount) a else pure a
    pure
      $ a'
      & (unhealedHorrorThisRoundL %~ min 0 . subtract amount)
  HealDamageDirectly (InvestigatorTarget iid) source amount | iid == investigatorId && amount > 0 -> do
    -- USE ONLY WHEN NO CALLBACKS
    cannotHealDamage <- hasModifier a CannotHealDamage
    if cannotHealDamage
      then pure a
      else liftRunMessage (RemoveTokens source (toTarget a) #damage amount) a
  InvestigatorWhenDefeated source iid | iid == investigatorId -> do
    modifiedHealth <- field InvestigatorHealth (toId a)
    modifiedSanity <- field InvestigatorSanity (toId a)
    let
      defeatedByHorror = investigatorSanityDamage a >= modifiedSanity
      defeatedByDamage = investigatorHealthDamage a >= modifiedHealth
      defeatedBy = case (defeatedByHorror, defeatedByDamage) of
        (True, True) -> DefeatedByDamageAndHorror source
        (True, False) -> DefeatedByHorror source
        (False, True) -> DefeatedByDamage source
        (False, False) -> DefeatedByOther source
    windowMsg <- checkWindows [mkWhen $ Window.InvestigatorDefeated defeatedBy iid]
    pushAll [windowMsg, InvestigatorIsDefeated source iid]
    pure a
  InvestigatorKilled source iid | iid == investigatorId -> do
    unless investigatorDefeated do
      isLead <- (== iid) <$> getLead
      pushAll $ [ChooseLeadInvestigator | isLead] <> [Msg.InvestigatorDefeated source iid]
    pure $ a & defeatedL .~ True & endedTurnL .~ True & killedL .~ True
  MoveAllTo source lid | not (a ^. defeatedL || a ^. resignedL) -> do
    push $ MoveTo $ (move source investigatorId lid) {moveMeans = Place}
    pure a
  MoveTo movement | isTarget a (moveTarget movement) -> do
    cancelled <- hasModifier a CannotMove
    unless cancelled $ push $ Do msg
    pure a
  Do (MoveTo movement) | isTarget a (moveTarget movement) -> do
    case moveDestination movement of
      ToLocationMatching matcher -> do
        lids <- select matcher
        player <- getPlayer investigatorId
        push
          $ chooseOrRunOne
            player
            [targetLabel lid [MoveTo $ movement {moveDestination = ToLocation lid}] | lid <- lids]
        pure a
      ToLocation lid -> do
        let iid = investigatorId

        moveWith <-
          if movement.means == Place
            then pure []
            else do
              select (InvestigatorWithModifier (CanMoveWith $ InvestigatorWithId iid) <> colocatedWith iid)
                >>= filterM (\iid' -> getCanMoveTo iid' (moveSource movement) lid)
                >>= traverse (traverseToSnd getPlayer)

        afterMoveButBeforeEnemyEngagement <-
          Helpers.checkWindows [mkAfter (Window.MovedButBeforeEnemyEngagement iid lid)]

        afterEntering <- checkAfter $ Window.Entering iid lid

        pushAll
          $ [ WhenWillEnterLocation iid lid
            , Do (WhenWillEnterLocation iid lid)
            , After (WhenWillEnterLocation iid lid)
            , EnterLocation iid lid
            ]
          <> [ chooseOne
                 player
                 [ Label "Move too" [Move $ move iid' iid' lid]
                 , Label "Skip" []
                 ]
             | movement.means /= Place
             , (iid', player) <- moveWith
             ]
          <> moveAfter movement
          <> [afterEntering]
          <> [afterMoveButBeforeEnemyEngagement | movement.means /= Place]
          <> [CheckEnemyEngagement iid]
        pure a
  Do (WhenWillEnterLocation iid lid) | iid == investigatorId -> do
    pure $ a & placementL .~ AtLocation lid
  CheckEnemyEngagement iid | iid == investigatorId -> do
    -- [AsIfAt]: enemies don't move to threat with AsIf, so we use actual location here
    -- This might not be correct and we should still check engagement and let
    -- that handle whether or not to move to threat area
    enemies <- select $ EnemyAt $ locationWithInvestigator iid
    pushAll [EnemyCheckEngagement eid | eid <- enemies]
    pure a
  AddSlot iid slotType slot | iid == investigatorId -> do
    let
      slots = findWithDefault [] slotType investigatorSlots
      emptiedSlots = sort $ slot : map emptySlot slots
    push $ RefillSlots a.id []
    pure $ a & slotsL %~ insertMap slotType emptiedSlots
  RemoveSlot iid slotType | iid == investigatorId -> do
    -- This arbitrarily removes the first slot of the given type provided that
    -- it is granted by the investigator
    push $ RefillSlots a.id []
    pure $ a & slotsL . ix slotType %~ deleteFirstMatch (isSource a . slotSource)
  RemoveSlotFrom iid source slotType | iid == investigatorId -> do
    -- This arbitrarily removes the first slot of the given type provided that
    -- it is granted by the investigator
    push $ RefillSlots a.id []
    pure $ a & slotsL . ix slotType %~ deleteFirstMatch (isSource source . slotSource)
  RefillSlots iid xs | iid == investigatorId && not investigatorEliminated -> do
    assetIds <- select $ mapOneOf AssetWithPlacement [InPlayArea iid, InThreatArea iid]

    requirements <- concatForM assetIds \assetId -> do
      assetCard <- field AssetCard assetId
      mods <- getCombinedModifiers [toTarget assetId, toTarget assetCard]
      let slotFilter sType = DoNotTakeUpSlot sType `notElem` mods
      slots <- field AssetSlots assetId
      pure $ (assetId,assetCard,) <$> filter slotFilter slots

    let allSlots :: [(SlotType, Slot)] = concatMap (\(k, vs) -> (k,) . emptySlot <$> vs) $ Map.assocs (a ^. slotsL)

    canHoldMap :: Map SlotType [SlotType] <- do
      mods <- getModifiers a
      let
        canHold = \case
          SlotCanBe slotType canBeSlotType -> insertWith (<>) slotType [canBeSlotType]
          _ -> id
      pure $ foldr canHold mempty mods

    let
      lookupSlot :: SlotType -> [(SlotType, Slot)] -> [Slot]
      lookupSlot k = map snd . filter ((== k) . fst)
      go :: HasGame m => [(AssetId, Card, SlotType)] -> [(SlotType, Slot)] -> m [SlotType]
      go [] _ = pure []
      go rs [] = pure $ map (\(_, _, sType) -> sType) rs
      go ((aid, card, slotType) : rs) slots = do
        (availableSlots1, unused1) <- partitionM (canPutIntoSlot card) (lookupSlot slotType slots)
        case availableSlots1 of
          [] -> case findWithDefault [] slotType canHoldMap of
            [] -> (slotType :) <$> go rs slots
            [other] -> do
              (availableSlots2, unused2) <- partitionM (canPutIntoSlot card) (lookupSlot slotType slots)
              case availableSlots2 of
                [] -> (slotType :) <$> go rs slots
                _ -> do
                  slots' <- placeInAvailableSlot aid card availableSlots2
                  go rs $ filter ((/= other) . fst) slots <> map (other,) slots' <> map (other,) unused2
            _ -> error "not designed to work with more than one yet"
          _ -> do
            slots' <- placeInAvailableSlot aid card availableSlots1
            go rs $ filter ((/= slotType) . fst) slots <> map (slotType,) slots' <> map (slotType,) unused1

    let
      fill :: HasGame m => [(AssetId, Card, SlotType)] -> Map SlotType [Slot] -> m (Map SlotType [Slot])
      fill [] slots = pure slots
      fill ((aid, card, slotType) : rs) slots = do
        (availableSlots1, _unused1) <- partitionM (canPutIntoSlot card) (slots ^. at slotType . non [])
        case availableSlots1 of
          [] -> case findWithDefault [] slotType canHoldMap of
            [] -> pure slots -- suppose we get dendromorphosis and the king in yellow
            [other] -> do
              (availableSlots2, _unused2) <- partitionM (canPutIntoSlot card) (slots ^. at other . non [])
              case availableSlots2 of
                [] -> pure slots
                _ -> do
                  slots' <- placeInAvailableSlot aid card (slots ^. at other . non [])
                  fill rs (slots & at other . non [] .~ slots')
            _ -> error "not designed to work with more than one yet"
          _ -> do
            slots' <- placeInAvailableSlot aid card (slots ^. at slotType . non [])
            fill rs (slots & at slotType . non [] .~ slots')

    failedSlotTypes <- nub <$> go requirements allSlots

    let
      failedSlotTypes' = nub $ concatMap (\s -> s : findWithDefault [] s canHoldMap) failedSlotTypes
      failedAssetIds' = map (\(aid, _, _) -> aid) $ filter (\(_, _, s) -> s `elem` failedSlotTypes') requirements

    failedAssetIds <- selectFilter AssetCanLeavePlayByNormalMeans failedAssetIds'

    -- N.B. This is explicitly for Empower Self and it's possible we don't want to do this without checking
    let assetsInSlotsOf aid = nub $ concat $ filter (elem aid) $ map slotItems $ concat $ toList (a ^. slotsL)

    if null failedAssetIds
      then do
        slots' <- fill requirements (Map.map (map emptySlot) $ a ^. slotsL)
        pure $ a & slotsL .~ slots'
      else do
        player <- getPlayer iid
        push
          $ chooseOne player
          $ [ targetLabel aid' $ map (toDiscardBy iid GameSource) assets <> [RefillSlots iid xs]
            | aid' <- filter (`notElem` xs) failedAssetIds
            , let assets = let ks = assetsInSlotsOf aid' in if null ks then [aid'] else ks
            ]
        pure a
  ChooseEndTurn iid | iid == investigatorId -> pure a
  Do BeginRound -> do
    actionsForTurn <- getAbilitiesForTurn a
    current <- getMaybeLocation a.id
    pure
      $ a
      & (endedTurnL .~ False)
      & (remainingActionsL .~ actionsForTurn)
      & (usedAdditionalActionsL .~ mempty)
      & (actionsTakenL .~ mempty)
      & (actionsPerformedL .~ mempty)
      & (beganRoundAtL .~ current)
      & (unhealedHorrorThisRoundL .~ 0)
  Begin InvestigationPhase -> do
    pure $ a & endedTurnL .~ False
  Again (Begin InvestigationPhase) -> do
    actionsForTurn <- getAbilitiesForTurn a
    pure
      $ a
      & (remainingActionsL .~ actionsForTurn)
      & (usedAdditionalActionsL .~ mempty)
      & (actionsTakenL .~ mempty)
      & (actionsPerformedL .~ mempty)
  DiscardTopOfDeck iid n source mTarget | iid == investigatorId -> do
    let (cs, deck') = draw n investigatorDeck
        (cs', essenceOfTheDreams) = partition ((/= "06113") . toCardCode) cs
    windowMsgs <-
      if null deck'
        then
          pure
            <$> checkWindows
              ((`mkWindow` Window.DeckHasNoCards iid) <$> [#when, #after])
        else pure []
    pushAll
      $ windowMsgs
      <> [DeckHasNoCards investigatorId mTarget | null deck']
      <> [ DiscardedTopOfDeck iid cs source target
         | target <- maybeToList mTarget
         ]
    pure
      $ a
      & (deckL .~ deck')
      & (discardL %~ (reverse cs' <>))
      & (bondedCardsL <>~ map toCard essenceOfTheDreams)
      & (foundCardsL . each %~ filter (`notElem` map toCard cs'))
  DiscardUntilFirst iid' source (Deck.InvestigatorDeck iid) matcher | iid == investigatorId -> do
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
  RevealUntilFirst iid source (Deck.InvestigatorDeck iid') matcher | iid == investigatorId && iid' == iid -> do
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
  DrawStartingHand iid | iid == investigatorId -> do
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
  Instead (DoDrawCards iid) msg' | iid == toId a -> do
    mMsg <-
      maybeToList <$> popMessageMatching \case
        DrawEnded iid' -> iid == iid'
        _ -> False
    pushAll $ mMsg <> [msg']
    pure $ a & drawingL .~ Nothing
  DrawCards iid cardDraw | iid == toId a -> do
    phase <- getPhase
    wouldDrawCard <- checkWindows [mkWhen (Window.WouldDrawCard iid cardDraw.deck)]
    drawEncounterCardWindow <- checkWindows [mkWhen $ Window.WouldDrawEncounterCard a.id phase]
    if cardDrawAction cardDraw
      then do
        beforeWindowMsg <- checkWindows [mkWhen (Window.PerformAction iid #draw)]
        afterWindowMsg <- checkWindows [mkAfter (Window.PerformAction iid #draw)]
        pushAll
          $ [BeginAction, beforeWindowMsg]
          <> [drawEncounterCardWindow | cardDraw.isEncounterDraw]
          <> [ TakeActions iid [#draw] (ActionCost 1)
             , CheckAttackOfOpportunity iid False
             , wouldDrawCard
             , DoDrawCards iid
             , DrawEnded iid
             , afterWindowMsg
             , FinishAction
             , TakenActions iid [#draw]
             ]
      else
        pushAll $ wouldDrawCard
          : [drawEncounterCardWindow | cardDraw.isEncounterDraw] <> [DoDrawCards iid, DrawEnded iid]
    pure $ a & drawingL ?~ cardDraw
  MoveTopOfDeckToBottom _ (Deck.InvestigatorDeck iid) n | iid == investigatorId -> do
    let (cards, deck) = draw n investigatorDeck
    pure $ a & deckL .~ Deck.withDeck (<> cards) deck
  DoDrawCards iid | iid == toId a -> do
    for_ (a ^. drawingL) \d -> do
      push $ Do (DrawCards iid d)
      for_ (cardDrawAndThen d) push
    pure $ a & drawingL .~ Nothing
  ReplaceCurrentCardDraw iid drawing | iid == investigatorId -> do
    pure $ a & drawingL ?~ drawing
  Do (DrawCards iid cardDraw) | iid == toId a && cardDraw.deck == Deck.InvestigatorDeck iid -> do
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

        let
          source = cardDrawSource cardDraw
          n = cardDrawAmount cardDraw

        modifiers' <- getModifiers (toTarget a)
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
            unless (null investigatorDiscard || CardsCannotLeaveYourDiscardPile `elem` modifiers') $ do
              wouldDo
                (EmptyDeck iid (Just $ drawCards iid source n))
                (Window.DeckWouldRunOutOfCards iid)
                (Window.DeckHasNoCards iid)
            -- push $ EmptyDeck iid
            pure a
          else do
            let deck = unDeck investigatorDeck
            if length deck < n
              then do
                push $ drawCards iid source (n - length deck)
                pure $ a & deckL .~ mempty & drawnCardsL %~ (<> deck)
              else do
                let
                  (drawn, deck') = splitAt n deck
                  allDrawn' = investigatorDrawnCards <> drawn
                  (discarded, allDrawn) = maybe ([], allDrawn') (\mtch -> partition (`cardMatch` mtch) allDrawn') cardDraw.discard
                  shuffleBackInEachWeakness = ShuffleBackInEachWeakness `elem` cardDrawRules cardDraw
                  handleCardDraw c = pure $ drawThisCardFrom iid c (Just cardDraw.deck)
                msgs <- if not shuffleBackInEachWeakness then concatMapM handleCardDraw allDrawn else pure []
                player <- getPlayer iid
                let
                  weaknesses = map PlayerCard $ filter (`cardMatch` WeaknessCard) allDrawn
                  msgs' =
                    (<> msgs)
                      $ guard (shuffleBackInEachWeakness && notNull weaknesses)
                      *> [ FocusCards weaknesses
                         , chooseOne
                             player
                             [ Label
                                 "Shuffle Weaknesses back in"
                                 [ UnfocusCards
                                 , ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) weaknesses
                                 ]
                             ]
                         ]

                windowMsgs <-
                  if null deck'
                    then pure <$> checkWindows ((`mkWindow` Window.DeckHasNoCards iid) <$> [#when, #after])
                    else pure []
                let (before, _, after) = frame $ Window.DrawCards iid $ map toCard allDrawn
                checkHandSize <- hasModifier iid CheckHandSizeAfterDraw

                -- Cards with revelations won't be in hand afte they resolve, so exclude them from the discard
                let
                  toDrawDiscard = \case
                    AfterDrawDiscard x -> Sum x
                    _ -> mempty
                let discardable =
                      filter (`cardMatch` (DiscardableCard <> NotCard CardWithRevelation)) allDrawn
                let discardAmount =
                      min (length discardable)
                        $ getSum (foldMap toDrawDiscard (toList $ cardDrawRules cardDraw))
                -- Only focus those that will still be in hand
                let focusable = map toCard $ filter (`cardMatch` NotCard CardWithRevelation) allDrawn

                pushAll
                  $ windowMsgs
                  <> [DeckHasNoCards iid Nothing | null deck']
                  <> [before]
                  <> [toDiscard (cardDrawSource cardDraw) (CardIdTarget card.id) | card <- discarded]
                  <> msgs'
                  <> [after]
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
  InvestigatorDrewPlayerCardFrom iid card mDeck | iid == investigatorId -> do
    hasForesight <- hasModifier iid (Foresight $ toTitle card)
    let uiRevelation = getPlayer iid >>= (`sendRevelation` (toJSON $ toCard card))
    case toCardType card of
      PlayerEnemyType -> pure ()
      _ -> when (hasRevelation card) uiRevelation
    mWhenDraw <- for mDeck \deck ->
      checkWindows [mkWhen $ Window.DrawCard iid (toCard card) deck]
    if hasForesight
      then do
        canCancel <- PlayerCard card <=~> CanCancelRevelationEffect #any
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
                      , ObtainCard card.id
                      , AddToHandQuiet iid [toCard card]
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
                <> [Label "Draw normally" $ maybeToList mWhenDraw <> [UnfocusCards, Do msg]]
            ]
      else pushAll $ FocusCards [toCard card] : maybeToList mWhenDraw <> [UnfocusCards, Do msg]
    pure a
  Do (InvestigatorDrewPlayerCardFrom iid card mdeck) | iid == investigatorId -> do
    mAfterDraw <- for mdeck \deck ->
      checkWindows [mkAfter $ Window.DrawCard iid (toCard card) deck]
    inLimit <- passesLimits iid (toCard card)
    if hasRevelation card && inLimit
      then
        if toCardType card == PlayerTreacheryType
          then pushAll $ DrewTreachery iid Nothing (toCard card) : maybeToList mAfterDraw
          else
            pushAll $ Revelation iid (CardIdSource card.id)
              : maybeToList mAfterDraw <> [ResolvedCard iid $ toCard card]
      else
        if toCardType card == PlayerEnemyType
          then pushAll $ DrewPlayerEnemy iid (toCard card) : maybeToList mAfterDraw
          else for_ mAfterDraw push

    let
      cardFilter :: IsCard c => [c] -> [c]
      cardFilter = filter ((/= card.id) . toCardId)
    doCheck <- hasModifier iid CheckHandSizeAfterDraw
    when doCheck $ push $ CheckHandSize iid
    pure
      $ a
      & (handL %~ nub . (toCard card :))
      & (deckL %~ filter ((/= card.id) . toCardId))
      & (foundCardsL . each %~ cardFilter)
      & (cardsUnderneathL %~ cardFilter)
      & (discardL %~ cardFilter)
      & (bondedCardsL %~ cardFilter)
  InvestigatorSpendClues iid n | iid == investigatorId -> do
    assetsWithClues <- select $ assetControlledBy iid <> AssetWithAnyClues
    if null assetsWithClues
      then push $ Do msg
      else push $ DoStep n msg
    pushM $ checkAfter $ Window.SpentClues iid n
    pure a
  DoStep n msg'@(InvestigatorSpendClues iid _) | n > 0 && iid == investigatorId -> do
    assets <- select $ assetControlledBy iid <> AssetWithAnyClues
    if null assets
      then push $ Do $ InvestigatorSpendClues iid n
      else do
        -- if all we have is enough clues, then spend all of them
        clues <- field InvestigatorClues iid
        if clues == n
          then do
            for_ assets \asset -> do
              assetClues <- field AssetClues asset
              push $ RemoveTokens (toSource iid) (toTarget asset) Clue assetClues
            push $ Do (InvestigatorSpendClues iid $ investigatorClues a)
          else do
            player <- getPlayer iid
            pushAll
              [ chooseOne
                  player
                  $ [ClueLabel iid [Do (InvestigatorSpendClues iid 1)] | investigatorClues a > 0]
                  <> [targetLabel asset [RemoveTokens (toSource iid) (toTarget asset) Clue 1] | asset <- assets]
              , DoStep (n - 1) msg'
              ]
    pure a
  Do (InvestigatorSpendClues iid n) | iid == investigatorId -> do
    pure $ a & tokensL %~ subtractTokens Clue n
  SpendResources iid _ | iid == investigatorId -> do
    push $ Do msg
    pure a
  Do (SpendResources iid n) | iid == investigatorId -> do
    pure $ a & tokensL %~ subtractTokens Resource n
  LoseResources iid source n | iid == investigatorId -> do
    beforeWindowMsg <- checkWindows [mkWhen (Window.LostResources iid source n)]
    afterWindowMsg <- checkWindows [mkAfter (Window.LostResources iid source n)]
    pushAll [beforeWindowMsg, Do msg, afterWindowMsg]
    pure a
  Do (LoseResources iid source n) | iid == investigatorId -> liftRunMessage (RemoveTokens source (toTarget a) #resource n) a
  LoseAllResources iid source | iid == investigatorId -> do
    liftRunMessage (LoseResources iid source a.resources) a
  TakeResources iid n source True | iid == investigatorId -> do
    let ability = restricted iid ResourceAbility (Self <> Never) (ActionAbility [#resource] $ ActionCost 1)
    whenActivateAbilityWindow <- checkWhen $ Window.ActivateAbility iid [] ability
    afterActivateAbilityWindow <- checkAfter $ Window.ActivateAbility iid [] ability
    beforeWindowMsg <- checkWhen $ Window.PerformAction iid #resource
    afterWindowMsg <- checkAfter $ Window.PerformAction iid #resource
    canGain <- can.gain.resources (sourceToFromSource source) iid

    when canGain do
      pushAll
        [ BeginAction
        , beforeWindowMsg
        , whenActivateAbilityWindow
        , TakeActions iid [#resource] (ActionCost 1)
        , CheckAttackOfOpportunity iid False
        , TakeResources iid n source False
        , afterWindowMsg
        , afterActivateAbilityWindow
        , FinishAction
        , TakenActions iid [#resource]
        ]
    pure a
  TakeResources iid n source False | iid == investigatorId -> do
    canGain <- can.gain.resources (sourceToFromSource source) iid
    when canGain do
      beforeWindowMsg <- checkWindows [mkWhen (Window.GainsResources iid source n)]
      pushAll [beforeWindowMsg, Do msg]
    pure a
  Do (TakeResources iid n source False) | iid == investigatorId -> do
    canGain <- can.gain.resources (sourceToFromSource source) iid
    if canGain
      then do
        mods <- getModifiers a
        let additional = sum [x | AdditionalResources x <- mods]
        liftRunMessage (PlaceTokens source (toTarget a) #resource (n + additional)) a
      else pure a
  PlaceTokens source (isTarget a -> True) token n -> do
    when (token == Doom && a.doom == 0) do
      pushM $ checkAfter $ Window.PlacedDoomCounterOnTargetWithNoDoom source (toTarget a) n
    when (token == #damage || token == #horror) do
      push $ checkDefeated source a

    afterPlacedWindowMsg <-
      Helpers.checkWindow $ mkAfter $ Window.PlacedToken source (toTarget a) token n
    push afterPlacedWindowMsg
    pure $ a & tokensL %~ addTokens token n
  RemoveTokens _ (isTarget a -> True) token n -> do
    pure $ a & tokensL %~ subtractTokens token n
  DoBatch _ (EmptyDeck iid mDrawing) | iid == investigatorId -> do
    player <- getPlayer iid
    pushAll
      $ [EmptyDeck iid mDrawing]
      <> maybeToList mDrawing
      <> [chooseOne player [Label "Your deck is empty, take 1 horror" [assignHorror iid EmptyDeckSource 1]]]
    pure a
  EmptyDeck iid _ | iid == investigatorId -> do
    modifiers' <- getModifiers (toTarget a)
    pushWhen (CardsCannotLeaveYourDiscardPile `notElem` modifiers')
      $ ShuffleDiscardBackIn iid
    pure a
  ForInvestigator iid AllDrawCardAndResource | iid == investigatorId && not (a ^. defeatedL || a ^. resignedL) -> do
    whenM (can.draw.cards a.id) $ do
      mods <- getModifiers a
      let alternateUpkeepDraws = [target | AlternateUpkeepDraw target <- mods]
      if notNull alternateUpkeepDraws
        then do
          pid <- getPlayer investigatorId
          push
            $ chooseOrRunOne
              pid
              [ targetLabel target [SendMessage target (ForInvestigator investigatorId AllDrawCardAndResource)]
              | target <- alternateUpkeepDraws
              ]
        else push $ drawCards investigatorId ScenarioSource 1
    push $ ForTarget (toTarget a) (DoStep 2 (ForInvestigator investigatorId AllDrawCardAndResource))
    pure a
  ForTarget (isTarget a -> True) (DoStep 2 (ForInvestigator _ AllDrawCardAndResource)) | not (a ^. defeatedL || a ^. resignedL) -> do
    lift $ takeUpkeepResources a
  LoadDeck iid deck | iid == investigatorId -> do
    shuffled <- shuffleM $ flip map (unDeck deck) $ \card ->
      card {pcOwner = Just iid}
    pure $ a & deckL .~ Deck shuffled
  LoadSideDeck iid deck | iid == investigatorId -> do
    pure $ a & sideDeckL ?~ deck
  InvestigatorCommittedCard iid card | iid == investigatorId -> do
    let commitedCardWindows = Helpers.windows [Window.CommittedCard iid card]
    pushAll $ FocusCards [card] : commitedCardWindows <> [UnfocusCards]
    pure
      $ a
      & (handL %~ filter (/= card))
      & (deckL %~ Deck . filter ((/= card) . PlayerCard) . unDeck)
  PlaceUnderneath target cards | isTarget a target -> do
    update <-
      appEndo
        <$> foldMapM
          ( \card -> do
              mAssetId <- selectOne $ AssetWithCardId $ toCardId card
              pure
                $ Endo
                $ (slotsL %~ maybe id removeFromSlots mAssetId)
                . (deckL %~ Deck . filter ((/= card) . PlayerCard) . unDeck)
                . (handL %~ filter (/= card))
                . (discardL %~ filter ((/= card) . PlayerCard))
          )
          cards
    pure $ a & cardsUnderneathL <>~ cards & update
  PlaceUnderneath _ cards -> do
    update <-
      appEndo
        . mconcat
        <$> traverse
          ( \card -> do
              mAssetId <- selectOne $ AssetWithCardId $ toCardId card
              pure
                $ Endo
                $ (slotsL %~ maybe id removeFromSlots mAssetId)
                . (deckL %~ Deck . filter ((/= card) . PlayerCard) . unDeck)
                . (handL %~ filter (/= card))
          )
          cards
    let a' = a & update & foundCardsL %~ Map.map (filter (`notElem` cards))
    when (null a'.deck) do
      pushM $ checkWhen $ Window.DeckHasNoCards investigatorId
      pushM $ checkAfter $ Window.DeckHasNoCards investigatorId
    pure a'
  ChaosTokenCanceled iid source token | iid == investigatorId -> do
    whenWindow <- checkWindows [mkWhen (Window.CancelChaosToken iid token)]
    whenWindow2 <- checkWindows [mkWhen (Window.CancelledOrIgnoredCardOrGameEffect source)]
    afterWindow <- checkWindows [mkAfter (Window.CancelChaosToken iid token)]
    afterWindow2 <- checkWindows [mkAfter (Window.CancelledOrIgnoredCardOrGameEffect source)]
    pushAll [whenWindow, whenWindow2, afterWindow2, afterWindow]
    pure a
  ChaosTokenIgnored iid source token | iid == toId a -> do
    whenWindow <- checkWindows [mkWhen (Window.IgnoreChaosToken iid token)]
    whenWindow2 <- checkWindows [mkWhen (Window.CancelledOrIgnoredCardOrGameEffect source)]
    afterWindow <- checkWindows [mkAfter (Window.IgnoreChaosToken iid token)]
    afterWindow2 <- checkWindows [mkAfter (Window.CancelledOrIgnoredCardOrGameEffect source)]
    pushAll [whenWindow, whenWindow2, afterWindow2, afterWindow]
    pure a
  BeforeSkillTest skillTestId -> do
    getSkillTestInvestigator >>= traverse_ \iid' -> do
      when (iid' == a.id) do
        skillTestModifiers' <- getModifiers skillTestId
        push
          $ if RevealChaosTokensBeforeCommittingCards `elem` skillTestModifiers'
            then StartSkillTest investigatorId
            else CommitToSkillTest skillTestId $ StartSkillTestButton investigatorId
    pure a
  CommitToSkillTest _skillTestId _ -> do
    getSkillTestInvestigator >>= traverse_ \iid' -> do
      when (iid' == a.id) do
        investigators <- getInvestigators
        for_ investigators \i -> do
          mustBeCommitted <- getMustBeCommittableCards i
          for_ mustBeCommitted $ push . SkillTestCommitCard investigatorId
        push $ Do msg
    pure a
  Do (CommitToSkillTest skillTestId triggerMessage') -> do
    getSkillTest >>= traverse_ \skillTest -> do
      let iid = skillTestInvestigator skillTest
      when (iid == a.id) do
        committedCards <- field InvestigatorCommittedCards iid
        uncommittableCards <- filterM (`withoutModifier` MustBeCommitted) committedCards
        let window = mkWhen (Window.SkillTest $ skillTestType skillTest)
        actions <- getActions iid [window]

        skillTestModifiers' <- getModifiers (SkillTestTarget skillTest.id)
        committableCards <- getCommittableCards (toId a)
        let
          mustCommit = any (elem MustBeCommittedToYourTest . cdCommitRestrictions . toCardDef) committableCards
          triggerMessage =
            [ triggerMessage'
            | CannotPerformSkillTest `notElem` skillTestModifiers' && not mustCommit
            ]
          beginMessage = Do (CommitToSkillTest skillTestId triggerMessage')
        player <- getPlayer iid
        if notNull committableCards || notNull uncommittableCards || notNull actions
          then
            push
              $ SkillTestAsk
              $ chooseOne player
              $ map
                (\card -> targetLabel (toCardId card) [SkillTestCommitCard iid card, beginMessage])
                committableCards
              <> [ targetLabel (toCardId card) [SkillTestUncommitCard iid card, beginMessage]
                 | card <- uncommittableCards
                 ]
              <> map
                (\action -> AbilityLabel iid action [window] [] [beginMessage])
                actions
              <> triggerMessage
          else
            pushWhen (notNull triggerMessage)
              $ SkillTestAsk
              $ chooseOne player triggerMessage
      when (iid /= a.id) do
        committedCards <- field InvestigatorCommittedCards investigatorId
        let beginMessage = Do (CommitToSkillTest skillTestId triggerMessage')
        committableCards <- getCommittableCards a.id
        uncommittableCards <- filterM (`withoutModifier` MustBeCommitted) committedCards
        player <- getPlayer investigatorId
        pushWhen (notNull committableCards || notNull uncommittableCards)
          $ SkillTestAsk
          $ chooseOne player
          $ map
            (\card -> targetLabel (toCardId card) [SkillTestCommitCard investigatorId card, beginMessage])
            committableCards
          <> [ targetLabel (toCardId card) [SkillTestUncommitCard investigatorId card, beginMessage]
             | card <- uncommittableCards
             ]
    pure a
  CheckWindows windows | not (investigatorDefeated || investigatorResigned) || Window.hasEliminatedWindow windows -> do
    pure $ a & skippedWindowL .~ False
  SkippedWindow iid | iid == investigatorId -> do
    pure $ a & skippedWindowL .~ True
  Do (CheckWindows windows)
    | not investigatorSkippedWindow
        && (not (investigatorDefeated || investigatorResigned) || Window.hasEliminatedWindow windows) -> do
        actions <- getActions a.id windows
        playableCards <-
          if not (investigatorDefeated || investigatorResigned)
            then getPlayableCards a a (UnpaidCost NeedsAction) windows
            else pure []
        runWindow a windows actions playableCards
        pure a
  SpendActions iid _ _ 0 | iid == investigatorId -> do
    pure a
  SpendActions iid source mAction n | iid == investigatorId -> do
    -- We want to try and spend the most restrictive action so we get any
    -- action that is not any additional action first, and if not that then the
    -- any additional action
    additionalActions <- getAdditionalActions a
    specificAdditionalActions <-
      filterM
        ( andM
            . sequence
              [ additionalActionCovers source (toList mAction)
              , pure . ((/= AnyAdditionalAction) . additionalActionType)
              ]
        )
        additionalActions
    let anyAdditionalActions = filter ((== AnyAdditionalAction) . additionalActionType) additionalActions

    case specificAdditionalActions of
      [] -> case anyAdditionalActions of
        [] -> pure $ a & remainingActionsL %~ max 0 . subtract n
        xs -> do
          player <- getPlayer iid
          push
            $ chooseOrRunOne player
            $ [ Label
                  ("Use action from " <> lbl)
                  [LoseAdditionalAction iid ac, SpendActions iid source mAction (n - 1)]
              | ac@(AdditionalAction lbl _ _) <- xs
              ]
          pure a
      xs -> do
        player <- getPlayer iid
        push
          $ chooseOrRunOne player
          $ [ Label
                ("Use action from " <> lbl)
                [LoseAdditionalAction iid ac, SpendActions iid source mAction (n - 1)]
            | ac@(AdditionalAction lbl _ _) <- xs
            ]
        pure a
  UseEffectAction iid eid _ | iid == investigatorId -> do
    additionalActions <- getAdditionalActions a
    let
      isEffectAction aAction = case additionalActionType aAction of
        EffectAction _ eid' -> eid == eid'
        _ -> False
    case find isEffectAction additionalActions of
      Nothing -> pure a
      Just aAction -> pure $ a & usedAdditionalActionsL %~ (aAction :)
  LoseActions iid source n | iid == investigatorId -> do
    beforeWindowMsg <- checkWindows [mkWhen $ Window.LostActions iid source n]
    afterWindowMsg <- checkWindows [mkAfter $ Window.LostActions iid source n]
    pushAll [beforeWindowMsg, Do msg, afterWindowMsg]
    pure a
  Do (LoseActions iid _source n) | iid == investigatorId -> do
    -- TODO: after losing all remaining actions we can lose additional actions
    additionalActions <- getAdditionalActions a
    let
      remaining = max 0 (n - a ^. remainingActionsL)
      additional = min remaining (length additionalActions)
      a' = a & remainingActionsL %~ max 0 . subtract n
    if additional > 0
      then
        if additional == length additionalActions
          then pure $ a' & usedAdditionalActionsL <>~ additionalActions
          else do
            player <- getPlayer iid
            push
              $ chooseN player additional
              $ [ Label ("Lose action from " <> lbl) [LoseAdditionalAction iid ac]
                | ac@(AdditionalAction lbl _ _) <- additionalActions
                ]
            pure a'
      else pure a'
  SetActions iid _ 0 | iid == investigatorId -> do
    additionalActions <- getAdditionalActions a
    pure
      $ a
      & remainingActionsL
      .~ 0
      & usedAdditionalActionsL
      .~ nub (investigatorUsedAdditionalActions <> additionalActions)
  SetActions iid _ n | iid == investigatorId -> do
    pure $ a & remainingActionsL .~ n
  SetAsideCards cards -> do
    pure
      $ a
      & (handL %~ filter (`notElem` cards))
      & (discardL %~ filter ((`notElem` cards) . PlayerCard))
      & (deckL %~ Deck . filter ((`notElem` cards) . PlayerCard) . unDeck)
  GainActions iid _ n | iid == investigatorId -> do
    -- TODO: If we add a window here we need to reconsider Ace of Rods, likely it would need a Do variant
    pure $ a & remainingActionsL +~ n
  LoseAdditionalAction iid n | iid == investigatorId -> do
    pure $ a & usedAdditionalActionsL %~ (n :)
  TakeActions iid actions cost | iid == investigatorId -> do
    push $ PayForAbility (abilityEffect a actions cost) []
    pure a
  TakenActions iid actions | iid == investigatorId -> do
    let previous = fromMaybe [] $ lastMay investigatorActionsPerformed
    let duplicated = actions `List.intersect` previous
    let streak = longestUniqueStreak (actions : reverse investigatorActionsPerformed)

    when (notNull duplicated)
      $ pushM
      $ checkWindows [mkAfter (Window.PerformedSameTypeOfAction iid duplicated)]

    when (length streak > 1)
      $ pushM
      $ checkWindows
        [mkAfter (Window.PerformedDifferentTypesOfActionsInARow iid (length streak) (nub $ concat streak))]

    when (#parley `elem` actions && #parley `notElem` previous)
      $ pushM
      $ checkWindows [mkWhen (Window.FirstTimeParleyingThisRound iid)]

    pure $ a & actionsTakenL %~ (<> [actions]) & actionsPerformedL %~ (<> [actions])
  PerformedActions iid actions | iid == investigatorId -> do
    let previous = fromMaybe [] $ lastMay investigatorActionsPerformed
    let duplicated = actions `List.intersect` previous

    when (notNull duplicated)
      $ pushM
      $ checkWindows [mkAfter (Window.PerformedSameTypeOfAction iid duplicated)]

    when (#parley `elem` actions && #parley `notElem` previous)
      $ pushM
      $ checkWindows [mkWhen (Window.FirstTimeParleyingThisRound iid)]

    pure $ a & actionsPerformedL %~ (<> [actions])
  PutCardOnTopOfDeck _ (Deck.InvestigatorDeck iid) card | iid == toId a -> do
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
  PutCardOnTopOfDeck _ _ card -> case card of
    PlayerCard pc ->
      pure
        $ a
        & (deckL %~ Deck . filter (/= pc) . unDeck)
        & (handL %~ filter (/= card))
        & (discardL %~ filter (/= pc))
        & (foundCardsL . each %~ filter (/= PlayerCard pc))
    EncounterCard _ -> pure $ a & handL %~ filter (/= card)
    VengeanceCard vcard -> pure $ a & handL %~ filter (/= vcard)
  PutCardOnBottomOfDeck _ (Deck.InvestigatorDeck iid) card | iid == toId a -> do
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
  PutCardOnBottomOfDeck _ _ card -> case card of
    PlayerCard pc ->
      pure
        $ a
        & (deckL %~ Deck . filter (/= pc) . unDeck)
        & (handL %~ filter (/= card))
        & (discardL %~ filter (/= pc))
        & (foundCardsL . each %~ filter (/= PlayerCard pc))
    EncounterCard _ -> pure a
    VengeanceCard _ -> pure a
  DebugAddToHand iid cardId | iid == investigatorId -> do
    card <- getCard cardId
    liftRunMessage (AddToHandQuiet iid [card]) a
  AddToHand iid cards | iid == investigatorId -> do
    for_ cards obtainCard
    for_ (reverse cards) \case
      PlayerCard pc -> push $ InvestigatorDrewPlayerCardFrom iid pc Nothing
      EncounterCard ec -> push $ InvestigatorDrewEncounterCard iid ec
      VengeanceCard {} -> error "Can not add vengeance card to hand"
    assetIds <- catMaybes <$> for cards (selectOne . AssetWithCardId . toCardId)
    pure
      $ a
      & (cardsUnderneathL %~ filter (`notElem` cards))
      & (slotsL %~ flip (foldr removeFromSlots) assetIds)
      & (discardL %~ filter ((`notElem` cards) . PlayerCard))
      & (foundCardsL . each %~ filter (`notElem` cards))
      & (bondedCardsL %~ filter (`notElem` cards))
      & (deckL %~ Deck . filter ((`notElem` cards) . PlayerCard) . unDeck)
      & (decksL . each %~ filter (`notElem` cards))
  DrawToHandFrom iid deck cards | iid == investigatorId -> do
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
  DrawToHand iid cards | iid == investigatorId -> do
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
  AddToHandQuiet iid cards | iid == investigatorId -> do
    for_ cards obtainCard
    push $ Do msg
    pure a
  Do (AddToHandQuiet iid cards) | iid == investigatorId -> do
    assetIds <- catMaybes <$> for cards (selectOne . AssetWithCardId . toCardId)
    pure
      $ a
      & (cardsUnderneathL %~ filter (`notElem` cards))
      & (slotsL %~ flip (foldr removeFromSlots) assetIds)
      & (discardL %~ filter ((`notElem` cards) . PlayerCard))
      & (foundCardsL . each %~ filter (`notElem` cards))
      & (bondedCardsL %~ filter (`notElem` cards))
      & (handL %~ (<> cards))
      & (decksL . each %~ filter (`notElem` cards))
  SwapPlaces (aTarget, _) (_, newLocation) | a `is` aTarget -> do
    push $ CheckEnemyEngagement a.id
    pure $ a & placementL .~ AtLocation newLocation
  SwapPlaces (_, newLocation) (bTarget, _) | a `is` bTarget -> do
    push $ CheckEnemyEngagement a.id
    pure $ a & placementL .~ AtLocation newLocation
  ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [] | iid == investigatorId -> do
    -- can't shuffle zero cards
    pure a
  ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) cards | iid == toId a -> do
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
  AddFocusedToHand _ (InvestigatorTarget iid') cardSource cardId | iid' == toId a -> do
    let
      card =
        fromJustNote "missing card"
          $ find ((== cardId) . toCardId) (findWithDefault [] cardSource $ a ^. foundCardsL)
      foundCards' = Map.map (filter ((/= cardId) . toCardId)) (a ^. foundCardsL)
    push $ addToHand iid' card
    pure $ a & foundCardsL .~ foundCards' & (deckL %~ Deck . filter ((/= card) . toCard) . unDeck)
  DrawFocusedToHand _ (InvestigatorTarget iid') cardSource cardId | iid' == toId a -> do
    let
      card =
        fromJustNote "missing card"
          $ find ((== cardId) . toCardId) (findWithDefault [] cardSource $ a ^. foundCardsL)
      foundCards' = Map.map (filter ((/= cardId) . toCardId)) (a ^. foundCardsL)
    push $ case zoneToDeck a.id cardSource of
      Nothing -> drawToHand iid' card
      Just deck -> drawToHandFrom iid' deck card
    pure $ a & foundCardsL .~ foundCards' & (deckL %~ Deck . filter ((/= card) . toCard) . unDeck)
  CommitCard _ card -> do
    pure $ a & foundCardsL . each %~ filter (/= card) & discardL %~ filter ((/= card) . toCard)
  AddFocusedToTopOfDeck _ (InvestigatorTarget iid') cardId | iid' == toId a -> do
    let
      card =
        fromJustNote "missing card"
          $ find ((== cardId) . toCardId) (concat $ toList $ a ^. foundCardsL)
          >>= toPlayerCard
      foundCards = Map.map (filter ((/= cardId) . toCardId)) $ a ^. foundCardsL
    push $ PutCardOnTopOfDeck iid' (Deck.InvestigatorDeck iid') (toCard card)
    pure $ a & foundCardsL .~ foundCards
  ShuffleAllFocusedIntoDeck _ (InvestigatorTarget iid') | iid' == investigatorId -> do
    let cards = findWithDefault [] Zone.FromDeck $ a ^. foundCardsL
    push $ ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid') cards
    pure $ a & foundCardsL %~ deleteMap Zone.FromDeck
  PutAllFocusedIntoDiscard _ (InvestigatorTarget iid') | iid' == investigatorId -> do
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
  UpdateSearchReturnStrategy iid zone returnStrategy | iid == investigatorId -> do
    let
      updateZone = \case
        (z@(FromTopOfDeck _), _) | zone == FromDeck -> (z, returnStrategy)
        (z@(FromBottomOfDeck _), _) | zone == FromDeck -> (z, returnStrategy)
        (z, _) | z == zone -> (z, returnStrategy)
        other -> other
    case investigatorSearch of
      Nothing -> error "Invalid call, no search for investigator"
      Just s -> pure $ a & searchL ?~ s {searchZones = map updateZone (searchZones s)}
  EndSearch iid _ (InvestigatorTarget iid') _ | iid == investigatorId -> do
    let cardSources = maybe [] searchZones investigatorSearch
    let
      foundKey = \case
        Zone.FromTopOfDeck _ -> Zone.FromDeck
        Zone.FromBottomOfDeck _ -> Zone.FromDeck
        other -> other
    player <- getPlayer iid
    for_ cardSources $ \(cardSource, returnStrategy) -> case returnStrategy of
      DiscardRest -> do
        push
          $ chooseOneAtATime player
          $ map
            ( \case
                PlayerCard c -> targetLabel (toCardId c) [AddToDiscard iid c]
                EncounterCard c -> targetLabel (toCardId c) [AddToEncounterDiscard c]
                VengeanceCard _ -> error "not possible"
            )
            (findWithDefault [] Zone.FromDeck $ a ^. foundCardsL)
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
      ShuffleBackIn -> do
        when (foundKey cardSource /= Zone.FromDeck) (error "Expects a deck: Investigator<ShuffleBackIn>")
        for_ investigatorSearch \MkSearch {searchType} ->
          pushWhen (searchType == Searching) $ ShuffleDeck (Deck.InvestigatorDeck a.id)
      PutBack -> pure () -- Nothing moves while searching
      DoNothing -> pure () -- Nothing moves while searching
      RemoveRestFromGame -> do
        -- Try to obtain, then don't add back
        pushAll $ map (ObtainCard . toCardId) $ findWithDefault [] Zone.FromDeck (a ^. foundCardsL)

    push (SearchEnded iid)
    pure
      $ a
      & usedAbilitiesL
      %~ filter
        ( \UsedAbility {..} ->
            case abilityLimitType (abilityLimit usedAbility) of
              Just (PerSearch _) -> False
              _ -> True
        )
  EndSearch iid _ _ _ | iid == investigatorId -> do
    pure
      $ a
      & usedAbilitiesL
      %~ filter
        ( \UsedAbility {..} ->
            case abilityLimitType (abilityLimit usedAbility) of
              Just (PerSearch _) -> False
              _ -> True
        )
  SearchEnded iid | iid == investigatorId -> do
    case investigatorSearch of
      Just search' -> do
        when (notNull $ search' ^. Search.drawnCardsL) do
          pushM $ checkWindows [mkAfter $ Window.DrawCards iid $ search' ^. Search.drawnCardsL]
      _ -> pure ()

    pure $ a & searchL .~ Nothing
  CancelSearch iid | iid == investigatorId -> pure $ a & searchL .~ Nothing
  Search (MkSearch searchType iid _ (InvestigatorTarget iid') _ _ _ _ _) | iid' == toId a -> do
    let deck = Deck.InvestigatorDeck iid'
    if searchType == Searching
      then wouldDo msg (Window.WouldSearchDeck iid deck) (Window.SearchedDeck iid deck)
      else do
        batchId <- getRandom
        push $ DoBatch batchId msg

    pure a
  DoBatch _ (Search (MkSearch _ iid _ (InvestigatorTarget iid') _ _ foundStrategy _ _)) | iid' == toId a -> do
    let isDrawing = isSearchDraw foundStrategy
    let deck = Deck.InvestigatorDeck iid'
    wouldDrawCard <- checkWindows [mkWhen (Window.WouldDrawCard iid deck)]
    pushAll $ [wouldDrawCard | isDrawing] <> [Do msg]
    pure a
  Do
    ( DoBatch
        batchId
        ( Search
            ( MkSearch
                searchType
                iid
                source
                target@(InvestigatorTarget iid')
                cardSources
                cardMatcher
                foundStrategy
                _
                _
              )
          )
      ) | iid' == toId a -> do
      mods <- getModifiers iid
      let
        additionalDepth =
          sum [x | searchType == Searching, SearchDepth x <- mods]
            + sum [x | searchType == Looking, LookAtDepth x <- mods]
        foundCards :: Map Zone [Card] =
          foldl'
            ( \hmap (cardSource, _) -> case cardSource of
                Zone.FromDeck ->
                  insertWith (<>) Zone.FromDeck (map PlayerCard $ unDeck investigatorDeck) hmap
                Zone.FromHand -> insertWith (<>) Zone.FromHand investigatorHand hmap
                Zone.FromTopOfDeck n ->
                  insertWith
                    (<>)
                    Zone.FromDeck
                    (map PlayerCard . take (n + additionalDepth) $ unDeck investigatorDeck)
                    hmap
                Zone.FromBottomOfDeck n ->
                  insertWith
                    (<>)
                    Zone.FromDeck
                    (map PlayerCard . take (n + additionalDepth) . reverse $ unDeck investigatorDeck)
                    hmap
                Zone.FromDiscard ->
                  insertWith (<>) Zone.FromDiscard (map PlayerCard investigatorDiscard) hmap
                other -> error $ mconcat ["Zone ", show other, " not yet handled"]
            )
            mempty
            cardSources

      when (searchType == Searching) $ do
        pushBatch batchId
          $ CheckWindows [Window #when (Window.AmongSearchedCards batchId iid) (Just batchId)]

      pushBatch batchId $ ResolveSearch investigatorId
      pushBatch batchId $ EndSearch investigatorId source target cardSources

      pure
        $ a
        & searchL
        ?~ MkSearch searchType iid source target cardSources cardMatcher foundStrategy foundCards []
  ResolveSearch x | x == investigatorId -> do
    case investigatorSearch of
      Just
        ( MkSearch
            searchType
            iid
            source
            (InvestigatorTarget iid')
            _
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
              let
                choices =
                  [ targetLabel
                      card
                      [ if card `elem` playableCards
                          then
                            chooseOne
                              player
                              [ Label "Add to hand" [addFoundToHand]
                              , Label "Play Card" [addFoundToHand, PayCardCost iid card windows']
                              ]
                          else addFoundToHand
                      ]
                  | (zone, cards) <- mapToList targetCards
                  , card <- cards
                  , let addFoundToHand = AddFocusedToHand iid (toTarget who) zone (toCardId card)
                  ]
              push
                $ if null choices
                  then chooseOne player [Label "No cards found" []]
                  else chooseN player (min n (length choices)) choices
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
                              [ Label "Draw it" [addFoundToHand]
                              , Label "Commit to skill test" [CommitCard who card]
                              ]
                          else addFoundToHand
                      ]
                  | (zone, cards) <- mapToList targetCards
                  , card <- cards
                  , let addFoundToHand = AddFocusedToHand iid (toTarget who) zone (toCardId card)
                  ]
              push
                $ if null choices
                  then chooseOne player [Label "No cards found" []]
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
                  then chooseOne player [Label "No cards found" []]
                  else chooseN player (min n (length choices)) choices
            DrawFound who n -> do
              let
                choices =
                  [ targetLabel
                      card
                      [DrawFocusedToHand iid (toTarget who) zone (toCardId card)]
                  | (zone, cards) <- mapToList targetCards
                  , card <- cards
                  ]
              push
                $ if null choices
                  then chooseOne player [Label "No cards found" []]
                  else chooseN player (min n (length choices)) choices
            DrawFoundUpTo who n -> do
              let
                choices =
                  [ targetLabel (toCardId card) [DrawFocusedToHand iid (toTarget who) zone (toCardId card)]
                  | (zone, cards) <- mapToList targetCards
                  , card <- cards
                  ]
              push
                $ if null choices
                  then chooseOne player [Label "No cards found" []]
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
              push $ chooseN player n $ if null choices then [Label "No cards found" []] else choices
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
              push $ chooseN player n $ if null choices then [Label "No cards found" []] else choices
            DeferSearchedToTarget searchTarget _ -> do
              -- N.B. You must handle target duplication (see Mandy Thompson) yourself
              push
                $ if null targetCards
                  then chooseOne player [Label "No cards found" [SearchNoneFound iid searchTarget]]
                  else SearchFound iid searchTarget (Deck.InvestigatorDeck iid') (concat $ toList targetCards)
            DrawAllFound who -> do
              let
                choices =
                  [ targetLabel (toCardId card) [AddFocusedToHand iid (toTarget who) zone (toCardId card)]
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
                    [ chooseOne player [Label "No cards found" [ShuffleDeck (Deck.InvestigatorDeck a.id) | shouldShuffle]]
                    ]
                  else
                    let cards = concat $ toList targetCards
                        (before, _, after) = frame $ Window.DrawCards iid cards
                     in [before, chooseOneAtATime player choices]
                          <> [ ShuffleDeck (Deck.InvestigatorDeck a.id) | shouldShuffle && length targetCards == length foundCards
                             ]
                          <> [after]
            ReturnCards -> pure ()
      _ -> pure ()
    pure a
  RemoveFromDiscard iid cardId | iid == investigatorId -> do
    pure $ a & discardL %~ filter ((/= cardId) . toCardId)
  PlaceInBonded iid card | iid == investigatorId -> do
    pure
      $ a
      & (bondedCardsL %~ nub . (card :))
      & (handL %~ filter (/= card))
      & (discardL %~ filter ((/= card) . toCard))
      & (deckL %~ filter ((/= card) . toCard))
      & (foundCardsL . each %~ filter (/= card))
      & (cardsUnderneathL %~ filter (/= card))
      & (decksL . each %~ filter (/= card))
  SufferTrauma iid physical mental | iid == investigatorId -> do
    push $ CheckTrauma iid
    pure $ a & physicalTraumaL +~ physical & mentalTraumaL +~ mental
  CheckTrauma iid | iid == investigatorId -> do
    pushWhen (investigatorMentalTrauma >= investigatorSanity) $ DrivenInsane iid
    pushWhen (investigatorPhysicalTrauma >= investigatorHealth) $ InvestigatorKilled (toSource a) iid
    pushWhen
      (investigatorMentalTrauma >= investigatorSanity || investigatorPhysicalTrauma >= investigatorHealth)
      CheckForRemainingInvestigators
    pure a
  HealTrauma iid physical mental | iid == investigatorId -> do
    pure
      $ a
      & (physicalTraumaL %~ max 0 . subtract physical)
      & (mentalTraumaL %~ max 0 . subtract mental)
  GainXP iid _ amount | iid == investigatorId -> pure $ a & xpL +~ amount
  SpendXP iid amount | iid == investigatorId -> do
    pure $ a & xpL %~ max 0 . subtract amount
  InvestigatorPlaceCluesOnLocation iid source n | iid == investigatorId -> do
    field InvestigatorLocation iid >>= traverse_ \lid -> do
      assetClues <- selectSum AssetClues $ assetControlledBy iid <> AssetWithAnyClues
      let cluesToPlace = min n (investigatorClues a + assetClues)
      push $ MoveTokens source (toSource a) (LocationTarget lid) Clue cluesToPlace
    pure a
  InvestigatorPlaceAllCluesOnLocation iid source | iid == investigatorId -> do
    -- [AsIfAt] assuming as if is still in effect
    mlid <- field InvestigatorLocation iid
    for_ mlid $ \lid ->
      push $ PlaceTokens source (LocationTarget lid) Clue (investigatorClues a)
    pure $ a & tokensL %~ removeAllTokens Clue
  RemoveFromBearersDeckOrDiscard card -> do
    pure $ a & (discardL %~ filter (/= card)) & (deckL %~ Deck . filter (/= card) . unDeck)
  RemovePlayerCardFromGame _addToRemovedFromGame card -> do
    case preview _PlayerCard card of
      Just pc ->
        pure
          $ a
          & discardL
          %~ filter (/= pc)
          & handL
          %~ filter (/= card)
          & (deckL %~ Deck . filter (/= pc) . unDeck)
          & foundCardsL
          . each
          %~ filter (/= card)
      Nothing ->
        -- encounter cards can only be in hand
        pure $ a & (handL %~ filter (/= card))
  RemoveDiscardFromGame iid | iid == investigatorId -> do
    pushAll $ map (RemovedFromGame . PlayerCard) investigatorDiscard
    pure $ a & discardL .~ []
  After (FailedSkillTest iid mAction _ (InvestigatorTarget iid') _ n) | iid == iid' && iid == toId a -> do
    mTarget <- getSkillTestTarget
    mCurrentLocation <- field InvestigatorLocation iid

    modifiers' <-
      getSkillTestId >>= \case
        Just sid -> getModifiers (toTarget sid)
        Nothing -> pure []

    let
      windows = do
        guard $ CancelEffects `notElem` modifiers'
        Action.Investigate <- maybeToList mAction
        go =<< maybeToList mTarget
      go = \case
        ProxyTarget t _ -> go t
        LocationTarget lid ->
          [ mkWhen $ Window.FailInvestigationSkillTest iid lid n
          , mkAfter $ Window.FailInvestigationSkillTest iid lid n
          ]
        BothTarget t1 t2 -> go t1 <> go t2
        _ -> []
      windows' =
        if null windows && CancelEffects `notElem` modifiers'
          then case mCurrentLocation of
            Just currentLocation ->
              [ mkWhen $ Window.FailInvestigationSkillTest iid currentLocation n
              , mkAfter $ Window.FailInvestigationSkillTest iid currentLocation n
              ]
            _ -> []
          else windows
    pushM
      $ checkWindows
      $ mkWhen (Window.FailSkillTest iid n)
      : mkAfter (Window.FailSkillTest iid n)
      : windows'
    pure a
  When (PassedSkillTest iid mAction source (InvestigatorTarget iid') _ n) | iid == iid' && iid == toId a -> do
    mTarget <- getSkillTestTarget
    let
      windows = do
        Action.Investigate <- maybeToList mAction
        go =<< maybeToList mTarget
      go = \case
        ProxyTarget t _ -> go t
        LocationTarget lid -> [mkWhen $ Window.PassInvestigationSkillTest iid lid n]
        BothTarget t1 t2 -> go t1 <> go t2
        _ -> []
    pushM $ checkWindows $ mkWhen (Window.PassSkillTest mAction source iid n) : windows
    pure a
  After (PassedSkillTest iid mAction source (InvestigatorTarget iid') _ n) | iid == iid' && iid == toId a -> do
    mTarget <- getSkillTestTarget
    let
      windows = do
        Action.Investigate <- maybeToList mAction
        go =<< maybeToList mTarget
      go = \case
        ProxyTarget t _ -> go t
        LocationTarget lid -> [mkAfter $ Window.PassInvestigationSkillTest iid lid n]
        BothTarget t1 t2 -> go t1 <> go t2
        _ -> []
    pushM $ checkWindows $ mkAfter (Window.PassSkillTest mAction source iid n) : windows
    pure a
  PlayerWindow iid additionalActions isAdditional | iid == investigatorId -> do
    mTurnInvestigator <- maybeToList <$> selectOne TurnInvestigator
    let
      windows =
        map (mkWhen . Window.DuringTurn) mTurnInvestigator
          <> [mkWhen Window.FastPlayerWindow, mkWhen Window.NonFast]

    actions <- asIfTurn iid (getActions iid windows)
    anyForced <- anyM (isForcedAbility iid) actions
    if anyForced
      then do
        -- Silent forced abilities should trigger automatically
        let
          (isSilent, normal) = partition isSilentForcedAbility actions
          toForcedAbilities = map (flip (UseAbility iid) windows)
          toUseAbilities = map ((\f -> f windows [] []) . AbilityLabel iid)
        player <- getPlayer iid
        pushAll
          $ toForcedAbilities isSilent
          <> [chooseOne player (toUseAbilities normal) | notNull normal]
          <> [PlayerWindow iid additionalActions isAdditional]
      else do
        modifiers <- getModifiers (InvestigatorTarget iid)
        canAffordTakeResources <- getCanAfford a [#resource]
        canAffordDrawCards <- getCanAfford a [#draw]
        additionalActions' <- getAdditionalActions a
        let
          usesAction = not isAdditional
          drawCardsF = if usesAction then drawCardsAction else drawCards
          effectActions = flip mapMaybe additionalActions' $ \case
            AdditionalAction _ _ (EffectAction tooltip effectId) ->
              Just
                $ EffectActionButton
                  (Tooltip tooltip)
                  effectId
                  [UseEffectAction iid effectId windows]
            _ -> Nothing

        playableCards <- getPlayableCards iid iid (UnpaidCost NeedsAction) windows
        let drawing = drawCardsF iid a 1

        canDraw <- canDo iid #draw
        canTakeResource <- (&&) <$> canDo iid #resource <*> can.gain.resources FromOtherSource iid
        canPlay <- canDo iid #play
        player <- getPlayer iid

        push
          $ AskPlayer
          $ Ask player
          $ PlayerWindowChooseOne
          $ nub
          $ additionalActions
          <> [ ResourceLabel iid [TakeResources iid 1 (ResourceSource iid) usesAction]
             | canAffordTakeResources && canTakeResource
             ]
          <> [ ComponentLabel (InvestigatorDeckComponent iid) [drawing]
             | canAffordDrawCards
             , canDraw
             , none (`elem` modifiers) [CannotDrawCards, CannotManipulateDeck]
             ]
          <> [ targetLabel (toCardId c) [InitiatePlayCard iid c Nothing NoPayment windows usesAction]
             | canPlay
             , c <- playableCards
             ]
          <> [EndTurnButton iid [ChooseEndTurn iid]]
          <> map ((\f -> f windows [] []) . AbilityLabel iid) actions
          <> effectActions
    pure a
  PlayerWindow iid additionalActions isAdditional | iid /= investigatorId && a.inGame -> do
    let windows = [mkWhen Window.FastPlayerWindow]
    actions <- getActions investigatorId windows
    anyForced <- anyM (isForcedAbility investigatorId) actions
    unless anyForced $ do
      playableCards <- getPlayableCards investigatorId investigatorId (UnpaidCost NeedsAction) windows
      let
        usesAction = not isAdditional
        choices =
          additionalActions
            <> [ targetLabel
                   c
                   [ InitiatePlayCard investigatorId c Nothing NoPayment windows usesAction
                   , PlayerWindow iid additionalActions isAdditional
                   ]
               | c <- playableCards
               ]
            <> map
              ((\f -> f windows [] [PlayerWindow iid additionalActions isAdditional]) . AbilityLabel investigatorId)
              (filter (not . isActionAbility) actions)
      player <- getPlayer investigatorId
      unless (null choices) $ push $ AskPlayer $ Ask player $ PlayerWindowChooseOne choices
    pure a
  EndInvestigation -> do
    pure
      $ a
      & usedAbilitiesL
      %~ filter (\UsedAbility {..} -> abilityLimitType (abilityLimit usedAbility) /= Just PerPhase)
  EndEnemy -> do
    pure
      $ a
      & usedAbilitiesL
      %~ filter (\UsedAbility {..} -> abilityLimitType (abilityLimit usedAbility) /= Just PerPhase)
  ScenarioCountIncrementBy CurrentDepth n | n > 0 -> do
    pure
      $ a
      & usedAbilitiesL
      %~ filter (\UsedAbility {..} -> abilityLimitType (abilityLimit usedAbility) /= Just PerDepthLevel)
  EndUpkeep -> do
    pure
      $ a
      & usedAbilitiesL
      %~ filter (\UsedAbility {..} -> abilityLimitType (abilityLimit usedAbility) /= Just PerPhase)
  EndMythos -> do
    pure
      $ a
      & usedAbilitiesL
      %~ filter (\UsedAbility {..} -> abilityLimitType (abilityLimit usedAbility) /= Just PerPhase)
  EndRound -> do
    pure
      $ a
      & usedAbilitiesL
      %~ filter (\UsedAbility {..} -> abilityLimitType (abilityLimit usedAbility) /= Just PerRound)
      & (usedAdditionalActionsL .~ mempty)
  EndTurn iid | iid == toId a -> do
    pure
      $ a
      & (endedTurnL .~ True)
      & usedAbilitiesL
      %~ filter (\UsedAbility {..} -> abilityLimitType (abilityLimit usedAbility) /= Just PerTurn)
  UseCardAbility iid (isSource a -> True) 500 _ _ -> do
    otherInvestigators <- select $ colocatedWith a <> not_ (InvestigatorWithId investigatorId)
    case nonEmpty otherInvestigators of
      Nothing -> error "No other investigators"
      Just (x :| xs) -> do
        player <- getPlayer iid
        push
          $ chooseOrRunOne player
          $ [ targetLabel
                iid'
                [ chooseSome1
                    player
                    "Done giving keys"
                    [ Label ("Give " <> keyName k <> " key") [PlaceKey (toTarget iid') k]
                    | k <- toList investigatorKeys
                    ]
                ]
            | iid' <- x : xs
            ]
    pure a
  UseAbility _ ab _ | isSource a ab.source || isProxySource a ab.source -> do
    push $ Do msg
    pure a
  Do (UseAbility iid ability windows) | iid == investigatorId -> do
    activeInvestigator <- selectOne ActiveInvestigator
    mayIgnoreLocationEffectsAndKeywords <- hasModifier iid MayIgnoreLocationEffectsAndKeywords
    let
      mayIgnore =
        case abilitySource ability of
          LocationSource _ -> mayIgnoreLocationEffectsAndKeywords
          IndexedSource _ (LocationSource _) -> mayIgnoreLocationEffectsAndKeywords
          ProxySource (LocationSource _) _ -> mayIgnoreLocationEffectsAndKeywords
          _ -> False
      resolveAbility =
        [SetActiveInvestigator iid | x <- maybeToList activeInvestigator, iid /= x]
          <> [PayForAbility ability windows, MoveWithSkillTest (ResolvedAbility ability)]
          <> [SetActiveInvestigator x | x <- maybeToList activeInvestigator, iid /= x]
    player <- getPlayer iid

    if mayIgnore
      then push $ chooseOne player [Label "Ignore effect" [], Label "Do not ignore effect" resolveAbility]
      else pushAll resolveAbility
    case find ((== ability) . usedAbility) investigatorUsedAbilities of
      Nothing -> do
        depth <- getWindowDepth
        -- NOTE: if a used ability is missing it's traits for some reason, it's
        -- likely because it was discarded before this point and we don't know
        -- anymore (see: Spires of Carcosa)
        traits' <- sourceTraits $ abilitySource ability
        let
          used =
            UsedAbility
              { usedAbility = ability
              , usedAbilityInitiator = iid
              , usedAbilityWindows = windows
              , usedTimes = 1
              , usedDepth = depth
              , usedAbilityTraits = traits'
              , usedThisWindow = depth > 0
              }
        pure $ a & usedAbilitiesL %~ (used :)
      Just _ -> do
        let
          updateUsed used
            | usedAbility used == ability =
                used {usedTimes = usedTimes used + 1, usedAbilityWindows = usedAbilityWindows used <> windows}
            | otherwise = used
        pure $ a & usedAbilitiesL %~ map updateUsed
  DoNotCountUseTowardsAbilityLimit iid ability | iid == investigatorId -> do
    let
      updateUsed used
        | usedAbility used == ability = used {usedTimes = max 0 (usedTimes used - 1)}
        | otherwise = used
    pure $ a & usedAbilitiesL %~ map updateUsed
  SkillTestEnds {} -> do
    pure
      $ a
      & ( usedAbilitiesL
            %~ filter (\UsedAbility {..} -> abilityLimitType (abilityLimit usedAbility) /= Just PerTestOrAbility)
        )
      & (usedAbilitiesL %~ map (\u -> u {usedThisWindow = False}))
  PerformEnemyAttack eid -> do
    withMaybeField Field.EnemyAttacking eid \details -> do
      when (any (isTarget a) details.targets) do
        Lifted.updateHistory a $ HistoryItem HistoryEnemiesAttackedBy [eid]
    pure a
  After (PerformEnemyAttack {}) -> do
    pure $ a & (usedAbilitiesL %~ filter (\ab -> ab.limitType /= Just PerAttack))
  PickSupply iid s | iid == toId a -> pure $ a & suppliesL %~ (s :)
  UseSupply iid s | iid == toId a -> pure $ a & suppliesL %~ deleteFirst s
  Blanked msg' -> liftRunMessage msg' a
  SetInvestigatorForm iid form | iid == toId a -> pure $ a & formL .~ form
  RemovedLocation lid | investigatorLocation a == Just lid -> do
    -- needs to look at the "real" location not as if
    pure $ a & placementL .~ Unplaced
  PlaceInvestigator iid placement | iid == toId a -> do
    case placement of
      AtLocation lid -> do
        pushAll
          [ WhenWillEnterLocation iid lid
          , Do (WhenWillEnterLocation iid lid)
          , After (WhenWillEnterLocation iid lid)
          , EnterLocation iid lid
          ]
        pure a
      _ -> liftRunMessage (Do msg) a
  Do (PlaceInvestigator iid placement) | iid == toId a -> do
    when (placement == Unplaced) do
      enemies <- select $ enemyEngagedWith iid
      case investigatorLocation a of
        Just lid -> pushAll [PlaceEnemy enemy (AtLocation lid) | enemy <- enemies]
        Nothing -> pushAll [toDiscard GameSource (toTarget enemy) | enemy <- enemies]

    pure $ a & placementL .~ placement
  _ -> investigatorSettings `seq` pure a

investigatorLocation :: InvestigatorAttrs -> Maybe LocationId
investigatorLocation a = case a.placement of
  AtLocation lid -> Just lid
  _ -> Nothing

getFacingDefeat :: HasGame m => InvestigatorAttrs -> m Bool
getFacingDefeat a@InvestigatorAttrs {..} = do
  canOnlyBeDefeatedByDamage <- hasModifier a CanOnlyBeDefeatedByDamage
  modifiedHealth <- field InvestigatorHealth (toId a)
  modifiedSanity <- field InvestigatorSanity (toId a)
  pure
    $ or
      [ investigatorHealthDamage a + investigatorAssignedHealthDamage >= modifiedHealth
      , and
          [ investigatorSanityDamage a + investigatorAssignedSanityDamage >= modifiedSanity
          , not canOnlyBeDefeatedByDamage
          ]
      ]

takeUpkeepResources :: InvestigatorAttrs -> Runnable InvestigatorAttrs
takeUpkeepResources a = do
  fullModifiers <- getModifiers' a
  let mods = map modifierType fullModifiers

  let cannotGainResourcesFromPlayerCardEffects = CannotGainResourcesFromPlayerCardEffects `elem` mods

  let additionalAmount =
        sum
          [ n
          | Modifier s (UpkeepResources n) _ _ <- fullModifiers
          , not cannotGainResourcesFromPlayerCardEffects || sourceToFromSource s /= FromPlayerCardEffect
          ]
  let amount = 1 + additionalAmount
  if CannotGainResources `elem` mods
    then pure a
    else
      if MayChooseNotToTakeUpkeepResources `elem` mods
        then do
          player <- getPlayer (toId a)
          push
            $ chooseOne
              player
              [ Label "Do not take resource(s)" []
              , Label "Take resource(s)" [TakeResources (toId a) amount (ResourceSource $ toId a) False]
              ]
          pure a
        else
          pure $ a & tokensL %~ addTokens Resource amount
