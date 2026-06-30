{-# LANGUAGE TypeAbstractions #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Investigator.Runner (module Arkham.Investigator.Runner, module X) where

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
import Arkham.Customization
import Arkham.Deck qualified as Deck
import Arkham.DefeatedBy
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
  isForcedAbility,
 )
import Arkham.Helpers.Action (
  getActions,
 )
import Arkham.Helpers.Card (
  cardIsFast',
  getModifiedCardCost,
  passesLimits,
 )
import Arkham.Helpers.Cost (getCanAffordCost)
import Arkham.Helpers.Criteria (passesCriteria)
import Arkham.Helpers.Discover
import Arkham.Helpers.Game (withAlteredGame)
import Arkham.Helpers.Location (
  getCanMoveTo,
  isDiscoveringLastClue,
  withLocationOf,
 )
import Arkham.Helpers.Log (hasCampaignOption)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Playable (getPlayableCards)
import Arkham.Helpers.SkillTest
import Arkham.Helpers.Slot (
  canPutIntoSlot,
  emptySlot,
  removeIfMatches,
  removeIfMatchesOnce,
  slotItems,
 )
import Arkham.Helpers.Window (
  checkAfter,
  checkWhen,
  checkWindows,
  frame,
  pushBatch,
  pushBatched,
  windowMatches,
 )
import Arkham.Helpers.Window qualified as Helpers
import Arkham.History
import Arkham.I18n (cardNameVar, countVar, ikey', keyVar, withI18n)
import Arkham.Investigate.Types
import {-# SOURCE #-} Arkham.Investigator
import Arkham.Investigator.Runner.Action
import Arkham.Investigator.Runner.Card
import Arkham.Investigator.Runner.Damage
import Arkham.Investigator.Runner.Movement
import Arkham.Investigator.Runner.Search
import Arkham.Investigator.Types qualified as Attrs
import Arkham.Key
import Arkham.Keyword (Keyword (Starting))
import Arkham.Location.Types (Field (..))
import Arkham.Matcher (
  AssetMatcher (..),
  CardMatcher (..),
  EnemyMatcher (..),
  EventMatcher (..),
  ForPlay (..),
  InvestigatorMatcher (..),
  LocationMatcher (..),
  ScenarioMatcher (..),
  assetControlledBy,
  assetIs,
  at_,
  cardIs,
  coveredByAnyInPlayEnemy,
  locationWithInvestigator,
  oneOf,
  orConnected,
  pattern AnyInPlayEnemy,
  pattern AssetWithAnyClues,
 )
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted (takeControlOfAsset)
import Arkham.Message.Lifted qualified as Lifted
import Arkham.Message.Lifted.Choose qualified as Choose
import Arkham.Message.Lifted.Move (moveToEdit)
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
import Arkham.SkillTest
import Arkham.Slot
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Token qualified as Token
import Arkham.Tracing
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window (Window (..), mkAfter, mkWhen, mkWindow, primaryWindowTarget)
import Arkham.Window qualified as Window
import Arkham.Zone qualified as Zone
import Control.Lens (each, non, sumOf)
import Control.Monad.State.Strict (evalStateT, get, modify)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Monoid
import Data.Set qualified as Set

instance RunMessage Investigator where
  runMessage msg i@(Investigator (a :: original)) =
    do
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

-- Longest prefix admitting a system of distinct representatives: multi-type actions
-- (e.g. Fight+Activate from a bold-Fight play) contribute one chosen type per action.

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
getAllAbilitiesSkippable :: (Tracing m, HasGame m) => InvestigatorAttrs -> [Window] -> m Bool
getAllAbilitiesSkippable attrs windows = allM (getWindowSkippable attrs windows) windows

getWindowSkippable :: (Tracing m, HasGame m) => InvestigatorAttrs -> [Window] -> Window -> m Bool
getWindowSkippable
  _attrs
  ws
  ( windowTiming &&& windowType ->
      (Timing.When, Window.PlayCard iid (Window.CardPlay card@(PlayerCard pc) asAction))
    ) = do
    allModifiers <- getModifiers card
    mCost <- getModifiedCardCost iid card
    isFast <- cardIsFast' (\_ -> pure allModifiers) card
    needsFast <- Helpers.inFastWindow
    iids <- filter (/= iid) <$> getInvestigators
    iidsWithModifiers <- for iids \iid' -> (iid',) <$> getModifiers iid'
    modifiers <- getModifiers iid

    canHelpPay <-
      iidsWithModifiers & filterM \(_iid', modifiers') -> do
        modifiers' & anyM \case
          CanSpendResourcesOnCardFromInvestigator iMatcher cMatcher -> runValidT do
            guard $ cardMatch card cMatcher
            guard $ CannotAffectOtherPlayersWithPlayerEffectsExceptDamage `notElem` modifiers'
            liftGuardM $ iid <=~> iMatcher
          _ -> pure False

    resourcesFromAssets <-
      sum <$> for ((iid, modifiers) : iidsWithModifiers) \(iid', modifiers') -> do
        sum <$> for modifiers' \case
          CanSpendUsesAsResourceOnCardFromInvestigator assetId uType iMatcher cMatcher | cardMatch card cMatcher -> do
            let canAffect = iid == iid' || CannotAffectOtherPlayersWithPlayerEffectsExceptDamage `notElem` modifiers'
            canContribute <- (canAffect &&) <$> iid <=~> iMatcher
            if canContribute
              then fieldMap AssetUses (findWithDefault 0 uType) assetId
              else pure 0
          _ -> pure 0
    additionalResources <-
      (resourcesFromAssets +)
        . sum
        <$> traverse (field InvestigatorResources . fst) canHelpPay

    runValidT do
      when needsFast $ guard isFast
      cost <- hoistMaybe mCost

      liftGuardM
        $ withAlteredGame withoutCanModifiers
        $ getCanAffordCost iid pc [#play] ws (ResourceCost $ max 0 $ cost - additionalResources)
      when (not isFast && asAction) do
        liftGuardM $ getCanAffordCost iid pc [#play] ws (ActionCost 1)
      liftGuardM $ withAlteredGame withoutCanModifiers $ passesLimits iid card
getWindowSkippable
  attrs
  _ws
  ( windowTiming &&& windowType ->
      (Timing.When, Window.PlayEvent iid eid)
    ) | iid == toId attrs = do
    card <- field EventCard eid
    withAlteredGame withoutCanModifiers $ passesLimits iid card
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
  mCost <- getModifiedCardCost iid card
  let isFast = isJust $ cdFastWindow (toCardDef card) <|> listToMaybe [w | BecomesFast w <- allModifiers]
  andM
    [ case mCost of
        Nothing -> pure True
        Just cost ->
          withAlteredGame withoutCanModifiers
            $ getCanAffordCost (toId attrs) pc [#play] ws (ResourceCost cost)
    , if isFast
        then pure True
        else getCanAffordCost (toId attrs) pc [#play] ws (ActionCost 1)
    ]
getWindowSkippable _ _ _ = pure True

runWindow
  :: (HasGame m, Tracing m, HasQueue Message m)
  => InvestigatorAttrs -> [Window] -> [Ability] -> [Card] -> m ()
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
            $ [ targetLabel c [InitiatePlayCardWithWindows iid c Nothing NoPayment windows True]
              | c <- playableCards
              ]
            <> map
              ( \(ability, windows') ->
                  let ability' =
                        if abilityHighlightFromWindow ability && isNothing (abilityTarget ability)
                          then case listToMaybe windows' >>= primaryWindowTarget . windowType of
                            Just target -> withHighlight target ability
                            Nothing -> ability
                          else ability
                   in AbilityLabel iid ability' windows' [] []
              )
              actionsWithMatchingWindows
            <> [SkipTriggersButton iid | skippable]

runInvestigatorMessage :: Runner InvestigatorAttrs
runInvestigatorMessage msg a@InvestigatorAttrs {..} = runQueueT $ case msg of
  ClearAbilityUse ref -> do
    pure $ a & usedAbilitiesL %~ filter ((/= ref) . (.ref) . usedAbility)
  SealedChaosToken token miid (isTarget a -> True) -> do
    when (a.id `elem` miid) do
      Lifted.checkWhen (Window.ChaosTokenSealed a.id token)
      Lifted.checkAfter (Window.ChaosTokenSealed a.id token)
    pure $ a & sealedChaosTokensL %~ (token :)
  SealedChaosToken token miid _ -> do
    when (a.id `elem` miid) do
      Lifted.checkWhen (Window.ChaosTokenSealed a.id token)
      Lifted.checkAfter (Window.ChaosTokenSealed a.id token)
    pure $ a & sealedChaosTokensL %~ filter (/= token)
  UnsealChaosToken token -> pure $ a & sealedChaosTokensL %~ filter (/= token)
  ReturnChaosTokensToPool tokens -> pure $ a & sealedChaosTokensL %~ filter (`notElem` tokens)
  RemoveAllChaosTokens face -> do
    pure $ a & sealedChaosTokensL %~ filter ((/= face) . chaosTokenFace)
  UpdateGlobalSetting iid s | iid == a.id -> do
    let attrs' = a & settingsL %~ updateGlobalSetting s
    -- If a fast player window is open, re-evaluate so the choice list reflects
    -- the new setting immediately (e.g. enabling/disabling Skip Triggers should
    -- repopulate the current fast-window question without waiting for the next
    -- one). The new WindowAsk replaces any pending question for this player.
    currentWindows <- concat <$> getWindowStack
    when (any (\w -> Window.windowType w == Window.FastPlayerWindow) currentWindows) do
      push $ Do (CheckWindows currentWindows)
    pure attrs'
  UpdateCardSetting iid cCode s | iid == a.id -> do
    pure $ a & settingsL %~ updateCardSetting cCode s
  EndOfGame _ -> do
    -- Transfiguration (and Hank Samson's resolute flip) last "until the end
    -- of the game", so the form must revert before interludes check traits
    let resetForm = \case
          TransfiguredForm _ -> RegularForm
          form -> form
    pure $ a & placementL .~ Unplaced & formL %~ resetForm
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
  ForTarget (isTarget a -> True) (EndOfScenario {}) -> do
    pure $ a & handL .~ mempty & defeatedL .~ False & resignedL .~ False
  ForInvestigators _ ResetGame ->
    pure
      $ (cbCardBuilder (investigator id (toCardDef a) (getAttrStats a)) nullCardId investigatorPlayerId)
        { Attrs.investigatorId = investigatorId
        , investigatorXp = investigatorXp
        , investigatorSpentXp = investigatorSpentXp
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
    shuffled <- shuffle (unDeck investigatorDeck)
    -- A CannotPutIntoPlay modifier (e.g. a campaign-wide restriction) keeps a
    -- card that would otherwise start in play in the deck instead.
    setupModifiers <- getModifiers a
    let cannotPutIntoPlay c = any (\case CannotPutIntoPlay m -> cardMatch c m; _ -> False) setupModifiers
    (startsWithMsgs, deck') <-
      foldM
        ( \(msgs, currentDeck) cardDef -> do
            let (before, after) = break ((== cardDef) . toCardDef) (unDeck currentDeck)
            case after of
              (card : rest)
                | cannotPutIntoPlay (toCard card) -> pure (msgs, currentDeck)
                | otherwise ->
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
                cardDef' <-
                  if investigatorId == "05046" && cardDef.cardCode == "05108"
                    then
                      selectOne TheScenario <&> \case
                        Just "54016" -> Treacheries.fateOfAllFoolsUnspeakableFate
                        _ -> cardDef
                    else pure cardDef
                card <- setOwner investigatorId =<< genCard cardDef'
                push $ ReplaceCard card.id card
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
        ([], Deck shuffled)
        investigatorStartsWith
    let (permanentCards, deck'') =
          partition (\c -> cdPermanent (toCardDef c) && not (cannotPutIntoPlay (toCard c))) (unDeck deck')
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
  ReturnToHand iid (AssetTarget aid) | iid == investigatorId -> handleReturnToHand a iid aid
  ReturnToHand iid (EventTarget aid) | iid == investigatorId -> handleReturnToHandV2 a iid aid
  ReturnToHand iid (CardIdTarget cardId) | iid == investigatorId -> handleReturnToHandV3 a iid cardId
  ReturnToHand iid (CardMatcherTarget matcher) | iid == investigatorId -> handleReturnToHandV4 a iid matcher
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
    startingResources <- do
      if CannotGainResources `elem` mods
        then pure 0
        else getStartingResources iid
    let startingClues = getSum $ mconcat [Sum n | StartingClues n <- mods]
    pure $ a & tokensL %~ (setTokens Resource startingResources . setTokens Clue startingClues)
  InvestigatorMulligan iid | iid == investigatorId -> do
    unableToMulligan <- hasModifier a CannotMulligan
    hand <- field InvestigatorHand iid
    let mulliganableHand = filter (\c -> toCardId c `notElem` investigatorExcludeFromMulligan) hand
    if null mulliganableHand || unableToMulligan
      then push $ FinishedWithMulligan investigatorId
      else Choose.chooseOneM iid do
        Choose.labeledI "doneWithMulligan" $ push $ FinishedWithMulligan investigatorId
        for_ mulliganableHand \card ->
          when (cdCanReplace $ toCardDef card) do
            Choose.targeting card do
              push $ DiscardCard iid GameSource (toCardId card)
              push $ InvestigatorMulligan iid
    pure a
  BeginTrade iid _source (AssetTarget aid) iids | iid == investigatorId -> do
    Choose.chooseTargetM iid iids (`takeControlOfAsset` aid)
    pure a
  BeginTrade iid source (ResourceTarget _) iids | iid == investigatorId -> do
    pid <- getPlayer iid
    resources <- field InvestigatorResources iid
    Choose.chooseTargetM iid iids \iid' -> do
      resources' <- field InvestigatorResources iid'
      push $ Ask pid $ ChooseExchangeAmounts source iid resources iid' resources' #resource
    pure a
  PlaceSwarmCards iid eid n | iid == investigatorId && n > 0 -> do
    usePlaceholders <- hasCampaignOption UseSwarmPlaceholders
    if usePlaceholders
      then do
        hostCard <- field Field.EnemyCard eid
        replicateM_ n do
          card <- genCard hostCard
          push $ PlacedSwarmCard eid card
        pure a
      else do
        let cards = map toCard . take n $ unDeck investigatorDeck
        for_ cards $ push . PlacedSwarmCard eid
        pure $ a & (deckL %~ filter ((`notElem` cards) . PlayerCard))
  AllRandomDiscard source matcher | not (a ^. defeatedL || a ^. resignedL) -> do
    push $ toMessage $ randomDiscardMatching investigatorId source matcher
    pure a
  FinishedWithMulligan iid | iid == investigatorId -> do
    mods <- getModifiers a
    let allowedMulligans = max 0 . (1 +) $ sumOf (traverse . _Mulligans) mods
    let startingHandAmount = max 0 . (5 +) $ sumOf (traverse . _StartingHand) mods
    let additionalStartingCards = concat $ mapMaybe (preview _AdditionalStartingCards) mods
    -- investigatorHand is dangerous, but we want to use it here because we're
    -- only affecting cards actually in hand [I think]
    let excludedCount = length $ filter (\c -> toCardId c `elem` investigatorExcludeFromMulligan) investigatorHand
    (discard, hand, deck) <-
      if any (`elem` mods) [CannotDrawCards, CannotManipulateDeck]
        then pure (investigatorDiscard, investigatorHand, unDeck investigatorDeck)
        else drawOpeningHand a (startingHandAmount - (length investigatorHand - excludedCount))
    startsWithInHandCards <- traverse genCard investigatorStartsWithInHand
    for_ startsWithInHandCards \card -> push $ ReplaceCard card.id card
    let additionalHandCards = additionalStartingCards <> startsWithInHandCards

    -- need the virtual hand to get correct length
    hand' <- field InvestigatorHand iid
    let moreMulligans = (a ^. mulligansTakenL + 1) < allowedMulligans && startingHandAmount - length hand' > 0

    -- Starting keyword: offer search before mulligan cards are shuffled back in
    unless moreMulligans do
      let handIds :: Set CardId = setFromList $ map toCardId (hand <> additionalHandCards)
      let startingCardsInDeck =
            filter
              (\card -> Starting `member` cdKeywords (toCardDef card) && toCardId card `notElem` handIds)
              deck
      unless (null startingCardsInDeck) do
        Lifted.focusCards startingCardsInDeck do
          Choose.chooseOneM iid do
            Choose.labeledI "doNotAddStartingCard" Choose.nothing
            for_ startingCardsInDeck \card ->
              Choose.targeting card do
                Lifted.drawToHand iid [PlayerCard card]

    Lifted.shuffleDiscardBackIn iid
    Lifted.checkAfter (Window.DrawingStartingHand iid)

    when moreMulligans do
      push $ InvestigatorMulligan iid

    pure
      $ a
      & (discardL .~ discard)
      & (handL .~ hand <> additionalHandCards)
      & (deckL .~ Deck deck)
      & (mulligansTakenL +~ 1)
  ForInvestigator iid BeginGame | iid == investigatorId -> do
    -- if we have any cards with revelations on them, we need to trigger them
    let
      choices = mapMaybe cardChoice investigatorHand
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
  ShuffleDeck (Deck.InvestigatorDeck iid) | iid == investigatorId -> handleShuffleDeck a iid
  ShuffleDiscardBackIn iid | iid == investigatorId -> handleShuffleDiscardBackIn a iid
  Resign iid | iid == investigatorId -> do
    -- "When you resign" abilities must fire before the investigator is marked
    -- resigned; otherwise the window is skipped for that investigator and
    -- forced abilities on cards they control (e.g. Relics of the Past artifacts)
    -- never get a chance to trigger.
    Lifted.checkWhen $ Window.InvestigatorResigned iid
    pushAll $ resolve (Msg.InvestigatorResigned iid)
    pure $ a & endedTurnL .~ True
  Msg.InvestigatorDefeated source iid | iid == investigatorId -> do
    whenM (withoutModifier a CannotBeDefeated) do
      -- a card effect defeats an investigator directly
      windowMsg <-
        checkWindows [mkWhen $ Window.InvestigatorWouldBeDefeated (DefeatedByOther source) (toId a)]
      pushAll [windowMsg, InvestigatorIsDefeated source iid]
    pure a
  InvestigatorIsDefeated source iid | iid == investigatorId -> handleInvestigatorIsDefeated a source iid
  Msg.InvestigatorResigned iid | iid == investigatorId -> do
    pushAll [InvestigatorWhenEliminated (toSource a) iid (Just $ Do msg)]
    pure $ a & endedTurnL .~ True
  Do (Msg.InvestigatorResigned iid) | iid == investigatorId -> do
    isLead <- (== iid) <$> getLead
    pushWhen isLead ChooseLeadInvestigator
    pure $ a & resignedL .~ True
  -- InvestigatorWhenEliminated is handled by the scenario
  InvestigatorEliminated iid | iid == investigatorId -> handleInvestigatorEliminated a iid
  PerformAction iid source action | iid == investigatorId -> handlePerformAction a iid source action
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
  ChooseAndDiscardAsset iid source assetMatcher | iid == investigatorId -> handleChooseAndDiscardAsset a iid source assetMatcher
  AttachAsset aid _ -> pure $ a & (slotsL %~ removeFromSlots aid)
  PlaceAsset aid placement -> do
    case placement of
      InPlayArea iid | iid == investigatorId -> do
        push $ InvestigatorPlayAsset iid aid
        pure a
      InThreatArea iid | iid == investigatorId -> do
        push $ InvestigatorPlayAsset iid aid
        pure a
      AttachedToAsset _ (Just (InPlayArea iid)) | iid == investigatorId -> do
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
    excess <- getExcessInHandCount a
    pushWhen (excess > 0) $ CheckHandSize investigatorId
    pure a
  CheckHandSize iid | iid == investigatorId -> do
    excess <- getExcessInHandCount a
    when (excess > 0) $ do
      handSize <- getHandSize a
      send $ format a <> " must discard down to " <> tshow handSize <> " cards"
      pushAll [SetActiveInvestigator iid, Do msg]
    pure a
  Do (CheckHandSize iid) | iid == investigatorId -> do
    excess <- getExcessInHandCount a
    -- investigatorHand: can only discard cards actually in hand
    player <- getPlayer iid
    let viable = filter (isNothing . cdCardSubType . toCardDef) $ onlyPlayerCards investigatorHand

    pushWhen (excess > 0 && notNull viable)
      $ chooseOne player
      $ [ targetLabel (toCardId card) [DiscardCard iid GameSource (toCardId card), Do (CheckHandSize iid)]
        | card <- viable
        ]
    pure a
  AddToDiscard iid pc | investigatorOwnsCardCode a iid -> do
    modifiers' <- getModifiers a
    case [target | PlaceUnderneathInsteadOfDiscard target <- modifiers'] of
      (target : _) -> do
        pushAll [ObtainCard (toCard pc).id, PlaceUnderneath target [toCard pc]]
        pure a
      [] -> handleAddToDiscard a iid pc
  DiscardFromHand handDiscard | handDiscard.investigator == investigatorId -> handleDiscardFromHand a handDiscard msg
  Do (DiscardFromHand handDiscard) | handDiscard.investigator == investigatorId -> handleDoDiscardFromHand a handDiscard
  Discard _ source (CardIdTarget cardId) | isJust (find ((== cardId) . toCardId) investigatorHand) -> handleDiscard a source cardId
  Discard _ _ (SearchedCardTarget cardId) -> handleDiscardV2 a cardId
  DiscardHand iid source | iid == investigatorId -> do
    liftRunMessage (DiscardFromHand $ discardAll iid source AnyCard) a
  DiscardCard iid source cardId | iid == investigatorId -> handleDiscardCard a iid source cardId msg
  Do (DiscardCard iid _source cardId) | iid == investigatorId -> handleDoDiscardCard a iid cardId
  DoneDiscarding iid | iid == investigatorId -> handleDoneDiscarding a iid
  RemoveCardFromHand iid cardId | iid == investigatorId -> handleRemoveCardFromHand a iid cardId
  RemoveCardFromSearch iid cardId | iid == investigatorId -> handleRemoveCardFromSearch a iid cardId
  ShuffleIntoDeck (Deck.InvestigatorDeck iid) (AssetTarget aid) | iid == investigatorId -> handleShuffleIntoDeck a iid aid msg
  ShuffleIntoDeck (Deck.InvestigatorDeck iid) (EventTarget eid) | iid == investigatorId -> handleShuffleIntoDeckV2 a iid eid msg
  ShuffleIntoDeck (Deck.InvestigatorDeck iid) (SkillTarget aid) | iid == investigatorId -> handleShuffleIntoDeckV3 a iid aid msg
  ShuffleIntoDeck (Deck.InvestigatorDeck iid) (CardIdTarget cid) | iid == investigatorId -> handleShuffleIntoDeckV4 a iid cid
  PutOnTopOfDeck _ (Deck.InvestigatorDeck iid) (CardIdTarget cid) | iid == investigatorId -> handlePutOnTopOfDeck a iid cid
  PutOnBottomOfDeck _ (Deck.InvestigatorDeck iid) (CardIdTarget cid) | iid == investigatorId -> handlePutOnBottomOfDeck a iid cid
  Discarded (AssetTarget aid) _ (PlayerCard card) -> handleDiscarded a aid card
  RemoveAsset aid -> do
    pushWhen (providedSlot a aid) $ RefillSlots a.id []
    pure $ a & (slotsL %~ removeFromSlots aid)
  Discarded (AssetTarget aid) _ (EncounterCard _) -> handleDiscardedV2 a aid
  Discarded (EventTarget aid) _ _ -> handleDiscardedV3 a aid
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
    smods <- filter (== IgnoreAloof) <$> getModifiers choose.skillTest
    enemyIds <-
      withAlteredGame withoutCanModifiers
        $ asIfTurn investigatorId
        $ select
        $ foldr
          applyMatcherModifiers
          (canFightMatcher <> enemyMatcher <> mustChooseMatchers)
          (modifiers <> smods)

    canMoveToConnected <- case source.asset of
      Just aid -> aid <=~> AssetWithCustomization InscriptionOfTheHunt
      _ -> pure False
    -- Targets that are merely attackable "as if an enemy" (Mist-Pylons, Key Loci) are not
    -- real enemies. Only offer them when the fight is unrestricted; a fight narrowed by
    -- the card's matcher (e.g. Toe to Toe's @EnemyCanAttack You@) must not include them.
    let includeAsIfEnemy = coveredByAnyInPlayEnemy enemyMatcher
    locationIds <-
      if includeAsIfEnemy
        then
          withAlteredGame withoutCanModifiers
            $ asIfTurn investigatorId
            $ select
            $ LocationWithModifier CanBeAttackedAsIfEnemy
            <> if canMoveToConnected
              then orConnected ForMovement (locationWithInvestigator investigatorId)
              else locationWithInvestigator investigatorId
        else pure []
    concealed <- if includeAsIfEnemy then getConcealedIds NotForExpose investigatorId else pure []
    assetIds <-
      if includeAsIfEnemy
        then
          withAlteredGame withoutCanModifiers
            $ asIfTurn investigatorId
            $ select
            $ AssetWithModifier CanBeAttackedAsIfEnemy
            <> at_ (locationWithInvestigator investigatorId)
        else pure []
    player <- getPlayer investigatorId
    let choices = enemyIds <> map coerce locationIds <> map coerce concealed <> map coerce assetIds
    -- we might have killed the enemy via a reaction before getting here
    let flabel eid = if choose.skillType /= #combat then FightLabelWithSkill eid choose.skillType else FightLabel eid
    unless (null choices) do
      push
        $ chooseOne
          player
          [ flabel
              eid
              $ ChoseEnemy choose.skillTest investigatorId source eid
              : [ FightEnemy eid choose
                | not choose.onlyChoose
                ]
          | eid <- choices
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
      <> [ Will (CheckAttackOfOpportunity iid False Nothing)
         | ActionDoesNotCauseAttacksOfOpportunity #engage `notElem` modifiers'
         ]
      <> [ CheckAttackOfOpportunity iid False Nothing
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
        $ [ BeginAction
          , beforeWindowMsg
          ]
        <> [ TakeActions iid [#fight] (foldl' applyFightCostModifiers (ActionCost 1) modifiers')
           | choose.payCost
           ]
        <> [ FightEnemy eid choose {chooseFightIsAction = False}
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
  PlaceAdditionalDamage target source damage horror | isTarget a target -> handlePlaceAdditionalDamage a target source damage horror
  InvestigatorDamageInvestigator iid xid | iid == investigatorId -> handleInvestigatorDamageInvestigator a iid xid
  InvestigatorDamageEnemy iid eid source | iid == investigatorId -> handleInvestigatorDamageEnemy a iid eid source
  ChooseEvadeEnemy choose | choose.investigator == investigatorId -> do
    modifiers <- getModifiers a
    let source = choose.source
    let mTarget = choose.target
    let skillType = choose.skillType
    let enemyMatcher = choose.matcher
    let isAction = choose.isAction
    let payCost = choose.payCost
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
    concealed <- getConcealedIds NotForExpose investigatorId
    let choices = enemyIds <> map coerce concealed
    let elabel eid = if skillType /= #agility then EvadeLabelWithSkill eid skillType else EvadeLabel eid
    unless (null choices) do
      if isAction && not payCost
        then do
          let iid = investigatorId
          beforeWindowMsg <- checkWindows [mkWhen $ Window.PerformAction iid #evade]
          afterWindowMsg <- checkWindows [mkAfter $ Window.PerformAction iid #evade]
          pushAll
            [ BeginAction
            , beforeWindowMsg
            , chooseOne player
                $ choose.additionalOptions
                <> [ elabel
                       eid
                       [ ChosenEvadeEnemy choose.skillTest source eid
                       , EvadeEnemy choose.skillTest a.id eid source mTarget skillType False
                       ]
                   | eid <- choices
                   ]
            , afterWindowMsg
            , FinishAction
            , TakenActions iid [#evade]
            ]
        else
          push
            $ chooseOne player
            $ choose.additionalOptions
            <> [ elabel
                   eid
                   [ ChosenEvadeEnemy choose.skillTest source eid
                   , EvadeEnemy choose.skillTest a.id eid source mTarget skillType isAction
                   ]
               | eid <- choices
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
  MoveAction iid lid cost True | iid == investigatorId -> handleMoveAction a iid lid cost
  MoveAction iid lid _cost False | iid == investigatorId -> handleMoveActionV2 a iid lid
  Move movement | isTarget a (moveTarget movement) -> handleMove a movement
  WhenCanMove iid msgs | iid == investigatorId -> handleWhenCanMove a iid msgs
  Will (PassedSkillTest iid _ _ (InvestigatorTarget iid') _ n) | iid == iid' && iid == investigatorId -> do
    pushM $ checkWindows [mkWhen (Window.WouldPassSkillTest iid n)]
    pure a
  Will (FailedSkillTest iid _ _ (InvestigatorTarget iid') _ n) | iid == iid' && iid == toId a -> do
    pushM $ checkWindows [mkWhen (Window.WouldFailSkillTest iid n)]
    pure a
  CancelDamage iid n | iid == investigatorId -> handleCancelDamage a iid n
  CancelHorror iid n | iid == investigatorId -> handleCancelHorror a iid n
  InvestigatorDirectDamage iid source damage horror | iid == toId a -> handleInvestigatorDirectDamage a iid source damage horror
  InvestigatorAssignDamage iid source strategy damage horror | iid == toId a -> handleInvestigatorAssignDamage a iid source strategy damage horror
  InvestigatorDoAssignDamage iid source damageStrategy _ 0 0 damageTargets horrorTargets
    | iid == toId a
    , isDeferredStrategy damageStrategy ->
        finalizeDeferredDamageAssignment a iid source damageStrategy damageTargets horrorTargets
  InvestigatorDoAssignDamage iid source damageStrategy _ 0 0 damageTargets horrorTargets
    | iid == toId a ->
        finalizeDamageAssignment a iid source damageStrategy damageTargets horrorTargets
  InvestigatorDoAssignDamage iid source DamageEvenly matcher health 0 damageTargets horrorTargets
    | iid == toId a ->
        assignHealthDamageEvenly a iid source matcher health damageTargets horrorTargets
  InvestigatorDoAssignDamage iid source DamageEvenly matcher 0 sanity damageTargets horrorTargets
    | iid == toId a ->
        assignHorrorEvenly a iid source matcher sanity damageTargets horrorTargets
  InvestigatorDoAssignDamage iid _ DamageEvenly _ _ _ _ _ | iid == investigatorId -> assignDamageEvenlyUnsupported a iid
  InvestigatorDoAssignDamage iid source SingleTarget matcher health sanity damageTargets horrorTargets
    | iid == toId a ->
        assignDamageToSingleTarget a iid source matcher health sanity damageTargets horrorTargets
  InvestigatorDoAssignDamage iid source strategy matcher health sanity damageTargets horrorTargets
    | iid == toId a ->
        assignDamageDivided
          a
          iid
          source
          strategy
          matcher
          health
          sanity
          damageTargets
          horrorTargets
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
          ]
        <> [ TakeActions investigatorId [#investigate] (ActionCost investigateCost)
           | investigation.payCost
           ]
        <> [ Will (CheckAttackOfOpportunity investigatorId False Nothing)
           | investigation.payCost
           , ActionDoesNotCauseAttacksOfOpportunity #investigate `notElem` modifiers'
           ]
        <> [ CheckAttackOfOpportunity investigatorId False Nothing
           | investigation.payCost
           , ActionDoesNotCauseAttacksOfOpportunity #investigate `notElem` modifiers'
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
  FlipDoom target n | isTarget a target -> do
    pure $ a & tokensL %~ flipDoom n
  DiscoverClues iid d | iid == investigatorId && d.location == DiscoverYourLocation -> do
    lid <- fromJustNote "missing location" <$> getDiscoverLocation iid d
    push $ DiscoverClues iid (d {discoverLocation = DiscoverAtLocation lid})
    pure a
  DoStep 1 (DiscoverClues iid d) | iid == investigatorId -> do
    mods <- getModifiers iid
    dmods <- getModifiers $ DiscoverTarget d.id
    let additionalDiscoveredAt = Map.fromListWith (<>) [(olid, Sum x) | DiscoveredCluesAt olid x <- mods]
    let additionalDiscovered =
          getSum
            ( fold $ [Sum x | d.isInvestigate == IsInvestigate, DiscoveredClues x <- mods]
                <> [Sum x | DiscoveredClues x <- dmods]
            )

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
          push
            $ Do
            $ DiscoverClues iid
            $ d {discoverLocation = DiscoverAtLocation lid', discoverCount = discoveredClues'}
    pure a
  Do (DiscoverClues iid d) | iid == investigatorId -> do
    lid <- fromJustNote "missing location" <$> getDiscoverLocation iid d
    canDiscoverClues <- getCanDiscoverClues d.isInvestigate iid lid
    if canDiscoverClues
      then do
        locationClues <- field LocationClues lid
        lastClue <- isDiscoveringLastClue lid d.count
        let clueCount = min locationClues d.count
        locationWindowsBefore <-
          checkWindows $ mkWhen (Window.DiscoverClues iid lid d.source clueCount)
            : [mkWhen (Window.DiscoveringLastClue iid lid) | lastClue]
        locationWindowsAfter <-
          checkWindows $ mkAfter (Window.DiscoverClues iid lid d.source clueCount)
            : mkAfter (Window.GainsClues iid d.source clueCount)
            : [mkAfter (Window.DiscoveringLastClue iid lid) | lastClue]

        concealed <- getConcealedAt (ForExpose $ toSource iid) lid
        settings <- getSettings
        let
          strictAsIfAt = settingsStrictAsIfAt settings
          wrapWindows msgs
            | strictAsIfAt = [SetAsIfAtIgnored iid True] <> msgs <> [SetAsIfAtIgnored iid False]
            | otherwise = msgs

        let
          defaultDiscover :: Lifted.ReverseQueue n => n ()
          defaultDiscover =
            pushAll
              $ [ MoveTokens d.source (toSource lid) (toTarget iid) Clue clueCount
                ]
              <> wrapWindows [locationWindowsBefore]
              <> [ UpdateHistory iid (HistoryItem HistoryCluesDiscovered $ singletonMap lid clueCount)
                 , After $ GainClues iid d.source clueCount
                 ]
              <> wrapWindows [locationWindowsAfter]
              <> d.discoverThen

        if
          | notNull concealed && clueCount > 0 ->
              Choose.chooseOneM iid do
                Choose.labeledI "exposeConcealedCard" $ chooseExposeConcealedAt iid iid (LocationWithId lid)
                Choose.labeledI "discoverNormally" defaultDiscover
          | notNull concealed -> chooseExposeConcealedAt iid iid (LocationWithId lid)
          | otherwise -> defaultDiscover

        send $ format a <> " discovered " <> pluralize clueCount "clue"
        pure a
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
      <> [InitiatePlayCardWithWindows iid (PlayerCard choiceAsCard) Nothing payment windows' asAction]
    pure $ a & handL %~ (PlayerCard choiceAsCard :) . filter (/= card)
  InitiatePlayCard iid card mtarget payment windows' asAction | iid == investigatorId -> do
    -- we need to check if the card is first an AsIfInHand card, if it is, then we let the owning entity handle this message
    modifiers' <- getModifiers (toTarget a)
    let
      shouldSkip = flip any modifiers' $ \case
        AsIfInHand card' -> card == card'
        _ -> False
    unless shouldSkip $ do
      push $ PlayCard iid card mtarget payment windows' asAction
    pure a
  InitiatePlayCardWithWindows iid card mtarget payment windows' asAction | iid == investigatorId -> do
    -- we need to check if the card is first an AsIfInHand card, if it is, then we let the owning entity handle this message
    modifiers' <- getModifiers (toTarget a)
    let
      shouldSkip = flip any modifiers' $ \case
        AsIfInHand card' -> card == card'
        _ -> False
      shouldAddToHand = flip any modifiers' $ \case
        AsIfInHandFor ForPlay cardId -> card.id == cardId
        _ -> False
    unless shouldSkip $ do
      afterPlayCard <- checkWindows [mkAfter (Window.PlayCard iid $ Window.CardPlay card asAction)]
      when shouldAddToHand do
        Lifted.cardResolutionModifier card GameSource iid (AsIfInHandFor NotForPlay card.id)
      if cdSkipPlayWindows (toCardDef card)
        then push $ PlayCard iid card mtarget payment windows' asAction
        else
          pushAll
            [ CheckWindows [mkWhen (Window.PlayCard iid $ Window.CardPlay card asAction)]
            , InitiatePlayCard iid card mtarget payment windows' asAction
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
  InitDeck iid murl _ | iid == investigatorId -> handleInitDeck a iid murl
  UpgradeDeck iid murl _ | iid == investigatorId -> handleUpgradeDeck a iid murl
  ObtainCard cardId -> handleObtainCard a cardId
  ReplaceCard cardId card -> handleReplaceCard a cardId card
  PutCampaignCardIntoPlay iid cardDef | iid == investigatorId -> handlePutCampaignCardIntoPlay a iid cardDef
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
                [ pure $ sType /= sType'
                , pure $ sType `elem` adjustableSlotsFor slot
                , canPutIntoSlot assetCard (emptySlot slot)
                ]
          )
          $ concatMap (\(t, bs) -> (t,) <$> bs)
          $ mapToList (a ^. slotsL)
    choices <- concatForM slots $ \sType -> do
      slots' <- adjustableSlots sType
      for slots' \(sType', slot) -> do
        -- Name a filled slot by the card occupying it (so two otherwise-identical
        -- adjustable slots are distinguishable); when empty, name it by its
        -- current slot type ("empty Hand slot") rather than the card that grants
        -- it. `sType` (destination) and `sType'` (current) are localized via @:{}.
        label <- case slotItems slot of
          (occupantAid : _) -> do
            occCard <- field AssetCard occupantAid
            pure
              $ withI18n
              $ cardNameVar occCard
              $ keyVar "slot" (slotKey sType)
              $ ikey' "label.changeSlot"
          [] ->
            pure
              $ withI18n
              $ keyVar "fromSlot" (slotKey sType')
              $ keyVar "slot" (slotKey sType)
              $ ikey' "label.changeEmptySlot"
        pure $ Label label [InvestigatorAdjustSlot iid slot sType' sType]

    when (notNull choices) do
      player <- getPlayer iid
      push $ chooseSome player "Do not change slots" choices

    pure a
  InvestigatorAdjustSlot iid slot fromSlotType toSlotType | iid == investigatorId -> do
    push $ RefillSlots iid []
    -- N.B. Only remove the single slot being adjusted. The `Eq Slot` instance
    -- treats every AdjustableSlot as equal, so `filter (/= slot)` would wipe out
    -- \*all* adjustable slots of this type (e.g. both of two Hidden Pockets),
    -- silently dropping a slot and forcing an asset to be discarded. We match on
    -- exact source equality rather than `isSlotSource`, because two Hidden Pockets
    -- attached to the same asset share an AssetSource (so the fuzzy `isSlotSource`
    -- match could move the wrong slot); their full BothSource differs by event.
    pure
      $ a
      & (slotsL %~ ix fromSlotType %~ deleteFirstMatch ((== slotSource slot) . slotSource))
      & (slotsL %~ at toSlotType . non [] %~ (emptySlot slot :))
  InvestigatorClearUnusedAssetSlots iid xs | iid == investigatorId -> do
    updatedSlots <- for (mapToList investigatorSlots) \(slotType, slots) -> do
      slots' <- flip evalStateT [] do
        for slots \slot -> do
          case slotItems slot of
            [] -> pure slot
            assets -> do
              ignored <-
                assets & filterM \aid -> do
                  cardId <- field AssetCardId aid
                  orM
                    [ fieldMap AssetSlots (notElem slotType) aid
                    , hasAnyModifier aid [DoNotTakeUpSlots, DoNotTakeUpSlot slotType]
                    , hasAnyModifier cardId [DoNotTakeUpSlots, DoNotTakeUpSlot slotType]
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
        canHoldMap :: Map SlotType [SlotType] <- do
          mods <- getModifiers a
          let
            canHold = \case
              SlotCanBe slotType canBeSlotType -> insertWith (<>) slotType [canBeSlotType]
              _ -> id
          pure $ foldr canHold mempty mods
        let additionalSlots = concatMap (\k -> findWithDefault [] k canHoldMap) missingSlotTypes
        assetsThatCanProvideSlots <-
          select
            $ assetControlledBy iid
            <> DiscardableAsset
            <> AssetOneOf (map AssetInSlot (nub $ missingSlotTypes <> additionalSlots))
            <> not_ (AssetWithId aid)

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
  RemoveCampaignCardFromDeck iid cardDef | iid == investigatorId -> do
    pure
      $ a
      & (deckL %~ Deck . filter ((/= toCardCode cardDef) . toCardCode) . unDeck)
  RemoveAllCopiesOfCardFromGame iid cardCode | iid == investigatorId -> handleRemoveAllCopiesOfCardFromGame a iid cardCode
  PutCardIntoPlay _ card _ _ _ -> handlePutCardIntoPlay a card
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
          | otherwise -> Choose.chooseOneM iid $ withI18n $ countVar 1 do
              Choose.labeled' "cancelDamage"
                $ push
                $ DoStep 1
                $ Msg.InvestigatorDamage iid source (damage' - 1) horror'
              Choose.labeled' "cancelHorror"
                $ push
                $ DoStep 1
                $ Msg.InvestigatorDamage iid source damage' (horror' - 1)
      else push $ DoStep 1 $ Msg.InvestigatorDamage iid source damage' horror'
    pure a
  DoStep 1 (Msg.InvestigatorDamage iid _ damage horror) | iid == investigatorId -> do
    pure $ a & assignedHealthDamageL +~ max 0 damage & assignedSanityDamageL +~ max 0 horror
  DrivenInsane iid | iid == investigatorId -> handleDrivenInsane a iid
  CheckDefeated source (isTarget a -> True) | not (a ^. defeatedL || a ^. resignedL) -> handleCheckDefeated a source
  AssignDamage target | isTarget a target -> handleAssignDamage a target
  CancelAssignedDamage target damageReduction horrorReduction | isTarget a target -> handleCancelAssignedDamage a target damageReduction horrorReduction
  ApplyHealing source -> handleApplyHealing a source msg
  Do (ApplyHealing source) -> handleDoApplyHealing a source
  HealDamage (InvestigatorTarget iid) source amount' | iid == investigatorId -> handleHealDamage a iid source amount' msg
  Do (HealDamage (InvestigatorTarget iid) source amount) | iid == investigatorId -> handleDoHealDamage a iid source amount
  HealDamageDelayed (isTarget a -> True) source n -> handleHealDamageDelayed a source n
  HealHorrorWithAdditional (InvestigatorTarget iid) _source amount | iid == investigatorId -> handleHealHorrorWithAdditional a iid amount
  AdditionalHealHorror (InvestigatorTarget iid) source additional | iid == investigatorId -> handleAdditionalHealHorror a iid source additional
  HealHorror (InvestigatorTarget iid) source amount' | iid == investigatorId -> handleHealHorror a iid source amount'
  HealHorrorDelayed target@(isTarget a -> True) source n | n > 0 -> handleHealHorrorDelayed a target source n msg
  Do (HealHorrorDelayed (isTarget a -> True) source n) -> handleDoHealHorrorDelayed a source n
  Do (HealHorror (isTarget a -> True) source n) -> handleDoHealHorror a source n
  MoveTokens s _ (isTarget a -> True) tType amount -> liftRunMessage (PlaceTokens s (toTarget a) tType amount) a
  MoveTokens s (isSource a -> True) _target tType amount | amount > 0 -> do
    case tType of
      Clue -> do
        push $ ForInvestigator investigatorId msg
        pure a
      _ -> liftRunMessage (RemoveTokens s (toTarget a) tType amount) a
  ForInvestigator iid (MoveTokens s source@(isSource a -> True) target Clue amount) | amount > 0 && iid == investigatorId -> do
    includeStory <- not <$> hasCampaignOption PlayersDoNotControlStoryAssetClues
    let storyWrapper = if includeStory then id else (<> AssetNonStory)
    let
      wrapper = case target.asset of
        Just aid -> (<> not_ (AssetWithId aid))
        Nothing -> id
    assetsWithClues <-
      selectWithField AssetClues $ storyWrapper $ wrapper $ assetControlledBy a.id <> AssetWithAnyClues
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
  ReassignHorror (isSource a -> True) _ n -> handleReassignHorror a n
  ReassignDamage (isSource a -> True) _ n -> handleReassignDamage a n
  HealHorrorDirectly (InvestigatorTarget iid) source amount | iid == investigatorId -> handleHealHorrorDirectly a iid source amount
  HealDamageDirectly (InvestigatorTarget iid) source amount | iid == investigatorId && amount > 0 -> handleHealDamageDirectly a iid source amount
  InvestigatorWhenDefeated source iid | iid == investigatorId -> handleInvestigatorWhenDefeated a source iid
  InvestigatorKilled source iid | iid == investigatorId -> handleInvestigatorKilled a source iid
  MoveAllTo source lid | not (a ^. defeatedL || a ^. resignedL) -> handleMoveAllTo a source lid
  MoveToward target locationMatcher | isTarget a target -> handleMoveToward a target locationMatcher
  MoveUntil lid target | isTarget a target -> handleMoveUntil a lid target
  MoveTo movement | isTarget a (moveTarget movement) -> handleMoveTo a movement
  SetMovement iid movement | iid == investigatorId -> handleSetMovement a iid movement
  EnemySpawned details -> do
    pure
      $ a
      & usedAbilitiesL
      %~ filter \ab -> ab.limitType /= Just PerSpawn || maybe True (not . isTarget details.enemy) ab.target
  ResolvedMovement iid movementId | iid == investigatorId -> handleResolvedMovement a iid movementId
  ResolveMovement iid | iid == investigatorId -> handleResolveMovement a iid msg
  Do (ResolveMovement iid) | iid == investigatorId -> handleDoResolveMovement a iid
  ForInvestigator iid' (ForTarget (LocationTarget lid) (MoveTo movement)) | isTarget a (moveTarget movement) -> do
    whenM (getCanMoveTo iid' (moveSource movement) lid) do
      Choose.chooseOneM iid' do
        Choose.labeledI "moveToo" $ moveToEdit iid' iid' lid (\m -> m {moveSkipEngagement = True})
        Choose.labeledI "skip" Choose.nothing

    pure a
  Do (WhenWillEnterLocation iid lid) | iid == investigatorId -> handleDoWhenWillEnterLocation a iid lid
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
    assetIds <-
      select
        $ oneOf [AssetInPlayAreaOf (InvestigatorWithId iid), AssetInThreatAreaOf (InvestigatorWithId iid)]
    mods <- getModifiers a
    requirements <- concatForM assetIds \assetId -> do
      assetCard <- field AssetCard assetId
      amods <- getCombinedModifiers [toTarget assetId, toTarget assetCard]
      let slotFilter sType = all (`notElem` amods) [DoNotTakeUpSlot sType, DoNotTakeUpSlots]
      slots <- field AssetSlots assetId
      pure $ (assetId,assetCard,) <$> filter slotFilter slots

    let slotsToRemove = concat [List.replicate n s | FewerSlots s n <- mods]
    let allSlots' :: [(SlotType, Slot)] = concatMap (\(k, vs) -> (k,) . emptySlot <$> vs) $ Map.assocs (a ^. slotsL)
    let allSlots = foldr (\s -> deleteFirstMatch ((== s) . fst)) allSlots' slotsToRemove

    canHoldMap :: Map SlotType [SlotType] <- do
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
              (availableSlots2, unused2) <- partitionM (canPutIntoSlot card) (lookupSlot other slots)
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
  ChooseEndTurn iid | iid == investigatorId -> do
    if view endedTurnL a
      then Lifted.do_ msg
      else do
        Lifted.batched \_ -> do
          Lifted.checkWhen $ Window.WouldEndTurn iid
          Lifted.do_ msg
    pure a
  Do (ChooseEndTurn iid) | iid == investigatorId -> do
    msgs <- resolveWithWindow (EndTurn iid) (Window.TurnEnds iid)
    pushAll msgs
    pure $ a & endedTurnL .~ True
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
  DiscardTopOfDeck iid n source mTarget | iid == investigatorId -> handleDiscardTopOfDeck a iid n source mTarget
  Do (DiscardTopOfDeck iid n source mTarget) | iid == investigatorId -> handleDoDiscardTopOfDeck a iid n source mTarget
  DiscardUntilFirst iid' source (Deck.InvestigatorDeck iid) matcher | iid == investigatorId -> handleDiscardUntilFirst a iid' source iid matcher
  RevealUntilFirst iid source (Deck.InvestigatorDeck iid') matcher | iid == investigatorId && iid' == iid -> handleRevealUntilFirst a iid source iid' matcher
  DrawStartingHand iid | iid == investigatorId -> handleDrawStartingHand a iid
  Instead (DoDrawCards iid) msg' | iid == toId a -> do
    let
      isDraw = \case
        Run msgs -> any isDraw msgs
        Search s -> s.isDraw
        _ -> False
      afterDrawMessages =
        case investigatorDrawing >>= cardDrawAndThen of
          Just m | isDraw msg' -> [m]
          _ -> []
    mMsg <-
      maybeToList <$> popMessageMatching \case
        DrawEnded _ iid' -> iid == iid'
        _ -> False
    pushAll $ mMsg <> [msg'] <> afterDrawMessages
    pure $ a & drawingL .~ Nothing
  DrawCards iid cardDraw | iid == toId a -> handleDrawCards a iid cardDraw
  MoveTopOfDeckToBottom _ (Deck.InvestigatorDeck iid) n | iid == investigatorId -> handleMoveTopOfDeckToBottom a iid n
  DoDrawCards iid | iid == toId a -> handleDoDrawCards a iid
  ReplaceCurrentCardDraw iid drawing | iid == investigatorId -> handleReplaceCurrentCardDraw a iid drawing
  Do (DrawCards iid cardDraw) | iid == toId a && cardDraw.deck == Deck.InvestigatorDeck iid -> handleDoDrawCardsV2 a iid cardDraw
  InvestigatorDrewPlayerCardFrom iid card mDeck | iid == investigatorId -> handleInvestigatorDrewPlayerCardFrom a iid card mDeck msg
  Do (InvestigatorDrewPlayerCardFrom iid card mdeck) | iid == investigatorId -> handleDoInvestigatorDrewPlayerCardFrom a iid card mdeck
  InvestigatorSpendClues iid n | iid == investigatorId -> do
    includeStory <- not <$> hasCampaignOption PlayersDoNotControlStoryAssetClues
    let storyWrapper = if includeStory then id else (<> AssetNonStory)
    assetsWithClues <- select $ storyWrapper $ assetControlledBy iid <> AssetWithAnyClues
    afterWindow <- checkAfter $ Window.SpentClues iid n
    pushAll [if null assetsWithClues then Do msg else DoStep n msg, afterWindow]
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
  SpendResources iid n | iid == investigatorId -> handleSpendResources a iid n msg
  Do (SpendResources iid n) | iid == investigatorId -> handleDoSpendResources a iid n
  LoseResources iid source n | iid == investigatorId -> handleLoseResources a iid source n msg
  Do (LoseResources iid source n) | iid == investigatorId -> liftRunMessage (RemoveTokens source (toTarget a) #resource n) a
  LoseTokens iid _source token rule | iid == investigatorId -> do
    let
      f =
        case rule of
          AllLost -> deleteMap token
          AllLostBut n -> ix token %~ min n
          Lose n -> subtractTokens token n
    pure $ a & tokensL %~ f
  TakeResources iid n source True | iid == investigatorId -> handleTakeResources a iid n source
  TakeResources iid n source False | iid == investigatorId -> handleTakeResourcesV2 a iid n source msg
  Do (TakeResources iid n source False) | iid == investigatorId -> do
    canGain <- can.gain.resources (sourceToFromSource source) iid
    if canGain
      then do
        mods <- getModifiers a
        let additional = sum [x | AdditionalResources x <- mods]
        when (n + additional > 0) do
          afterWindowMsg <- checkWindows [mkAfter (Window.GainsResources iid source (n + additional))]
          push afterWindowMsg
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
    when (token == #resource)
      $ Lifted.updateHistory investigatorId
      $ HistoryItem HistoryResourcesGained n
    pure $ a & tokensL %~ addTokens token n
  RemoveTokens _ (isTarget a -> True) token n -> do
    case token of
      Damage | a.assignedHealthDamage > 0 -> do
        let subtractFromAssigned = min a.assignedHealthDamage n
            subtractFromPool = max 0 (n - subtractFromAssigned)
        pure
          $ a
          & (tokensL %~ subtractTokens token subtractFromPool)
          & (assignedHealthDamageL -~ subtractFromAssigned)
      Horror | a.assignedSanityDamage > 0 -> do
        let subtractFromAssigned = min a.assignedSanityDamage n
            subtractFromPool = max 0 (n - subtractFromAssigned)
        pure
          $ a
          & (tokensL %~ subtractTokens token subtractFromPool)
          & (assignedSanityDamageL -~ subtractFromAssigned)
      _ -> pure $ a & tokensL %~ subtractTokens token n
  DoBatch _ (EmptyDeck iid mDrawing) | iid == investigatorId -> do
    player <- getPlayer iid
    pushAll
      $ [EmptyDeck iid mDrawing]
      <> maybeToList mDrawing
      <> [chooseOne player [Label "$label.yourDeckIsEmptyTakeHorror" [assignHorror iid EmptyDeckSource 1]]]
    pure a
  EmptyDeck iid _ | iid == investigatorId -> handleEmptyDeck a iid
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
  SendMessage (isTarget a -> True) msg' -> liftRunMessage msg' a
  ForTarget (isTarget a -> True) (DoStep 2 (ForInvestigator _ AllDrawCardAndResource)) | not (a ^. defeatedL || a ^. resignedL) -> do
    lift $ takeUpkeepResources a
  LoadDeck iid deck | iid == investigatorId -> handleLoadDeck a iid deck
  LoadSideDeck iid deck | iid == investigatorId -> handleLoadSideDeck a iid deck
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
                . (discardL %~ filter ((/= card) . PlayerCard))
          )
          cards
    let a' = a & update & foundCardsL %~ Map.map (filter (`notElem` cards))
    when (null a'.deck) do
      pushM $ checkWhen $ Window.DeckHasNoCards investigatorId
      pushM $ checkAfter $ Window.DeckHasNoCards investigatorId
    pure a'
  ChaosTokenCanceled iid source token | iid == investigatorId -> do
    whenWindow <- checkWindows [mkWhen (Window.CancelChaosToken iid token)]
    whenWindow2 <- checkWindows [mkWhen (Window.CancelledOrIgnoredCardOrGameEffect source Nothing)]
    afterWindow <- checkWindows [mkAfter (Window.CancelChaosToken iid token)]
    afterWindow2 <- checkWindows [mkAfter (Window.CancelledOrIgnoredCardOrGameEffect source Nothing)]
    pushAll [whenWindow, whenWindow2, afterWindow2, afterWindow]
    pure a
  ChaosTokenIgnored iid source token | iid == toId a -> do
    whenWindow <- checkWindows [mkWhen (Window.IgnoreChaosToken iid token)]
    whenWindow2 <- checkWindows [mkWhen (Window.CancelledOrIgnoredCardOrGameEffect source Nothing)]
    afterWindow <- checkWindows [mkAfter (Window.IgnoreChaosToken iid token)]
    afterWindow2 <- checkWindows [mkAfter (Window.CancelledOrIgnoredCardOrGameEffect source Nothing)]
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
  DoStep 3 (CommitToSkillTest skillTestId triggerMessage') -> do
    getSkillTest >>= traverse_ \skillTest -> do
      let iid = skillTestInvestigator skillTest
      when (iid == a.id) do
        -- committedCards <- field InvestigatorCommittedCards iid
        uncommittableCards <- pure [] -- filterM (`withoutModifier` MustBeCommitted) committedCards
        skillTestModifiers' <- getModifiers (SkillTestTarget skillTest.id)
        committableCards <-
          filter (\c -> CanCommitAfterRevealingTokens `elem` cdCommitRestrictions (toCardDef c))
            <$> getCommittableCards (toId a)
        let
          mustCommit = any (elem MustBeCommittedToYourTest . cdCommitRestrictions . toCardDef) committableCards
          triggerMessage =
            [ triggerMessage'
            | CannotPerformSkillTest `notElem` skillTestModifiers' && not mustCommit
            ]
          beginMessage = DoStep 3 (CommitToSkillTest skillTestId triggerMessage')
        player <- getPlayer iid
        when (notNull committableCards || notNull uncommittableCards) do
          push
            $ SkillTestAsk
            $ chooseOne player
            $ map
              (\card -> targetLabel (toCardId card) [SkillTestCommitCard iid card, beginMessage])
              committableCards
            <> [ targetLabel (toCardId card) [SkillTestUncommitCard iid card, AddToHand iid [card], beginMessage]
               | card <- uncommittableCards
               ]
            <> triggerMessage
    -- NOTE: Currently not possible to commit after revealing tokens when not your test
    -- when (iid /= a.id) do
    --   -- committedCards <- field InvestigatorCommittedCards investigatorId
    --   let beginMessage = DoStep 3 (CommitToSkillTest skillTestId triggerMessage')
    --   committableCards <-
    --     filter (\c -> CanCommitAfterRevealingTokens `elem` cdCommitRestrictions (toCardDef c))
    --       <$> getCommittableCards a.id
    --   uncommittableCards <- pure [] -- filterM (`withoutModifier` MustBeCommitted) committedCards
    --   player <- getPlayer investigatorId
    --   pushWhen (notNull committableCards || notNull uncommittableCards)
    --     $ SkillTestAsk
    --     $ chooseOne player
    --     $ map
    --       (\card -> targetLabel (toCardId card) [SkillTestCommitCard investigatorId card, beginMessage])
    --       committableCards
    --     <> [ targetLabel
    --            (toCardId card)
    --            [SkillTestUncommitCard investigatorId card, AddToHand investigatorId [card], beginMessage]
    --        | card <- uncommittableCards
    --        ]
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
  SpendActions iid _ _ 0 | iid == investigatorId -> handleSpendActions a iid
  SpendActions iid source mAction n | iid == investigatorId -> handleSpendActionsV2 a iid source mAction n
  UseEffectAction iid eid _ | iid == investigatorId -> handleUseEffectAction a iid eid
  LoseActions iid source n | iid == investigatorId -> handleLoseActions a iid source n msg
  Do (LoseActions iid _source n) | iid == investigatorId -> handleDoLoseActions a iid n
  SetActions iid _ 0 | iid == investigatorId -> handleSetActions a iid
  SetActions iid _ n | iid == investigatorId -> handleSetActionsV2 a iid n
  SetAsideCards cards -> do
    pure
      $ a
      & (handL %~ filter (`notElem` cards))
      & (discardL %~ filter ((`notElem` cards) . PlayerCard))
      & (deckL %~ Deck . filter ((`notElem` cards) . PlayerCard) . unDeck)
  GainActions iid _ n | iid == investigatorId -> handleGainActions a iid n
  LoseAdditionalAction iid n | iid == investigatorId -> handleLoseAdditionalAction a iid n
  TakeActions iid actions cost | iid == investigatorId -> handleTakeActions a iid actions cost
  TakenActions iid actions | iid == investigatorId -> handleTakenActions a iid actions
  PerformedActions iid actions | iid == investigatorId -> handlePerformedActions a iid actions
  PutCardOnTopOfDeck _ (Deck.InvestigatorDeck iid) card | iid == toId a -> handlePutCardOnTopOfDeck a iid card
  PutCardOnTopOfDeck _ _ card -> handlePutCardOnTopOfDeckV2 a card
  PutCardOnBottomOfDeck _ (Deck.InvestigatorDeck iid) card | iid == toId a -> handlePutCardOnBottomOfDeck a iid card
  PutCardOnBottomOfDeck _ _ card -> handlePutCardOnBottomOfDeckV2 a card
  DebugAddToHand iid cardId | iid == investigatorId -> do
    card <- getCard cardId
    liftRunMessage (AddToHand iid [card]) a
  DrawToHandFrom iid deck cards | iid == investigatorId -> handleDrawToHandFrom a iid deck cards
  DrawToHand iid cards | iid == investigatorId -> handleDrawToHand a iid cards
  AddToHand iid cards | iid == investigatorId -> handleAddToHand a iid cards msg
  Do (AddToHand iid cards) | iid == investigatorId -> handleDoAddToHand a iid cards
  SwapPlaces (aTarget, _) (_, newLocation) | a `is` aTarget -> handleSwapPlaces a aTarget newLocation
  SwapPlaces (_, newLocation) (bTarget, _) | a `is` bTarget -> handleSwapPlacesV2 a newLocation bTarget
  ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [] | iid == investigatorId -> handleShuffleCardsIntoDeck a iid
  ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) cards | iid == toId a -> handleShuffleCardsIntoDeckV2 a iid cards
  AddFocusedToHand _ (InvestigatorTarget iid') cardSource cardId | iid' == toId a -> handleAddFocusedToHand a iid' cardSource cardId
  DrawFocusedToHand _ (InvestigatorTarget iid') cardSource cardId | iid' == toId a -> handleDrawFocusedToHand a iid' cardSource cardId
  CommitCard _ card -> do
    pure $ a & foundCardsL . each %~ filter (/= card) & discardL %~ filter ((/= card) . toCard)
  AddFocusedToTopOfDeck _ (InvestigatorTarget iid') cardId | iid' == toId a -> handleAddFocusedToTopOfDeck a iid' cardId
  ShuffleAllFocusedIntoDeck _ (InvestigatorTarget iid') | iid' == investigatorId -> handleShuffleAllFocusedIntoDeck a iid'
  PutAllFocusedIntoDiscard _ (InvestigatorTarget iid') | iid' == investigatorId -> handlePutAllFocusedIntoDiscard a iid'
  UpdateSearchReturnStrategy iid zone returnStrategy | iid == investigatorId -> handleUpdateSearchReturnStrategy a iid zone returnStrategy
  EndSearch iid _ (InvestigatorTarget iid') _ | iid == investigatorId -> handleEndSearch a iid iid'
  EndSearch iid _ _ _ | iid == investigatorId -> handleEndSearchV2 a iid
  SearchEnded (isTarget a -> True) -> handleSearchEnded a
  CancelSearch (isTarget a -> True) -> handleCancelSearch a
  Search (MkSearch searchType iid _ (InvestigatorTarget iid') zones _ _ _ _) | iid' == toId a -> handleSearch a searchType iid iid' zones msg
  DoBatch _ (Search (MkSearch _ iid _ (InvestigatorTarget iid') zones _ foundStrategy _ _)) | iid' == toId a -> do
    let isDrawing = isSearchDraw foundStrategy
    let deck = Deck.InvestigatorDeck iid'
    cid <- getRandom
    wouldDrawCard <- checkWindows [mkWhen (Window.WouldDrawCard iid cid deck)]
    let isFromDeck = any (zoneIsFromDeck . fst) zones
    pushAll $ [wouldDrawCard | isDrawing && isFromDeck] <> [Do msg, DrawEnded cid iid]
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

      pushBatch batchId $ ResolveSearch (toTarget investigatorId)
      pushBatch batchId $ EndSearch investigatorId source target cardSources

      pure
        $ a
        & searchL
        ?~ MkSearch searchType iid source target cardSources cardMatcher foundStrategy foundCards []
  ResolveSearch (isTarget a -> True) -> handleResolveSearch a
  RemoveFromDiscard iid cardId | iid == investigatorId -> handleRemoveFromDiscard a iid cardId
  CreateInBonded iid cardCode | iid == investigatorId -> do
    for_ (lookupCardDef cardCode) (genCard >=> Lifted.placeInBonded iid)
    pure a
  PlaceInBonded iid card | iid == investigatorId -> do
    pushAll [When (PlaceInBonded iid card), Do (PlaceInBonded iid card)]
    pure a
  Do (PlaceInBonded iid card) | iid == investigatorId -> do
    pure
      $ a
      & (bondedCardsL %~ nub . (card :))
      & (handL %~ filter (/= card))
      & (discardL %~ filter ((/= card) . toCard))
      & (deckL %~ filter ((/= card) . toCard))
      & (foundCardsL . each %~ filter (/= card))
      & (cardsUnderneathL %~ filter (/= card))
      & (decksL . each %~ filter (/= card))
  SufferTrauma iid physical mental | iid == investigatorId -> handleSufferTrauma a iid physical mental
  SetTrauma iid physical mental | iid == investigatorId -> handleSetTrauma a iid physical mental
  CheckTrauma iid | iid == investigatorId -> handleCheckTrauma a iid
  HealTrauma iid physical mental | iid == investigatorId -> handleHealTrauma a iid physical mental
  GainXP iid _ amount | iid == investigatorId -> pure $ a & xpL +~ amount
  SetXP iid amount | iid == investigatorId -> pure $ a & xpL .~ amount
  SpendXP iid amount | iid == investigatorId -> do
    pure $ a & xpL %~ max 0 . subtract amount
  InvestigatorPlaceCluesOnLocation iid source n | iid == investigatorId -> do
    withLocationOf iid \lid -> do
      batchId <- getRandom
      would <-
        Helpers.checkWindow
          $ (mkWhen $ Window.WouldPlaceClueOnLocation iid lid source n) {windowBatchId = Just batchId}
      pushBatched batchId [would, Do msg]
    pure a
  Do (InvestigatorPlaceCluesOnLocation iid source n) | iid == investigatorId -> do
    withLocationOf iid \lid -> do
      assetClues <- selectSum AssetClues $ assetControlledBy iid <> AssetWithAnyClues
      let cluesToPlace = min n (investigatorClues a + assetClues)
      push $ MoveTokens source (toSource a) (LocationTarget lid) Clue cluesToPlace
      pushM
        $ checkAfter
        $ Window.InvestigatorPlacedFromTheirPool iid source (toTarget lid) #clue cluesToPlace
    pure a
  InvestigatorPlaceAllCluesOnLocation iid source | iid == investigatorId -> do
    -- [AsIfAt] assuming as if is still in effect
    withLocationOf iid \lid -> do
      push $ PlaceTokens source (LocationTarget lid) Clue (investigatorClues a)
    pure $ a & tokensL %~ removeAllTokens Clue
  RemoveFromBearersDeckOrDiscard card -> handleRemoveFromBearersDeckOrDiscard a card
  RemovePlayerCardFromGame _addToRemovedFromGame card -> handleRemovePlayerCardFromGame a card
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
      goLocation = \case
        ProxyTarget t _ -> goLocation t
        LocationTarget lid -> do
          clues <- field LocationClues lid
          pure
            $ mkWhen (Window.PassInvestigationSkillTest iid lid n)
            : [mkWhen (Window.SuccessfullyInvestigateWithNoClues iid lid) | clues == 0]
        BothTarget t1 t2 -> (<>) <$> goLocation t1 <*> goLocation t2
        _ -> pure []
      goEnemy mk = \case
        ProxyTarget t _ -> goEnemy mk t
        EnemyTarget eid -> [mkWhen $ mk eid n]
        BothTarget t1 t2 -> goEnemy mk t1 <> goEnemy mk t2
        _ -> []
    windows <- case mAction of
      Just Action.Investigate -> concat <$> traverse goLocation (maybeToList mTarget)
      Just Action.Fight -> pure $ goEnemy (Window.SuccessfulAttackEnemy iid source) =<< maybeToList mTarget
      -- SuccessfulEvadeEnemy fires from the enemy's evade resolution (around
      -- EnemyEvaded) so reactions see the enemy exhausted; see Enemy.Runner.
      _ -> pure []
    pushM $ checkWindows $ mkWhen (Window.PassSkillTest mAction source iid n) : windows
    pure a
  After (PassedSkillTest iid mAction source (InvestigatorTarget iid') _ n) | iid == iid' && iid == toId a -> do
    mTarget <- getSkillTestTarget
    let
      goLocation = \case
        ProxyTarget t _ -> goLocation t
        LocationTarget lid -> do
          clues <- field LocationClues lid
          pure
            $ mkAfter (Window.PassInvestigationSkillTest iid lid n)
            : [mkAfter (Window.SuccessfullyInvestigateWithNoClues iid lid) | clues == 0]
        BothTarget t1 t2 -> (<>) <$> goLocation t1 <*> goLocation t2
        _ -> pure []
      goEnemy mk = \case
        ProxyTarget t _ -> goEnemy mk t
        EnemyTarget eid -> [mkAfter $ mk eid n]
        BothTarget t1 t2 -> goEnemy mk t1 <> goEnemy mk t2
        _ -> []
    windows <- case mAction of
      Just Action.Investigate -> concat <$> traverse goLocation (maybeToList mTarget)
      Just Action.Fight -> pure $ goEnemy (Window.SuccessfulAttackEnemy iid source) =<< maybeToList mTarget
      -- SuccessfulEvadeEnemy fires from the enemy's evade resolution (around
      -- EnemyEvaded) so reactions see the enemy exhausted; see Enemy.Runner.
      _ -> pure []
    pushM $ checkWindows $ mkAfter (Window.PassSkillTest mAction source iid n) : windows
    pure a
  PlayerWindow iid additionalActions isAdditional immediate | iid == investigatorId -> handlePlayerWindow a iid additionalActions isAdditional immediate
  PlayerWindow iid additionalActions isAdditional False | iid /= investigatorId && a.inGame -> handlePlayerWindowV2 a iid additionalActions isAdditional
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
  UseCardAbility iid (isSource a -> True) 500 _ _ -> handleUseCardAbility a iid
  UseCardAbility iid (isSource a -> True) 501 _ _ -> handleUseCardAbilityV2 a iid
  UseCardAbility iid (isSource a -> True) 502 _ _ -> handleUseCardAbilityV3 a iid
  UseAbility _ ab _ | isSource a ab.source || isProxySource a ab.source -> handleUseAbility a ab msg
  Do (UseAbility iid ability windows) | iid == investigatorId -> handleDoUseAbility a iid ability windows
  DoNotCountUseTowardsAbilityLimit iid ability | iid == investigatorId -> handleDoNotCountUseTowardsAbilityLimit a iid ability
  SkillTestEnds {} -> do
    pure
      $ a
      & ( usedAbilitiesL %~ filter \UsedAbility {..} ->
            case abilityLimitType (abilityLimit usedAbility) of
              Just PerTestOrAbility -> False
              Just PerTest -> False
              _ -> True
        )
      & (usedAbilitiesL %~ map (\u -> u {usedThisWindow = False}))
  AfterRevelation {} -> do
    pure
      $ a
      & ( usedAbilitiesL
            %~ filter (\UsedAbility {..} -> abilityLimitType (abilityLimit usedAbility) /= Just PerTestOrAbility)
        )
      & (usedAbilitiesL %~ map (\u -> u {usedThisWindow = False}))
  ResolvedAbility {} -> handleResolvedAbility a
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
  RemovedLocation lid | investigatorLocation a == Just lid -> handleRemovedLocation a lid
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
  Do (PlaceInvestigator iid placement) | iid == toId a -> handleDoPlaceInvestigator a iid placement
  ResetMetadata (isTarget a -> True) -> pure $ a & metaL .~ object []
  _ -> pure a

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
  if any (`elem` mods) [CannotGainResources, DoNotCollectResourcesDuringUpkeep]
    then pure a
    else
      if MayChooseNotToTakeUpkeepResources `elem` mods
        then do
          player <- getPlayer (toId a)
          push
            $ chooseOne
              player
              [ Label "$label.doNotTakeResources" []
              , Label
                  (withI18n $ countVar amount $ ikey' "label.takeResources")
                  [TakeResources (toId a) amount (ResourceSource $ toId a) False]
              ]
          pure a
        else
          pure $ a & tokensL %~ addTokens Resource amount
