{-# OPTIONS_GHC -Wno-unused-record-wildcards -Wno-unused-imports -Wno-unused-matches -Wno-missing-signatures -Wno-orphans #-}
{-# LANGUAGE TypeAbstractions #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Investigator.Runner.Action where


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
  longestUniqueStreak,
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

handlePerformAction a@InvestigatorAttrs{..} iid source action = do
  let windows' = defaultWindows iid
  let decreaseCost = flip applyAbilityModifiers [ActionCostModifier (-1)]
  actions <-
    filterM (getCanPerformAbility iid windows')
      . filter (`abilityIs` action)
      =<< getActionsWith iid windows' decreaseCost
  handCards <- field InvestigatorHand iid
  let actionCards = filter (elem action . actionsToList . cdActions . toCardDef) handCards
  playableCards <- filterM (getIsPlayable iid source (UnpaidCost NoAction) windows') actionCards
  when (notNull actions || notNull playableCards) do
    Lifted.chooseOne iid
      $ map ((\f -> f windows' [] []) . AbilityLabel iid) actions
      <> [targetLabel (toCardId item) [PayCardCost iid item windows'] | item <- playableCards]
  pure a

handleSpendResources a@InvestigatorAttrs{..} iid n msg = do
  let defaultFlow = do
        beforeWindowMsg <- checkWindows [mkWhen (Window.SpendsResources iid n)]
        pushAll [beforeWindowMsg, Do msg]
        pure a
  mods <- getModifiers a
  resourcePools <- forToSnd [aid | AsIfResourcePool aid <- mods] (field AssetResources)
  let totalPoolResources = sum (map snd resourcePools)
  if totalPoolResources == 0
    then defaultFlow
    else do
      player <- getPlayer iid
      push
        $ chooseOrRunN player n
        $ concatMap
          (\(aid, k) -> replicate k (targetLabel aid [RemoveResources (toSource a) (toTarget aid) 1]))
          resourcePools
        <> replicate a.resources (ResourceLabel iid [Do (SpendResources iid 1)])
      pure a

handleDoSpendResources a@InvestigatorAttrs{..} iid n = do
  Lifted.checkAfter (Window.SpendsResources iid n)
  pure $ a & tokensL %~ subtractTokens Resource n

handleLoseResources a@InvestigatorAttrs{..} iid source n msg = do
  beforeWindowMsg <- checkWindows [mkWhen (Window.LostResources iid source n)]
  afterWindowMsg <- checkWindows [mkAfter (Window.LostResources iid source n)]
  pushAll [beforeWindowMsg, Do msg, afterWindowMsg]
  pure a

handleTakeResources a@InvestigatorAttrs{..} iid n source = do
  let ability = restricted iid ResourceAbility (Self <> Never) (ActionAbility #resource Nothing $ ActionCost 1)
  whenActivateAbilityWindow <- checkWhen $ Window.ActivateAbility iid [] ability
  afterActivateAbilityWindow <- checkAfter $ Window.ActivateAbility iid [] ability
  beforeWindowMsg <- checkWhen $ Window.PerformAction iid #resource
  afterWindowMsg <- checkAfter $ Window.PerformAction iid #resource
  canGain <- can.gain.resources (sourceToFromSource source) iid
  modifiers' <- getModifiers iid

  when canGain do
    pushAll
      $ [ BeginAction
        , beforeWindowMsg
        , whenActivateAbilityWindow
        , TakeActions iid [#resource] (ActionCost 1)
        ]
      <> [ Will (CheckAttackOfOpportunity iid False Nothing)
         | ActionDoesNotCauseAttacksOfOpportunity #resource `notElem` modifiers'
         ]
      <> [ CheckAttackOfOpportunity iid False Nothing
         | ActionDoesNotCauseAttacksOfOpportunity #resource `notElem` modifiers'
         ]
      <> [ TakeResources iid n source False
         , afterWindowMsg
         , afterActivateAbilityWindow
         , FinishAction
         , TakenActions iid [#resource]
         ]
  pure a

handleTakeResourcesV2 a@InvestigatorAttrs{..} iid n source msg = do
  canGain <- can.gain.resources (sourceToFromSource source) iid
  when canGain do
    beforeWindowMsg <- checkWindows [mkWhen (Window.GainsResources iid source n)]
    pushAll [beforeWindowMsg, Do msg]
  pure a

handleSpendActions a@InvestigatorAttrs{..} iid = do
  pure a

handleSpendActionsV2 a@InvestigatorAttrs{..} iid source mAction n = do
  -- We want to try and spend the most restrictive action so we get any
  -- action that is not any additional action first, and if not that then the
  -- any additional action
  additionalActions <- getAdditionalActions a
  specificAdditionalActions <-
    filterM
      ( andM
          . sequence
            [ pure . ((/= AnyAdditionalAction) . additionalActionType)
            , additionalActionCovers source (toList mAction)
            ]
      )
      additionalActions
  let anyAdditionalActions = filter ((== AnyAdditionalAction) . additionalActionType) additionalActions

  case specificAdditionalActions of
    [] -> case anyAdditionalActions of
      [] -> do
        Lifted.updateHistory iid (HistoryItem HistoryActionsSpent n)
        pure $ a & remainingActionsL %~ max 0 . subtract n
      xs -> do
        player <- getPlayer iid
        push
          $ chooseOrRunOne player
          $ [ Label
                ("Use action from " <> lbl)
                [ UpdateHistory iid (HistoryItem HistoryActionsSpent 1)
                , LoseAdditionalAction iid ac
                , SpendActions iid source mAction (n - 1)
                ]
            | ac@(AdditionalAction lbl _ _) <- xs
            ]
        pure a
    xs -> do
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ [ Label
              ("Use action from " <> lbl)
              [ UpdateHistory iid (HistoryItem HistoryActionsSpent 1)
              , LoseAdditionalAction iid ac
              , SpendActions iid source mAction (n - 1)
              ]
          | ac@(AdditionalAction lbl _ _) <- xs
          ]
      pure a

handleUseEffectAction a@InvestigatorAttrs{..} iid eid = do
  additionalActions <- getAdditionalActions a
  let
    isEffectAction aAction = case additionalActionType aAction of
      EffectAction _ eid' -> eid == eid'
      _ -> False
  case find isEffectAction additionalActions of
    Nothing -> pure a
    Just aAction -> pure $ a & usedAdditionalActionsL %~ (aAction :)

handleLoseActions a@InvestigatorAttrs{..} iid source n msg = do
  beforeWindowMsg <- checkWindows [mkWhen $ Window.LostActions iid source n]
  afterWindowMsg <- checkWindows [mkAfter $ Window.LostActions iid source n]
  pushAll [beforeWindowMsg, Do msg, afterWindowMsg]
  pure a

handleDoLoseActions a@InvestigatorAttrs{..} iid n = do
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

handleSetActions a@InvestigatorAttrs{..} iid = do
  additionalActions <- getAdditionalActions a
  pure
    $ a
    & remainingActionsL
    .~ 0
    & usedAdditionalActionsL
    .~ nub (investigatorUsedAdditionalActions <> additionalActions)

handleSetActionsV2 a@InvestigatorAttrs{..} iid n = do
  pure $ a & remainingActionsL .~ n

handleGainActions a@InvestigatorAttrs{..} iid n = do
  -- TODO: If we add a window here we need to reconsider Ace of Rods, likely it would need a Do variant
  pure $ a & remainingActionsL +~ n

handleLoseAdditionalAction a@InvestigatorAttrs{..} iid n = do
  pure $ a & usedAdditionalActionsL %~ (n :)

handleTakeActions a@InvestigatorAttrs{..} iid actions cost = do
  push $ PayForAbility (abilityEffect a actions cost) []
  pure a

handleTakenActions a@InvestigatorAttrs{..} iid actions = do
  let previous = fromMaybe [] $ lastMay investigatorActionsPerformed
  let duplicated = actions `List.intersect` previous
  let streak = longestUniqueStreak (actions : reverse investigatorActionsPerformed)

  when (notNull duplicated)
    $ pushM
    $ checkWindows [mkAfter (Window.PerformedSameTypeOfAction iid duplicated)]

  when (length streak > 1)
    $ pushM
    $ checkWindows
      [mkAfter (Window.PerformedDifferentTypesOfActionsInARow iid (length streak) streak)]

  when (#parley `elem` actions && #parley `notElem` previous)
    $ pushM
    $ checkWindows [mkWhen (Window.FirstTimeParleyingThisRound iid)]

  pure $ a & actionsTakenL %~ (<> [actions]) & actionsPerformedL %~ (<> [actions])

handlePerformedActions a@InvestigatorAttrs{..} iid actions = do
  let previous = fromMaybe [] $ lastMay investigatorActionsPerformed
  let duplicated = actions `List.intersect` previous

  when (notNull duplicated)
    $ pushM
    $ checkWindows [mkAfter (Window.PerformedSameTypeOfAction iid duplicated)]

  when (#parley `elem` actions && #parley `notElem` previous)
    $ pushM
    $ checkWindows [mkWhen (Window.FirstTimeParleyingThisRound iid)]

  pure $ a & actionsPerformedL %~ (<> [actions])

handlePlayerWindow a@InvestigatorAttrs{..} iid additionalActions isAdditional immediate = do
  modifiers <- lift $ withSpan_ "getModifiers" $ getModifiers iid
  mTurnInvestigator <-
    if immediate
      then pure []
      else maybeToList <$> selectOne TurnInvestigator
  let
    windows =
      -- An "immediate" window is a granted/extra action (Carson Sinclair, Quick
      -- Thinking, Swift Reflexes, "as if it were your turn" effects). It is NOT
      -- actually your turn, so it presents only the NonFast action-taking window
      -- (no DuringTurn, no FastPlayerWindow): basic actions and non-fast action
      -- cards remain available, but "during your turn" Fast cards and fast/[free]
      -- abilities -- including taking control of a key -- are not. See #4894.
      map (mkWhen . Window.DuringTurn) mTurnInvestigator
        <> [mkWhen Window.FastPlayerWindow | not immediate]
        <> [mkWhen Window.NonFast]

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
        <> [PlayerWindow iid additionalActions isAdditional immediate]
    else do
      let mustTakeAbilities = [aref | MustPerformAbilityIfCan aref <- modifiers]
      let mustTakeActions = if null mustTakeAbilities then [] else filter ((`elem` mustTakeAbilities) . (.ref)) actions
      let actions' = if null mustTakeActions then actions else mustTakeActions
      canAffordTakeResources <- getCanAfford a [#resource]
      canAffordDrawCards <- getCanAfford a [#draw]
      additionalActions' <- getAdditionalActions a
      let
        usesAction = not isAdditional
        drawCardsF = if usesAction then drawCardsAction else drawCards
        effectActions = flip mapMaybe additionalActions' $ \case
          AdditionalAction _ _ (EffectAction t effectId) ->
            Just
              $ EffectActionButton
                (Tooltip t)
                effectId
                [UseEffectAction iid effectId windows]
          _ -> Nothing

      playableCards <-
        lift $ withSpan_ "getPlayableCards" $ getPlayableCards iid iid (UnpaidCost NeedsAction) windows
      let drawing = drawCardsF iid a 1

      let guardMustTake = if null mustTakeActions then id else const (pure False)

      canDraw <- guardMustTake $ canDo_ iid #draw
      canTakeResource <-
        guardMustTake $ (&&) <$> canDo_ iid #resource <*> can.gain.resources FromOtherSource iid
      canPlay <- guardMustTake $ canDo_ iid #play
      player <- getPlayer iid

      let
        promoteCanBecomeFast c = do
          mods <- getModifiers c
          let promoted = [BecomesFast AnyWindow | CanBecomeFast matcher <- modifiers, cardMatch c matcher]
          pure $ mods <> promoted

      playableCards' <-
        if canPlay
          then pure playableCards
          else playableCards & filterM (cardIsFast' promoteCanBecomeFast)

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
        <> [ targetLabel (toCardId c) [InitiatePlayCardWithWindows iid c Nothing NoPayment windows usesAction]
           | c <- playableCards'
           ]
        <> [EndTurnButton iid [ChooseEndTurn iid]]
        <> map ((\f -> f windows [] []) . AbilityLabel iid) actions'
        <> effectActions
  pure a

handlePlayerWindowV2 a@InvestigatorAttrs{..} iid additionalActions isAdditional = do
  let windows = [mkWhen Window.FastPlayerWindow]
  actions <- getActions investigatorId windows
  anyForced <- anyM (isForcedAbility investigatorId) actions
  unless anyForced $ do
    playableCards <-
      lift
        $ withSpan_ "getPlayableCards"
        $ getPlayableCards investigatorId investigatorId (UnpaidCost NeedsAction) windows
    let
      usesAction = not isAdditional
      choices =
        additionalActions
          <> [ targetLabel
                 c
                 [ InitiatePlayCardWithWindows investigatorId c Nothing NoPayment windows usesAction
                 , PlayerWindow iid additionalActions isAdditional False
                 ]
             | c <- playableCards
             ]
          <> map
            ( (\f -> f windows [] [PlayerWindow iid additionalActions isAdditional False])
                . AbilityLabel investigatorId
            )
            (filter (or . sequence [isFastAbility, not . isActionAbility]) actions)
    player <- getPlayer investigatorId
    unless (null choices) $ push $ AskPlayer $ Ask player $ PlayerWindowChooseOne choices
  pure a

handleUseCardAbility a@InvestigatorAttrs{..} iid = do
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

handleUseCardAbilityV2 a@InvestigatorAttrs{..} iid = do
  otherInvestigators <- select $ colocatedWith a <> not_ (InvestigatorWithId investigatorId)
  case nonEmpty otherInvestigators of
    Nothing -> error "No other investigators"
    Just (x :| xs) -> do
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ [ targetLabel
              iid'
              [ chooseOne
                  player
                  [ Label ("Give " <> sealName k <> " key") [PlaceSeal (toTarget iid') k]
                  | k <- toList investigatorSeals
                  ]
              ]
          | iid' <- x : xs
          ]
  pure a

handleUseCardAbilityV3 a@InvestigatorAttrs{..} iid = do
  otherInvestigators <-
    selectWithField InvestigatorSeals $ colocatedWith a
      <> not_ (InvestigatorWithId investigatorId)
      <> InvestigatorWithAnySeal
  case nonEmpty otherInvestigators of
    Nothing -> error "No other investigators"
    Just (x :| xs) -> do
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ [ targetLabel
              iid'
              [ chooseOne
                  player
                  [ Label ("Take " <> sealName k <> " key") [PlaceSeal (toTarget iid) k]
                  | k <- toList seals
                  ]
              ]
          | (iid', seals) <- x : xs
          ]
  pure a

handleUseAbility a@InvestigatorAttrs{..} ab msg = do
  push $ Do msg
  pure a

handleDoUseAbility a@InvestigatorAttrs{..} iid ability windows = do
  activeInvestigator <- selectOne ActiveInvestigator
  mods <- filter (\m -> m.kind == MayIgnoreLocationEffectsAndKeywords) <$> getFullModifiers iid
  -- mayIgnoreLocationEffectsAndKeywords <- hasModifier iid MayIgnoreLocationEffectsAndKeywords
  let
    mayIgnoreMods =
      case abilitySource ability of
        LocationSource _ -> mods
        IndexedSource _ (LocationSource _) -> mods
        ProxySource (LocationSource _) _ -> mods
        _ -> []
    resolveAbility =
      [SetActiveInvestigator iid | x <- maybeToList activeInvestigator, iid /= x]
        <> [PayForAbility ability windows, MoveWithSkillTest (ResolvedAbility ability)]
        <> [SetActiveInvestigator x | x <- maybeToList activeInvestigator, iid /= x]
  player <- getPlayer iid

  let
    target =
      case ability.limitType of
        Just PerSpawn -> Just $ toTarget $ Helpers.spawnedEnemy windows
        _ -> Nothing

  case mayIgnoreMods of
    [] -> pushAll resolveAbility
    [x] -> do
      after <- checkAfter $ Window.CancelledOrIgnoredCardOrGameEffect x.source (toCardId <$> x.card)
      push $ chooseOne player [Label "$label.ignoreEffect" [after], Label "$label.doNotIgnoreEffect" resolveAbility]
    _ -> error "Multiple may ignore modifiers, not sure which one to use"
  case find ((== ability) . usedAbility) investigatorUsedAbilities of
    Nothing -> do
      depth <- getWindowDepth
      -- NOTE: if a used ability is missing it's traits for some reason, it's
      -- likely because it was discarded before this point and we don't know
      -- anymore (see: Spires of Carcosa)
      traits' <- sourceTraits $ abilitySource ability
      let
        mMovementId = maybe (Helpers.getMovementId windows) (Just . (.id)) investigatorMovement
        upgradePerMove :: Ability -> Ability
        upgradePerMove = case mMovementId of
          Nothing -> id
          Just m -> over biplate \case
            PerMove -> PerMovement m
            other -> other
      let
        used =
          UsedAbility
            { usedAbility = upgradePerMove ability
            , usedAbilityInitiator = iid
            , usedAbilityWindows = windows
            , usedTimes = 1
            , usedDepth = depth
            , usedAbilityTraits = traits'
            , usedThisWindow = depth > 0
            , usedAbilityTarget = target
            }
      pure $ a & usedAbilitiesL %~ (used :)
    Just _ -> do
      let
        updateUsed used
          | usedAbility used == ability =
              used {usedTimes = usedTimes used + 1, usedAbilityWindows = usedAbilityWindows used <> windows}
          | otherwise = used
      pure $ a & usedAbilitiesL %~ map updateUsed

handleDoNotCountUseTowardsAbilityLimit a@InvestigatorAttrs{..} iid ability = do
  let
    updateUsed used
      | usedAbility used == ability = used {usedTimes = max 0 (usedTimes used - 1)}
      | otherwise = used
  pure $ a & usedAbilitiesL %~ map updateUsed

handleResolvedAbility a@InvestigatorAttrs{..} = do
  depth <- getWindowDepth
  pure
    $ a
    & ( usedAbilitiesL %~ filter \UsedAbility {..} ->
          case abilityLimitType (abilityLimit usedAbility) of
            Just PerTestOrAbility -> usedDepth <= depth
            _ -> True
      )
    & ( usedAbilitiesL
          %~ map
            ( \u ->
                case abilityLimitType (abilityLimit (usedAbility u)) of
                  Just PerTestOrAbility -> u {usedThisWindow = False}
                  _ -> u
            )
      )
