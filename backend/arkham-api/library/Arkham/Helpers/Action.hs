module Arkham.Helpers.Action where

import Arkham.Ability hiding (NoRestriction)
import Arkham.Action
import Arkham.Action.Additional
import Arkham.Asset.Types qualified as Field
import Arkham.Card
import Arkham.ClassSymbol
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.GameEnv (getAllAbilities, getCurrentWindowTick, getEntryTicks)
import Arkham.Helpers.Ability (getCanAffordAbility, getCanPerformAbility, isForcedAbility)
import Arkham.Helpers.CombatTarget
import Arkham.Helpers.Modifiers (
  ModifierType (..),
  getModifiers,
  withGrantedAction,
  withModifiersOf,
 )
import {-# SOURCE #-} Arkham.Helpers.Playable (getPlayableCards)
import Arkham.Helpers.Ref (sourceToMaybeCard, sourceToTarget)
import Arkham.Helpers.Source (sourceTraits)
import {-# SOURCE #-} Arkham.Helpers.Window (windowMatches)
import Arkham.Id
import Arkham.Investigator.Types (Field (..), Investigator, InvestigatorAttrs (..))
import Arkham.Matcher (replaceThisLocation)
import Arkham.Matcher.Ability
import Arkham.Matcher.Action
import Arkham.Matcher.Card
import Arkham.Matcher.Enemy
import Arkham.Matcher.Window
import Arkham.Prelude
import Arkham.Projection
import Arkham.Source
import Arkham.Target
import Arkham.Tracing
import Arkham.Window (Window (..), defaultWindows)
import Arkham.Window qualified as Window

data IsFast = IsFast | NotFast
  deriving stock Eq

actionMatches :: (Tracing m, HasGame m) => InvestigatorId -> Action -> ActionMatcher -> m Bool
actionMatches _ _ AnyAction = pure True
actionMatches _ a (ActionIs a') = pure $ a == a'
actionMatches iid a (ActionMatches as) = allM (actionMatches iid a) as
actionMatches iid a (ActionOneOf as) = anyM (actionMatches iid a) as
actionMatches iid a (FirstActionMatchOfRound inner) = do
  performed <- concat <$> field InvestigatorActionsPerformed iid
  andM
    [ actionMatches iid a inner
    , noneM (\b -> actionMatches iid b inner) performed
    ]
actionMatches iid a RepeatableAction = do
  a' <- getAttrs @Investigator iid
  actions <- withGrantedAction iid GameSource $ getActions iid (defaultWindows iid)

  playableCards <-
    filterCards (not_ FastCard)
      <$> getPlayableCards iid iid (UnpaidCost NoAction) (defaultWindows iid)

  canAffordTakeResources <- withModifiersOf iid GameSource [ActionCostOf IsAnyAction (-1)] do
    getCanAfford a' [#resource]

  canAffordDrawCards <- withModifiersOf iid GameSource [ActionCostOf IsAnyAction (-1)] do
    getCanAfford a' [#draw]
  let available = filter (elem a . abilityActions) actions
  canDraw <- canDo_ iid #draw
  canTakeResource <- canDo_ iid #resource
  canPlay <- canDo_ iid #play
  pure
    $ or
      [ notNull available
      , canAffordTakeResources && canTakeResource && a == #resource
      , canAffordDrawCards && canDraw && a == #draw
      , canPlay && notNull playableCards && a == #play
      ]

canDo_ :: (HasGame m, Tracing m) => InvestigatorId -> Action -> m Bool
canDo_ iid action = canDo iid action NotFast

canDo :: (HasGame m, Tracing m) => InvestigatorId -> Action -> IsFast -> m Bool
canDo iid action isFast = do
  mods <- getModifiers iid
  let
    prevents = \case
      CannotPerformAction x -> preventsAction x
      CannotTakeAction x | isFast == NotFast && ActionsAreFree `notElem` mods -> preventsAction x
      MustTakeAction x | isFast == NotFast -> not <$> preventsAction x -- reads a little weird but we want only thing things x would prevent with cannot take action
      MustTakeAction _ -> pure False
      CannotDrawCards -> pure $ action == #draw
      CannotDrawCardsFromPlayerCardEffects -> pure $ action == #draw
      CannotManipulateDeck -> pure $ action == #draw
      CannotGainResources -> pure $ action == #resource
      _ -> pure False
    preventsAction = \case
      FirstOneOfPerformed as | action `elem` as -> do
        fieldP InvestigatorActionsPerformed (\taken -> all (\a -> all (notElem a) taken) as) iid
      FirstOneOfPerformed {} -> pure False
      IsAction action' -> pure $ action == action'
      AnyActionTarget as -> anyM preventsAction as
      EnemyAction {} -> pure False
      AssetAction {} -> pure False
      IsAnyAction {} -> pure True

  not <$> anyM prevents mods

additionalActionCovers
  :: (HasGame m, Tracing m) => Source -> [Action] -> AdditionalAction -> m Bool
additionalActionCovers source actions (AdditionalAction _ _ aType) = case aType of
  PlayCardRestrictedAdditionalAction matcher -> case source of
    CardIdSource cid -> elem cid . map toCardId <$> select matcher
    _ -> pure False
  TraitRestrictedAdditionalAction t actionRestriction -> case actionRestriction of
    NoRestriction -> member t <$> sourceTraits source
    AbilitiesOnly -> case source of
      AbilitySource {} -> member t <$> sourceTraits source
      UseAbilitySource {} -> member t <$> sourceTraits source
      _ -> pure False
  AbilityRestrictedAdditionalAction s idx -> pure $ isAbilitySource s idx source
  ActionRestrictedAdditionalAction a -> pure $ a `elem` actions
  EffectAction _ _ -> pure False
  AnyAdditionalAction -> pure True
  BountyAction -> pure False -- Has to be handled by Tony Morgan
  BobJenkinsAction -> pure False -- Has to be handled by Bob Jenkins

getCanAfford :: (HasGame m, Tracing m) => InvestigatorAttrs -> [Action] -> m Bool
getCanAfford a@InvestigatorAttrs {..} as = do
  actionCost <- getActionCost a as
  additionalActions <- getAdditionalActions a
  additionalActionCount <-
    countM (\aa -> anyM (\ac -> additionalActionCovers (toSource a) [ac] aa) as) additionalActions
  pure $ actionCost <= (investigatorRemainingActions + additionalActionCount)

getAdditionalActions :: HasGame m => InvestigatorAttrs -> m [AdditionalAction]
getAdditionalActions attrs = do
  mods <- getModifiers attrs
  let
    toAdditionalAction = \case
      GiveAdditionalAction ac -> [ac]
      AdditionalActions label source n | n > 0 -> map (\x -> AdditionalAction label (IndexedSource x source) AnyAdditionalAction) [1 .. n]
      _ -> []
    additionalActions = concatMap toAdditionalAction mods

  if CannotGainAdditionalActions `elem` mods
    then pure []
    else pure $ filter (`notElem` investigatorUsedAdditionalActions attrs) additionalActions

getActionCost :: HasGame m => InvestigatorAttrs -> [Action] -> m Int
getActionCost attrs as = do
  modifiers <- getModifiers (toTarget attrs)
  pure $ foldr applyModifier 1 modifiers
 where
  applyModifier (ActionCostOf match m) n =
    if any (matchTarget attrs match) as then n + m else n
  applyModifier (AdditionalActionCostOf match m) n =
    if any (matchTarget attrs match) as then n + m else n
  applyModifier _ n = n

matchTarget :: InvestigatorAttrs -> ActionTarget -> Action -> Bool
matchTarget attrs (AnyActionTarget as) action = any (\atarget -> matchTarget attrs atarget action) as
matchTarget attrs (FirstOneOfPerformed as) action =
  action `elem` as && all (\a -> all (notElem a) $ investigatorActionsPerformed attrs) as
matchTarget _ (IsAction a) action = action == a
matchTarget _ (EnemyAction a _) action = action == a
matchTarget _ (AssetAction a _) action = action == a
matchTarget _ IsAnyAction _ = True

getActions :: (Tracing m, HasGame m, HasCallStack) => InvestigatorId -> [Window] -> m [Ability]
getActions iid ws = getActionsWith iid ws id

getActionsWith
  :: (HasCallStack, Tracing m, HasGame m)
  => InvestigatorId
  -> [Window]
  -> (Ability -> Ability)
  -> m [Ability]
getActionsWith iid ws f = do
  investigatorModifiers <- getModifiers iid
  let
    abilityFilters =
      investigatorModifiers & mapMaybe \case
        CannotTriggerAbilityMatching m -> Just (TriggeredAbility <> m)
        _ -> Nothing

  unfilteredActions <- map f <$> getAllAbilities
  actions' <-
    if null abilityFilters
      then pure unfilteredActions
      else do
        ignored <- select (AbilityOneOf abilityFilters)
        pure $ filter (`notElem` ignored) unfilteredActions
  actionsWithSources <-
    concat <$> for actions' \action -> do
      case abilitySource action of
        ProxySource (AgendaMatcherSource m) base -> do
          sources <- selectMap AgendaSource m
          pure $ sources & map \source -> action {abilitySource = ProxySource source base}
        ProxySource (ActMatcherSource m) base -> do
          sources <- selectMap ActSource m
          pure $ sources & map \source -> action {abilitySource = ProxySource source base}
        ProxySource (AssetMatcherSource m) base -> do
          sources <- selectMap AssetSource m
          pure $ sources & map \source -> action {abilitySource = ProxySource source base}
        ProxySource (LocationMatcherSource m) base -> do
          sources <- selectMap LocationSource m
          pure $ sources & map \source -> action {abilitySource = ProxySource source base}
        ProxySource (EnemyMatcherSource m) base -> do
          sources <- selectMap EnemySource m
          pure $ sources & map \source -> action {abilitySource = ProxySource source base}
        _ -> pure [action]

  entryTicks <- getEntryTicks
  actionsMatchingWindow <-
    if null ws
      then pure actionsWithSources
      else flip filterM actionsWithSources \ability -> do
        let abWindow = case (abilitySource ability).location of
              Nothing -> abilityWindow ability
              Just lid -> replaceThisLocation lid (abilityWindow ability)
        matched <-
          anyM
            (\w -> windowMatches iid (abilitySource ability) w abWindow)
            ws
        if not matched
          then pure False
          else do
            -- A forced/reaction ability may only respond to a window that
            -- opened strictly after its source card entered play. A card that
            -- enters during an open window cannot respond to that window's
            -- already-occurred triggering condition (#4927).
            isForced <- isForcedAbility iid ability
            let isReaction = isReactionAbility ability
            if not (isForced || isReaction)
              then pure True
              else
                sourceToMaybeCard (abilitySource ability) >>= \case
                  Nothing -> pure True
                  Just card -> case lookup card.id entryTicks of
                    Nothing -> pure True
                    Just entryTick ->
                      getCurrentWindowTick <&> \case
                        Nothing -> True
                        Just openTick -> openTick > entryTick

  let bountiesOnly = BountiesOnly `elem` investigatorModifiers

  let uniqueSources = nub (map abilitySource actionsMatchingWindow)
  sourceModifierMap :: Map Source [ModifierType] <-
    fmap mapFromList $ for uniqueSources $ \src ->
      (src,) <$> getModifiers (sourceToTarget src)
  sourceClassesMap :: Map Source (Set ClassSymbol) <-
    fmap mapFromList $ for uniqueSources $ \src -> case src of
      AssetSource aid -> (src,) <$> field Field.AssetClasses aid
      _ -> pure (src, singleton Neutral)
  sourceBountyMap :: Map Source Bool <-
    fmap mapFromList $ for uniqueSources $ \src -> case src of
      EnemySource eid -> (src,) <$> (eid <=~> EnemyWithBounty)
      _ -> pure (src, True)
  let
    lookupModifiers src = findWithDefault [] src sourceModifierMap
    lookupClasses src = findWithDefault (singleton Neutral) src sourceClassesMap
    lookupBounty src = findWithDefault True src sourceBountyMap
  actions'' <-
    catMaybes <$> for actionsMatchingWindow \ability -> do
      let modifiers' = lookupModifiers (abilitySource ability)
      let cardClasses = lookupClasses (abilitySource ability)

      -- if enemy only bounty enemies
      let sourceIsBounty = lookupBounty (abilitySource ability)

      isForced <- isForcedAbility iid ability
      let
        -- Lola Hayes: Forced abilities will always trigger
        prevents (CanOnlyUseCardsInRole role) =
          null (setFromList [role, Neutral, Mythos] `intersect` cardClasses)
            && not isForced
        prevents CannotTriggerFastAbilities = isFastAbility ability
        prevents _ = False
        -- Blank excludes any non-default abilities (index >= 100)
        -- TODO: this is a leaky abstraction, we may want to track this
        -- on the abilities themselves
        blankPrevents Blank = abilityIndex ability < 100
        blankPrevents BlankExceptForcedAbilities = not isForced
        blankPrevents _ = False
        -- If the window is fast we only permit fast abilities, but forced
        -- abilities need to be everpresent so we include them
        needsToBeFast =
          ws & all \window ->
            (windowType window == Window.FastPlayerWindow)
              && not (isFastAbility ability || isForced || isReactionAbility ability)

      pure
        $ if any prevents investigatorModifiers
          || any blankPrevents modifiers'
          || needsToBeFast
          || (bountiesOnly && not sourceIsBounty)
          then Nothing
          else Just $ applyAbilityModifiers ability modifiers'

  actions''' <-
    actions'' & filterM \action -> runValidT do
      liftGuardM $ getCanPerformAbility iid ws action
      liftGuardM $ getCanAffordAbility iid action ws
  forcedActions <- filterM (isForcedAbility iid) actions'''
  -- Encounter-card forced abilities (Treachery, Enemy, Location, Agenda, Act)
  -- resolve before player-card forced abilities (Asset, Event, Skill).
  let encounterForcedActions = filter (isEncounterCardSource . abilitySource) forcedActions
  let prioritizedForcedActions =
        if null encounterForcedActions then forcedActions else encounterForcedActions
  pure $ nub $ if bountiesOnly || null forcedActions then actions''' else prioritizedForcedActions

hasFightActions
  :: (Sourceable source, Tracing m, HasGame m)
  => InvestigatorId
  -> source
  -> WindowMatcher
  -> [Window]
  -> m Bool
hasFightActions iid requestor window windows' = do
  abilities <- selectMap (setRequestor requestor) (#basic <> #fight <> AbilityWindow window)
  andM
    [ hasFightTargets (toSource requestor) iid
    , anyM (\a -> getCanPerformAbility iid windows' $ decreaseAbilityActionCost a 1) abilities
    ]

hasEvadeActions
  :: (HasCallStack, Sourceable source, Tracing m, HasGame m)
  => InvestigatorId
  -> source
  -> WindowMatcher
  -> [Window]
  -> m Bool
hasEvadeActions iid requestor window windows' = do
  abilities <- selectMap (setRequestor requestor) (#basic <> #evade <> AbilityWindow window)
  andM
    [ hasEvadeTargets (toSource requestor) iid
    , anyM (\a -> getCanPerformAbility iid windows' $ decreaseAbilityActionCost a 1) abilities
    ]

hasInvestigateActions
  :: (Sourceable source, Tracing m, HasGame m)
  => InvestigatorId
  -> source
  -> WindowMatcher
  -> [Window]
  -> m Bool
hasInvestigateActions iid requestor window windows' = do
  abilities <- selectMap (setRequestor requestor) (#basic <> #investigate <> AbilityWindow window)
  anyM (\a -> getCanPerformAbility iid windows' $ decreaseAbilityActionCost a 1) abilities

-- | Each action can count as several types (e.g. a weapon's "[action]: Fight"
-- is both an activate action and a fight action). A streak of "different types
-- of actions in a row" is therefore a system of distinct representatives (SDR):
-- one distinct type assigned per action. Input is a list of the per-action type
-- groups.

-- | The longest prefix of the (newest-first) action groups that still admits an
-- SDR, i.e. the longest run of "different types in a row".
longestUniqueStreak :: Eq a => [[a]] -> [[a]]
longestUniqueStreak = go []
 where
  go acc [] = acc
  go acc (xs : xss)
    | sdrExists (xs : acc) = go (xs : acc) xss
    | otherwise = acc

-- | Does a system of distinct representatives exist for these groups?
sdrExists :: Eq a => [[a]] -> Bool
sdrExists [] = True
sdrExists (xs : rest) = any (\x -> sdrExists (map (filter (/= x)) rest)) xs

-- | Pick one concrete SDR (a distinct representative per group), or [] if none.
pickSDR :: Eq a => [[a]] -> [a]
pickSDR = fromMaybe [] . go
 where
  go [] = Just []
  go (xs : rest) =
    listToMaybe . catMaybes $
      [fmap (x :) (go (map (filter (/= x)) rest)) | x <- xs]
