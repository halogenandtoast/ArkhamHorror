module Arkham.Helpers.Action where

import Arkham.Ability hiding (NoRestriction)
import Arkham.Action
import Arkham.Action.Additional
import Arkham.Asset.Types qualified as Field
import Arkham.Card
import Arkham.ClassSymbol
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.GameEnv (getAllAbilities)
import Arkham.Helpers.Ability (getCanAffordAbility, getCanPerformAbility, isForcedAbility)
import Arkham.Helpers.Modifiers (
  ModifierType (..),
  getModifiers,
  withGrantedAction,
  withModifiersOf,
 )
import {-# SOURCE #-} Arkham.Helpers.Playable (getPlayableCards)
import Arkham.Helpers.Ref (sourceToTarget)
import Arkham.Helpers.Source (sourceTraits)
import Arkham.Id
import Arkham.Investigator.Types (Field (..), Investigator, InvestigatorAttrs (..))
import Arkham.Matcher.Ability
import Arkham.Matcher.Action
import Arkham.Matcher.Card
import Arkham.Matcher.Enemy
import Arkham.Matcher.Window
import Arkham.Prelude
import Arkham.Projection
import Arkham.Source
import Arkham.Target
import Arkham.Window (Window (..), defaultWindows)
import Arkham.Window qualified as Window

actionMatches :: HasGame m => InvestigatorId -> Action -> ActionMatcher -> m Bool
actionMatches _ _ AnyAction = pure True
actionMatches _ a (ActionIs a') = pure $ a == a'
actionMatches iid a (ActionMatches as) = allM (actionMatches iid a) as
actionMatches iid a (ActionOneOf as) = anyM (actionMatches iid a) as
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
  canDraw <- canDo iid #draw
  canTakeResource <- canDo iid #resource
  canPlay <- canDo iid #play
  pure
    $ or
      [ notNull available
      , canAffordTakeResources && canTakeResource && a == #resource
      , canAffordDrawCards && canDraw && a == #draw
      , canPlay && notNull playableCards && a == #play
      ]

canDo :: HasGame m => InvestigatorId -> Action -> m Bool
canDo iid action = do
  mods <- getModifiers iid
  let
    prevents = \case
      CannotTakeAction x -> preventsAction x
      MustTakeAction x -> not <$> preventsAction x -- reads a little weird but we want only thing things x would prevent with cannot take action
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
  :: HasGame m => Source -> [Action] -> AdditionalAction -> m Bool
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

getCanAfford :: HasGame m => InvestigatorAttrs -> [Action] -> m Bool
getCanAfford a@InvestigatorAttrs {..} as = do
  actionCost <- getActionCost a as
  additionalActions <- getAdditionalActions a
  additionalActionCount <-
    countM
      (\aa -> anyM (\ac -> additionalActionCovers (toSource a) [ac] aa) as)
      additionalActions
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

  pure $ filter (`notElem` investigatorUsedAdditionalActions attrs) additionalActions

getActionCost :: HasGame m => InvestigatorAttrs -> [Action] -> m Int
getActionCost attrs as = do
  modifiers <- getModifiers (toTarget attrs)
  pure $ foldr applyModifier 1 modifiers
 where
  applyModifier (ActionCostOf match m) n =
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

getActions :: (HasGame m, HasCallStack) => InvestigatorId -> [Window] -> m [Ability]
getActions iid ws = getActionsWith iid ws id

getActionsWith
  :: (HasCallStack, HasGame m)
  => InvestigatorId
  -> [Window]
  -> (Ability -> Ability)
  -> m [Ability]
getActionsWith iid ws f = do
  modifiersForFilter <- getModifiers iid
  let
    abilityFilters =
      mapMaybe
        ( \case
            CannotTriggerAbilityMatching m -> Just (TriggeredAbility <> m)
            _ -> Nothing
        )
        modifiersForFilter

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
          pure
            $ map
              (\source -> action {abilitySource = ProxySource source base})
              sources
        ProxySource (ActMatcherSource m) base -> do
          sources <- selectMap ActSource m
          pure
            $ map
              (\source -> action {abilitySource = ProxySource source base})
              sources
        ProxySource (AssetMatcherSource m) base -> do
          sources <- selectMap AssetSource m
          pure
            $ map
              (\source -> action {abilitySource = ProxySource source base})
              sources
        ProxySource (LocationMatcherSource m) base -> do
          sources <- selectMap LocationSource m
          pure
            $ map
              (\source -> action {abilitySource = ProxySource source base})
              sources
        ProxySource (EnemyMatcherSource m) base -> do
          sources <- selectMap EnemySource m
          pure
            $ map
              (\source -> action {abilitySource = ProxySource source base})
              sources
        _ -> pure [action]

  actions'' <-
    catMaybes <$> for
      actionsWithSources
      \ability -> do
        modifiers' <- getModifiers (sourceToTarget $ abilitySource ability)
        investigatorModifiers <- getModifiers (InvestigatorTarget iid)
        let bountiesOnly = BountiesOnly `elem` investigatorModifiers
        cardClasses <- case abilitySource ability of
          AssetSource aid -> field Field.AssetClasses aid
          _ -> pure $ singleton Neutral

        -- if enemy only bounty enemies
        sourceIsBounty <- case abilitySource ability of
          EnemySource eid -> eid <=~> EnemyWithBounty
          _ -> pure True

        isForced <- isForcedAbility iid ability
        let
          -- Lola Hayes: Forced abilities will always trigger
          prevents (CanOnlyUseCardsInRole role) =
            null (setFromList [role, Neutral] `intersect` cardClasses)
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
            all
              ( \window ->
                  windowType window
                    == Window.FastPlayerWindow
                    && not
                      ( isFastAbility ability
                          || isForced
                          || isReactionAbility ability
                      )
              )
              ws

        pure
          $ if any prevents investigatorModifiers
            || any blankPrevents modifiers'
            || needsToBeFast
            || (bountiesOnly && not sourceIsBounty)
            then Nothing
            else Just $ applyAbilityModifiers ability modifiers'


  actions''' <-
    filterM
      ( \action -> do
          andM
            [ getCanPerformAbility iid ws action
            , getCanAffordAbility iid action ws
            ]
      )
      actions''
  forcedActions <- filterM (isForcedAbility iid) actions'''
  pure $ nub $ if null forcedActions then actions''' else forcedActions

hasFightActions
  :: (Sourceable source, HasGame m)
  => InvestigatorId
  -> source
  -> WindowMatcher
  -> [Window]
  -> m Bool
hasFightActions iid requestor window windows' =
  anyM (\a -> getCanPerformAbility iid windows' $ decreaseAbilityActionCost a 1)
    . map (setRequestor requestor)
    =<< select (BasicAbility <> AbilityIsAction #fight <> AbilityWindow window)

hasEvadeActions
  :: (HasCallStack, HasGame m)
  => InvestigatorId
  -> WindowMatcher
  -> [Window]
  -> m Bool
hasEvadeActions iid window windows' =
  anyM (\a -> getCanPerformAbility iid windows' $ decreaseAbilityActionCost a 1)
    =<< select (AbilityIsAction #evade <> AbilityWindow window)
