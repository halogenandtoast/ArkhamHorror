module Arkham.Helpers.Playable where

import Arkham.Action.Additional
import Arkham.Asset.Types (Field (..))
import Arkham.Calculation
import Arkham.Card
import Arkham.ClassSymbol
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Cost
import Arkham.Criteria qualified as Criteria
import {-# SOURCE #-} Arkham.Game
import Arkham.Helpers.Action (getActionCost, hasEvadeActions, hasFightActions)
import Arkham.Helpers.Card (
  cardInFastWindows,
  getModifiedCardCost,
  getPotentiallyModifiedCardCost,
  passesLimits,
 )
import Arkham.Helpers.Cost (getCanAffordCost, getCanAffordCost_, getSpendableResources)
import Arkham.Helpers.Criteria (passesCriteria)
import Arkham.Helpers.Game (withDepthGuard)
import Arkham.Helpers.Investigator (getAsIfInHandCards)
import Arkham.Helpers.Modifiers (
  getModifiers,
  hasModifier,
  toModifiers,
  withGrantedActions,
  withModifiers,
  withoutModifier,
 )
import Arkham.Helpers.Query (getInvestigators)
import Arkham.Helpers.Slot
import Arkham.Id
import Arkham.Investigator.Types (Field (..), Investigator, InvestigatorAttrs (..))
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Matcher.Window qualified as Window
import Arkham.Modifier
import Arkham.Name
import Arkham.Prelude
import Arkham.Projection
import Arkham.Source
import Arkham.Window
import Control.Lens (over)
import Data.Data.Lens (biplate)

getPlayableCards
  :: (HasCallStack, HasGame m, Sourceable source, AsId investigator, IdOf investigator ~ InvestigatorId)
  => source -> investigator -> CostStatus -> [Window] -> m [Card]
getPlayableCards source investigator costStatus windows' = do
  asIfInHandCards <- getAsIfInHandCards (asId investigator)
  otherPlayersPlayableCards <- getOtherPlayersPlayableCards (asId investigator) costStatus windows'
  playableDiscards <- getPlayableDiscards source (asId investigator) costStatus windows'
  hand <- field InvestigatorHand (asId investigator)
  playableHandCards <-
    filterM (getIsPlayable (asId investigator) source costStatus windows') (hand <> asIfInHandCards)
  pure $ playableHandCards <> playableDiscards <> otherPlayersPlayableCards

getPlayableDiscards
  :: (HasGame m, Sourceable source, AsId investigator, IdOf investigator ~ InvestigatorId)
  => source -> investigator -> CostStatus -> [Window] -> m [Card]
getPlayableDiscards source investigator costStatus windows' = do
  attrs <- getAttrs @Investigator (asId investigator)
  modifiers <- getModifiers (asId investigator)
  filterM
    (getIsPlayable (asId investigator) source costStatus windows')
    (possibleCards attrs modifiers)
 where
  possibleCards attrs@InvestigatorAttrs {..} modifiers =
    map (PlayerCard . snd) $ filter (canPlayFromDiscard attrs modifiers) (withIndex investigatorDiscard)
  canPlayFromDiscard attrs modifiers (n, card) =
    cdPlayableFromDiscard (toCardDef card) || any (allowsPlayFromDiscard attrs n card) modifiers
  allowsPlayFromDiscard InvestigatorAttrs {..} 0 card (CanPlayTopmostOfDiscard (mcardType, traits)) =
    let cardMatcher = maybe AnyCard CardWithType mcardType <> foldMap CardWithTrait traits
        allMatches = filter (`cardMatch` cardMatcher) investigatorDiscard
     in case allMatches of
          (topmost : _) -> topmost == card
          _ -> False
  allowsPlayFromDiscard InvestigatorAttrs {..} _ card (CanPlayFromDiscard cardMatcher) =
    let allMatches = filter (`cardMatch` cardMatcher) investigatorDiscard
     in card `elem` allMatches
  allowsPlayFromDiscard _ _ _ _ = False

getIsPlayable
  :: (HasCallStack, HasGame m, Sourceable source, AsId investigator, IdOf investigator ~ InvestigatorId)
  => investigator
  -> source
  -> CostStatus
  -> [Window]
  -> Card
  -> m Bool
getIsPlayable (asId -> iid) source costStatus windows' c = do
  availableResources <- getSpendableResources iid
  getIsPlayableWithResources iid source availableResources costStatus windows' c

getIsPlayableWithResources
  :: forall m source investigator
   . (HasCallStack, HasGame m, Sourceable source, AsId investigator, IdOf investigator ~ InvestigatorId)
  => investigator
  -> source
  -> Int
  -> CostStatus
  -> [Window]
  -> Card
  -> m Bool
getIsPlayableWithResources _ _ _ _ _ (VengeanceCard _) = pure False
getIsPlayableWithResources _ _ _ _ _ (EncounterCard _) = pure False -- TODO: there might be some playable ones?
getIsPlayableWithResources (asId -> iid) (toSource -> source) availableResources costStatus windows' c@(PlayerCard _) = do
  if c.kind `elem` [PlayerTreacheryType, PlayerEnemyType]
    then pure False
    else do
      ignoreContexts <- hasModifier iid IgnorePlayableModifierContexts
      contexts :: [(CardMatcher, [ModifierType])] <-
        concat . mapMaybe (preview _PlayableModifierContexts) <$> getModifiers iid
      base <- go @m
      others <-
        traverse
          (\(matcher, ctx) -> (cardMatch c matcher &&) <$> withModifiers iid (toModifiers iid ctx) go)
          (if ignoreContexts then [] else contexts)
      pure $ or (base : others)
 where
  pcDef = toCardDef c
  prevents (CanOnlyUseCardsInRole role) =
    null $ intersect (cdClassSymbols pcDef) (setFromList [Neutral, role])
  prevents (CannotPlay matcher) = cardMatch c matcher
  prevents (CannotPutIntoPlay matcher) = cardMatch c matcher
  prevents _ = False
  go :: forall n. HasGame n => n Bool
  go = withDepthGuard 3 False do
    attrs <- getAttrs @Investigator iid
    isBobJenkins <- case source of
      AbilitySource (InvestigatorSource "08016") 1 -> do
        case toCardOwner c of
          Just owner ->
            andM
              [ pure $ c `cardMatch` card_ (#asset <> #item)
              , pure
                  $ BobJenkinsAction
                  `notElem` map additionalActionType (investigatorUsedAdditionalActions attrs)
              , owner <=~> affectsOthers (colocatedWith @InvestigatorId "08016")
              ]
          Nothing -> pure False
      UseAbilitySource _ (InvestigatorSource "08016") 1 -> do
        case toCardOwner c of
          Just owner ->
            andM
              [ pure $ c `cardMatch` card_ (#asset <> #item)
              , pure
                  $ BobJenkinsAction
                  `notElem` map additionalActionType (investigatorUsedAdditionalActions attrs)
              , owner <=~> affectsOthers (colocatedWith @InvestigatorId "08016")
              ]
          Nothing -> pure False
      _ -> pure False
    iids <- filter (/= iid) <$> getInvestigators
    iidsWithModifiers <- for iids $ \iid' -> (iid',) <$> getModifiers iid'
    canHelpPay <- flip filterM iidsWithModifiers \(iid', modifiers') -> do
      bobJenkinsPermitted <-
        if isBobJenkins
          then do
            case toCardOwner c of
              Just owner | owner == iid' -> pure True
              _ -> pure False
          else pure False
      modifierPermitted <- flip anyM modifiers' $ \case
        CanSpendResourcesOnCardFromInvestigator iMatcher cMatcher
          | cardMatch c cMatcher && CannotAffectOtherPlayersWithPlayerEffectsExceptDamage `notElem` modifiers' ->
              iid <=~> iMatcher
        _ -> pure False
      pure $ bobJenkinsPermitted || modifierPermitted

    modifiers <- getModifiers iid
    resourcesFromAssets <-
      sum <$> for ((iid, modifiers) : iidsWithModifiers) \(iid', modifiers') -> do
        sum <$> for modifiers' \case
          CanSpendUsesAsResourceOnCardFromInvestigator assetId uType iMatcher cMatcher | cardMatch c cMatcher -> do
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
    cardModifiers <- getModifiers c
    let title = nameTitle (cdName pcDef)
    passesUnique <- case (cdUnique pcDef, cdCardType pcDef) of
      (True, AssetType) ->
        not <$> case nameSubtitle (cdName pcDef) of
          Nothing -> selectAny (AssetWithTitle title)
          Just subtitle -> selectAny (AssetWithFullTitle title subtitle)
      _ -> pure True

    baseModifiedCardCost <- getModifiedCardCost iid c
    modifiedCardCost <- getPotentiallyModifiedCardCost iid c True baseModifiedCardCost
    modifiedCardCostWithChuckFergus <- getPotentiallyModifiedCardCost iid c False baseModifiedCardCost

    let
      alternateResourceCost = \case
        AlternateResourceCost cardMatcher cost | c `cardMatch` cardMatcher -> Just cost
        CanModify (AlternateResourceCost cardMatcher cost) | c `cardMatch` cardMatcher -> Just cost
        _ -> Nothing
      alternateResourceCosts = mapMaybe alternateResourceCost modifiers

    canAffordAlternateResourceCost <- case alternateResourceCosts of
      [] -> pure False
      _ -> anyM (getCanAffordCost iid source (cdActions $ toCardDef c) windows') alternateResourceCosts

    let
      auxiliaryCosts =
        case costStatus of
          AuxiliaryCost x _ -> [x]
          _ -> []

      auxiliaryResourceCosts = totalResourceCost (mconcat auxiliaryCosts)
      replaceThisCard' :: Card -> Source -> Source
      replaceThisCard' c' = \case
        ThisCard -> CardCostSource (toCardId c')
        s -> s
      replaceThisCardSource :: Data a => a -> a
      replaceThisCardSource = over biplate (replaceThisCard' c)
      canAffordCost' = modifiedCardCost + auxiliaryResourceCosts <= availableResources + additionalResources
      canAffordCost =
        if canAffordCost'
          then canAffordCost'
          else modifiedCardCostWithChuckFergus + auxiliaryResourceCosts
              <= availableResources + additionalResources
      needsChuckFergus = not canAffordCost' && canAffordCost
      handleCriteriaReplacement _ (CanPlayWithOverride (Criteria.CriteriaOverride cOverride)) = Just cOverride
      handleCriteriaReplacement m _ = m
      duringTurnWindow' = mkWhen (DuringTurn iid)
      notFastWindow = duringTurnWindow' `elem` windows'
      canBecomeFast =
        CannotPlay FastCard
          `notElem` modifiers
          && foldr applyModifier False (nub $ cardModifiers <> modifiers)
      canBecomeFastWindow =
        guard canBecomeFast
          $> fromMaybe
            (Window.DuringTurn You)
            (listToMaybe [w | BecomesFast w <- cardModifiers <> modifiers])
      applyModifier (BecomesFast _) _ = True
      applyModifier (CanBecomeFast cardMatcher) _ = cardMatch c cardMatcher
      applyModifier (ChuckFergus2Modifier cardMatcher _) _ = not needsChuckFergus && cardMatch c cardMatcher
      applyModifier _ val = val
      source' = replaceThisCardSource source

    passesCriterias <-
      maybe
        (pure True)
        (passesCriteria iid (Just (c, costStatus)) source' (CardIdSource c.id) windows')
        (foldl' handleCriteriaReplacement (replaceThisCardSource $ cdCriteria pcDef) cardModifiers)

    inFastWindow <-
      maybe
        (pure False)
        (cardInFastWindows iid source c windows')
        (cdFastWindow pcDef <|> canBecomeFastWindow)

    ac <- getActionCost attrs (cdActions pcDef)

    let
      isDuringTurnWindow = \case
        (windowType -> DuringTurn iid') -> iid == iid'
        _ -> False
      doAsIfTurn = any isDuringTurnWindow windows'

    canEvade <- withGrantedActions iid GameSource ac do
      if #evade `elem` cdActions pcDef
        then
          if inFastWindow || doAsIfTurn
            then
              asIfTurn iid
                $ hasEvadeActions iid (Window.DuringTurn You) (defaultWindows iid <> windows')
            else hasEvadeActions iid (Window.DuringTurn You) (defaultWindows iid <> windows')
        else pure False

    canFight <- withGrantedActions iid GameSource ac do
      if #fight `elem` pcDef.actions
        then
          if inFastWindow || doAsIfTurn
            then
              asIfTurn iid
                $ hasFightActions
                  iid
                  (CardIdSource c.id)
                  (Window.DuringTurn You)
                  (defaultWindows iid <> windows')
            else
              hasFightActions
                iid
                (CardIdSource c.id)
                (Window.DuringTurn You)
                (defaultWindows iid <> windows')
        else pure False

    canInvestigate <- runValidT do
      guard $ #investigate `elem` pcDef.actions
      loc <- MaybeT $ field InvestigatorLocation iid
      liftGuardM $ loc <=~> InvestigatableLocation
      liftGuardM $ withoutModifier iid (CannotInvestigateLocation loc)

    passesLimits' <- passesLimits iid c

    let
      isPlayAction = \case
        PaidCost -> False
        UnpaidCost NoAction -> False
        UnpaidCost NeedsAction -> True
        AuxiliaryCost _ inner -> isPlayAction inner

    -- N.B. We're checking if the cost is paid here and ignoring the additional cost
    -- not sure if this is correct, but going to see if any new issues come up (25/11/2)
    additionalCosts <-
      flip mapMaybeM (modifiers <> cardModifiers) \case
        AdditionalCost n -> pure (guard (costStatus /= PaidCost) $> n)
        AdditionalPlayCostOf match additionalCost -> do
          isMatch' <- c <=~> match
          pure $ guard isMatch' $> additionalCost
        AdditionalActionCostOf match n -> do
          performedActions <- field InvestigatorActionsPerformed iid
          takenActions <- field InvestigatorActionsTaken iid
          let cardActions = if isPlayAction costStatus then #play : c.actions else c.actions
          pure
            $ guard
              (costStatus /= PaidCost && any (matchTarget takenActions performedActions match) cardActions)
            $> ActionCost n
        _ -> pure Nothing

    let
      sealingToCost = \case
        Keyword.Sealing matcher -> Just $ SealCost matcher
        Keyword.SealUpTo n matcher -> Just $ UpTo (Fixed n) (SealCost matcher)
        Keyword.SealOneOf (m1 :| rest) -> Just $ OrCost $ mapMaybe sealingToCost (m1 : rest)
        Keyword.SealUpToX _ -> Nothing
      sealedChaosTokenCost = flip mapMaybe (setToList $ cdKeywords pcDef) $ \case
        Keyword.Seal sealing -> if costStatus == PaidCost then Nothing else sealingToCost sealing
        _ -> Nothing

    investigateCosts <-
      if #investigate `elem` cdActions pcDef
        then
          field InvestigatorLocation iid >>= \case
            Nothing -> pure []
            Just lid -> do
              mods <- getModifiers lid
              pure [m | AdditionalCostToInvestigate m <- mods]
        else pure []

    resignCosts <-
      if #resign `elem` cdActions pcDef
        then do
          mLocation <- field InvestigatorLocation iid
          case mLocation of
            Nothing -> pure []
            Just lid -> do
              mods <- getModifiers lid
              pure [m | AdditionalCostToResign m <- mods]
        else pure []

    -- NOTE: This used to be
    -- PaidCost -> pure . max 0 . subtract 1
    -- But we removed it because it conflicted with additional action costs
    let
      goActionCost = \case
        PaidCost -> 0
        UnpaidCost NoAction -> max 0 $ subtract 1 ac
        UnpaidCost NeedsAction -> ac
        AuxiliaryCost _ inner -> goActionCost inner
      actionCost = goActionCost costStatus

    -- NOTE: WE just changed this to pass False for can modify We may want to
    -- consolidate this in a way where this is covered by the default case
    canAffordAdditionalCosts <-
      getCanAffordCost_ iid (CardIdSource c.id) c.actions windows' False
        $ fold
        $ [ActionCost actionCost | actionCost > 0 && not inFastWindow && costStatus /= PaidCost]
        <> additionalCosts
        <> auxiliaryCosts
        <> investigateCosts
        <> resignCosts
        <> sealedChaosTokenCost
        <> [fromMaybe mempty (cdAdditionalCost pcDef) | costStatus /= PaidCost]

    passesSlots <-
      if null (cdSlots pcDef)
        then pure True
        else do
          possibleSlots <- getPotentialSlots c iid
          pure $ null $ cdSlots pcDef \\ possibleSlots

    let
      goNoAction = \case
        UnpaidCost NoAction -> True
        PaidCost ->
          -- NOTE: This addition is recent and is related to changes with
          -- windows. Because we pass all windows now it confuses the logic for
          -- playing a card outside of a normal window, but my choice here is
          -- that if we've PaidCost then we should not care.
          True
        AuxiliaryCost _ inner -> goNoAction inner
        _ -> False
      noAction = isNothing (cdFastWindow pcDef) && goNoAction costStatus

    pure
      $ (cdCardType pcDef /= SkillType)
      && ((costStatus == PaidCost) || (canAffordCost || canAffordAlternateResourceCost))
      && none prevents modifiers
      && ((isNothing (cdFastWindow pcDef) && notFastWindow) || inFastWindow || isBobJenkins || noAction)
      && ( (#evade `notElem` pcDef.actions)
             || canEvade
             || (cdOverrideActionPlayableIfCriteriaMet pcDef && #evade `elem` cdActions pcDef)
         )
      && ( (#fight `notElem` pcDef.actions)
             || canFight
             || (cdOverrideActionPlayableIfCriteriaMet pcDef && #fight `elem` cdActions pcDef)
         )
      && ((#investigate `notElem` cdActions pcDef) || canInvestigate)
      && passesCriterias
      && passesLimits'
      && passesUnique
      && passesSlots
      && canAffordAdditionalCosts

getOtherPlayersPlayableCards :: HasGame m => InvestigatorId -> CostStatus -> [Window] -> m [Card]
getOtherPlayersPlayableCards iid costStatus windows' = do
  mods <- getModifiers iid
  forMaybeM mods $ \case
    PlayableCardOf _ c -> do
      playable <- getIsPlayable iid (toSource iid) costStatus windows' c
      pure $ guard playable $> c
    _ -> pure Nothing
