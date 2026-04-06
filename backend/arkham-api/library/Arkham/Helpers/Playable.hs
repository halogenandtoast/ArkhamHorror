module Arkham.Helpers.Playable where

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
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Modifiers (
  getModifiers,
  hasModifier,
  toModifiers,
  withGrantedActions,
  withModifiers,
  withModifiersOf,
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
import Arkham.Taboo.Types
import Arkham.Tracing
import Arkham.Window
import Control.Lens (over)
import Data.Data.Lens (biplate)

getPlayableCards
  :: ( HasCallStack
     , Tracing m
     , HasGame m
     , Sourceable source
     , AsId investigator
     , IdOf investigator ~ InvestigatorId
     )
  => source -> investigator -> CostStatus -> [Window] -> m [Card]
getPlayableCards source investigator costStatus windows' = withSpan_ "getPlayableCards" do
  asIfInHandCards <- withSpan_ "getAsIfInHandCards" $ getAsIfInHandCards (asId investigator)
  otherPlayersPlayableCards <-
    withSpan_ "getOtherPlayersPlayableCards"
      $ getOtherPlayersPlayableCards (asId investigator) costStatus windows'
  playableDiscards <-
    withSpan_ "getPlayableDiscards" $ getPlayableDiscards source (asId investigator) costStatus windows'
  hand <- field InvestigatorHand (asId investigator)
  playableHandCards <-
    filterPlayable investigator source costStatus windows' (hand <> asIfInHandCards)
  pure $ playableHandCards <> playableDiscards <> otherPlayersPlayableCards

getPlayableCardsMatch
  :: ( HasCallStack
     , IsCardMatcher cardMatcher
     , HasGame m
     , Tracing m
     , Sourceable source
     , AsId investigator
     , IdOf investigator ~ InvestigatorId
     )
  => source -> investigator -> CostStatus -> [Window] -> cardMatcher -> m [Card]
getPlayableCardsMatch source investigator costStatus windows' cardMatcher =
  filterCards cardMatcher <$> getPlayableCards source investigator costStatus windows'

getPlayableDiscards
  :: (HasGame m, Tracing m, Sourceable source, AsId investigator, IdOf investigator ~ InvestigatorId)
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

filterPlayable
  :: ( HasCallStack
     , Tracing m
     , HasGame m
     , Sourceable source
     , AsId investigator
     , IdOf investigator ~ InvestigatorId
     )
  => investigator
  -> source
  -> CostStatus
  -> [Window]
  -> [Card]
  -> m [Card]
filterPlayable investigator source costStatus windows' cards = withSpan_ "filterPlayable" do
  filterM (getIsPlayable investigator source costStatus windows') cards

getIsPlayable
  :: ( HasCallStack
     , Tracing m
     , HasGame m
     , Sourceable source
     , AsId investigator
     , IdOf investigator ~ InvestigatorId
     )
  => investigator
  -> source
  -> CostStatus
  -> [Window]
  -> Card
  -> m Bool
getIsPlayable (asId -> iid) source costStatus windows' c =
  withSpan_ ("getIsPlayable[ " <> unCardCode c.cardCode <> "]") do
    availableResources <- getSpendableResources iid
    getIsPlayableWithResources iid source availableResources costStatus windows' c

withReducedCost
  :: ( Tracing m
     , HasGame m
     , Sourceable source
     , ToId investigator InvestigatorId
     )
  => investigator -> source -> Int -> ReaderT Game m a -> m a
withReducedCost (asId -> iid) source n = withModifiersOf iid source [ReduceCostOf AnyCard n]

getIsPlayableWithResources
  :: forall m source investigator
   . ( HasCallStack
     , Tracing m
     , HasGame m
     , Sourceable source
     , AsId investigator
     , IdOf investigator ~ InvestigatorId
     )
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
      base <- go
      others <-
        traverse
          (\(matcher, ctx) -> (cardMatch c matcher &&) <$> withModifiers iid (toModifiers iid ctx) go)
          (if ignoreContexts then [] else contexts)
      pure $ or (base : others)
 where
  go :: forall n. (Tracing n, HasGame n) => n Bool
  go = all (isNothing . snd) <$> getPlayabilityChecksWithResources iid source availableResources costStatus windows' c

getOtherPlayersPlayableCards
  :: (Tracing m, HasGame m) => InvestigatorId -> CostStatus -> [Window] -> m [Card]
getOtherPlayersPlayableCards iid costStatus windows' = do
  mods <- getModifiers iid
  forMaybeM mods $ \case
    PlayableCardOf _ c -> do
      playable <- getIsPlayable iid (toSource iid) costStatus windows' c
      pure $ guard playable $> c
    _ -> pure Nothing

getPlayabilityChecks
  :: ( HasCallStack
     , Tracing m
     , HasGame m
     , Sourceable source
     , AsId investigator
     , IdOf investigator ~ InvestigatorId
     )
  => investigator -> source -> CostStatus -> [Window] -> Card -> m [(Text, Maybe Text)]
getPlayabilityChecks (asId -> iid) source costStatus windows' c = do
  availableResources <- getSpendableResources iid
  getPlayabilityChecksWithResources iid source availableResources costStatus windows' c

getPlayabilityChecksWithResources
  :: forall m source
   . ( HasCallStack
     , Tracing m
     , HasGame m
     , Sourceable source
     )
  => InvestigatorId
  -> source
  -> Int
  -> CostStatus
  -> [Window]
  -> Card
  -> m [(Text, Maybe Text)]
getPlayabilityChecksWithResources _ _ _ _ _ (VengeanceCard _) =
  pure [("Card type", Just "Vengeance cards are not playable")]
getPlayabilityChecksWithResources _ _ _ _ _ (EncounterCard _) =
  pure [("Card type", Just "Encounter cards are not playable")]
getPlayabilityChecksWithResources iid (toSource -> source) availableResources costStatus windows' c@(PlayerCard _) =
  withDepthGuard 3 [] $ do
    let pcDef = toCardDef c

    -- Card type check
    let
      cardTypeOk = c.kind `notElem` [PlayerTreacheryType, PlayerEnemyType] && cdCardType pcDef /= SkillType
      cardTypeDetail = if cardTypeOk then Nothing else Just $ tshow (cdCardType pcDef) <> " cards cannot be played"

    -- Uniqueness check
    let title = nameTitle (cdName pcDef)
    uniquenessOk <- case (cdUnique pcDef, cdCardType pcDef) of
      (True, AssetType) ->
        not <$> case nameSubtitle (cdName pcDef) of
          Nothing -> selectAny (AssetWithTitle title)
          Just subtitle -> selectAny (AssetWithFullTitle title subtitle)
      _ -> pure True
    let uniquenessDetail = if uniquenessOk then Nothing else Just $ "A copy of \"" <> title <> "\" is already in play"

    -- Play restrictions check
    modifiers <- getModifiers iid
    let
      prevents (CanOnlyUseCardsInRole role) =
        null $ intersect (cdClassSymbols pcDef) (setFromList [Mythos, Neutral, role])
      prevents (CannotPlay matcher) = cardMatch c matcher
      prevents (CannotPutIntoPlay matcher) = cardMatch c matcher
      prevents _ = False
    let
      playRestrictionsOk = none prevents modifiers
      playRestrictionsDetail = if playRestrictionsOk then Nothing else Just "A modifier is preventing this card from being played"

    -- Bob Jenkins check
    isBobJenkins <- case source of
      AbilitySource (InvestigatorSource "08016") 1 -> do
        case toCardOwner c of
          Just owner ->
            andM
              [ pure $ c `cardMatch` card_ (#asset <> #item)
              , owner <=~> affectsOthers (colocatedWith @InvestigatorId "08016")
              ]
          Nothing -> pure False
      UseAbilitySource _ (InvestigatorSource "08016") 1 -> do
        case toCardOwner c of
          Just owner ->
            andM
              [ pure $ c `cardMatch` card_ (#asset <> #item)
              , owner <=~> affectsOthers (colocatedWith @InvestigatorId "08016")
              ]
          Nothing -> pure False
      _ -> pure False

    mTaboo <- field InvestigatorTaboo iid

    -- Resource cost check
    iids <- filter (/= iid) <$> getInvestigators
    iidsWithModifiers <- for iids \iid' -> (iid',) <$> getModifiers iid'
    canHelpPay <-
      iidsWithModifiers & filterM \(_iid', modifiers') -> do
        modifiers' & anyM \case
          CanSpendResourcesOnCardFromInvestigator iMatcher cMatcher -> runValidT do
            guard $ cardMatch c cMatcher
            guard $ CannotAffectOtherPlayersWithPlayerEffectsExceptDamage `notElem` modifiers'
            liftGuardM $ iid <=~> iMatcher
          _ -> pure False
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
    let
      alternateResourceCost = \case
        AlternateResourceCost cardMatcher cost | c `cardMatch` cardMatcher -> Just cost
        CanModify (AlternateResourceCost cardMatcher cost) | c `cardMatch` cardMatcher -> Just cost
        _ -> Nothing
      alternateResourceCosts = mapMaybe alternateResourceCost modifiers
    canAffordAlternateResourceCost <- case alternateResourceCosts of
      [] -> pure False
      _ -> anyM (getCanAffordCost iid source (pcDef.actions) windows') alternateResourceCosts
    mBaseModifiedCardCost <- getModifiedCardCost iid c
    mModifiedCardCost <-
      maybe (pure Nothing) (fmap Just . getPotentiallyModifiedCardCost iid c True) mBaseModifiedCardCost
    mModifiedCardCostWithChuckFergus <-
      maybe (pure Nothing) (fmap Just . getPotentiallyModifiedCardCost iid c False) mBaseModifiedCardCost
    let
      auxiliaryCosts =
        case costStatus of
          AuxiliaryCost x _ -> [x]
          _ -> []
      auxiliaryResourceCosts = totalResourceCost (mconcat auxiliaryCosts)
      totalAvailable = availableResources + additionalResources
      canAffordCost' = case mModifiedCardCost of
        Nothing -> False
        Just modifiedCardCost ->
          modifiedCardCost + auxiliaryResourceCosts <= totalAvailable
      canAffordWithChuck = case mModifiedCardCostWithChuckFergus of
        Nothing -> False
        Just modifiedCardCostWithChuckFergus ->
          modifiedCardCostWithChuckFergus + auxiliaryResourceCosts <= totalAvailable
      canAffordCost = canAffordCost' || canAffordWithChuck
      needsChuckFergus = not canAffordCost' && canAffordWithChuck
      resourceCostDetail =
        if costStatus == PaidCost || canAffordCost || canAffordAlternateResourceCost
          then Nothing
          else
            let need = maybe 0 (+ auxiliaryResourceCosts) mModifiedCardCost
             in Just $ "Need " <> tshow need <> " resources, have " <> tshow totalAvailable

    -- Criteria check
    cardModifiers <- getModifiers c
    let
      handleCriteriaReplacement _ (CanPlayWithOverride (Criteria.CriteriaOverride cOverride)) = Just cOverride
      handleCriteriaReplacement m _ = m
      resolvedCriteria =
        foldl'
          handleCriteriaReplacement
          (replaceYouMatcher iid $ over biplate (replaceThisCard' c) $ cdCriteria pcDef)
          cardModifiers
    criteriaOk <- maybe (pure True) (passesCriteria iid (Just (c, costStatus)) source (CardIdSource c.id) windows') resolvedCriteria
    criteriaDetail <- if criteriaOk
      then pure Nothing
      else case resolvedCriteria of
        Nothing -> pure (Just "No criteria defined but check failed")
        Just (Criteria.Criteria items) -> do
          failed <- filterM (\ctr -> not <$> passesCriteria iid (Just (c, costStatus)) source (CardIdSource c.id) windows' ctr) items
          pure $ Just $ "Failed: " <> mconcat (intersperse ", " (map tshow failed))
        Just ctr -> pure $ Just $ "Not met: " <> tshow ctr

    -- Play window check
    let
      goNoAction = \case
        UnpaidCost NoAction -> True
        PaidCost -> True
        AuxiliaryCost _ inner -> goNoAction inner
        _ -> False
      noAction = isNothing (cdFastWindow pcDef) && goNoAction costStatus
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
      applyModifier (ChuckFergus2Modifier cardMatcher _) _ =
        not needsChuckFergus
          && cardMatch c cardMatcher
          && (maybe True (< TabooList24) mTaboo || notFastWindow)
      applyModifier _ val = val
    inFastWindow <-
      maybe
        (pure False)
        (cardInFastWindows iid source c windows')
        (cdFastWindow pcDef <|> canBecomeFastWindow)
    let
      playWindowOk = (isNothing (cdFastWindow pcDef) && notFastWindow) || inFastWindow || isBobJenkins || noAction
      playWindowDetail = if playWindowOk then Nothing else
        if isJust (cdFastWindow pcDef)
          then Just "Fast card: not in a valid fast window"
          else Just "Not in a valid play window (not during your turn)"

    -- Limits check
    limitsOk <- passesLimits (if isBobJenkins then fromMaybe iid c.owner else iid) c
    let limitsDetail = if limitsOk then Nothing else Just "Per-round or per-game limit has been reached"

    -- Slots check
    slotsOk <- if null (cdSlots pcDef)
      then pure True
      else do
        possibleSlots <- getPotentialSlots c iid
        pure $ null $ cdSlots pcDef \\ possibleSlots
    let slotsDetail = if slotsOk then Nothing else Just $ "No available slot of type: " <> tshow (cdSlots pcDef)

    -- Evade/Fight actions check
    let isOrActions = isOrCardActions (cdActions pcDef)
    let hasEvade = #evade `elem` pcDef.actions && not (cdOverrideActionPlayableIfCriteriaMet pcDef && #evade `elem` pcDef.actions)
    attrs <- getAttrs @Investigator iid
    ac <- getActionCost attrs (pcDef.actions <> [#play | costStatus == UnpaidCost NeedsAction])
    let
      isDuringTurnWindow = \case
        (windowType -> DuringTurn iid') -> iid == iid'
        _ -> False
      doAsIfTurn = any isDuringTurnWindow windows'
    evadeOk <- if hasEvade
      then withGrantedActions iid GameSource ac do
        if inFastWindow || doAsIfTurn
          then asIfTurn iid $ hasEvadeActions iid (CardIdSource c.id) (Window.DuringTurn You) (defaultWindows iid <> windows')
          else hasEvadeActions iid (CardIdSource c.id) (Window.DuringTurn You) (defaultWindows iid <> windows')
      else pure True
    let evadeDetail = if evadeOk then Nothing else Just "No enemy at your location that can be evaded"

    -- Fight actions check (only if card has fight action)
    let hasFight = #fight `elem` pcDef.actions && not (cdOverrideActionPlayableIfCriteriaMet pcDef && #fight `elem` pcDef.actions)
    fightOk <- if hasFight
      then withGrantedActions iid GameSource ac do
        if inFastWindow || doAsIfTurn
          then asIfTurn iid $ hasFightActions iid (CardIdSource c.id) (Window.DuringTurn You) (defaultWindows iid <> windows')
          else hasFightActions iid (CardIdSource c.id) (Window.DuringTurn You) (defaultWindows iid <> windows')
      else pure True
    let fightDetail = if fightOk then Nothing else Just "No enemy at your location that can be fought"

    -- Investigate check (only if card has investigate action)
    let hasInvestigate = #investigate `elem` pcDef.actions && not isOrActions
    investigateOk <- if hasInvestigate
      then do
        mloc <- getLocationOf iid
        case mloc of
          Nothing -> pure False
          Just loc -> andM [loc <=~> InvestigatableLocation, not <$> hasModifier iid (CannotInvestigateLocation loc)]
      else pure True
    investigateDetail <- if investigateOk then pure Nothing else do
      mloc <- getLocationOf iid
      pure $ Just $ case mloc of
        Nothing -> "You are not at a location"
        Just _ -> "Your location cannot be investigated"

    -- Action cost check
    let
      goActionCost = \case
        PaidCost -> 0
        UnpaidCost NoAction -> max 0 $ subtract 1 ac
        UnpaidCost NeedsAction -> ac
        AuxiliaryCost _ inner -> goActionCost inner
      actionCost = goActionCost costStatus
    additionalCosts <-
      (modifiers <> cardModifiers) & mapMaybeM \case
        AdditionalCost n -> pure (guard (costStatus /= PaidCost) $> n)
        AdditionalPlayCostOf match additionalCost -> do
          isMatch' <- c <=~> match
          pure $ guard isMatch' $> additionalCost
        _ -> pure Nothing
    investigateCosts <- runDefaultMaybeT [] do
      guard $ case costStatus of
        UnpaidCost _ -> True
        _ -> False
      guard $ #investigate `elem` pcDef.actions
      lid <- MaybeT $ getLocationOf iid
      mods <- lift $ getModifiers lid
      pure [m | AdditionalCostToInvestigate m <- mods]
    let
      sealingToCost = \case
        Keyword.Sealing matcher -> Just $ SealCost matcher
        Keyword.SealUpTo n matcher -> Just $ UpTo (Fixed n) (SealCost matcher)
        Keyword.SealOneOf (m1 :| rest) -> Just $ OrCost $ mapMaybe sealingToCost (m1 : rest)
        Keyword.SealUpToX _ -> Nothing
      sealedChaosTokenCost = flip mapMaybe (setToList $ cdKeywords pcDef) $ \case
        Keyword.Seal sealing -> if costStatus == PaidCost then Nothing else sealingToCost sealing
        _ -> Nothing
    resignCosts <- runDefaultMaybeT [] do
      guard $ #resign `elem` pcDef.actions
      lid <- MaybeT $ field InvestigatorLocation iid
      mods <- lift $ getModifiers lid
      pure [m | AdditionalCostToResign m <- mods]
    actionCostOk <-
      getCanAffordCost_ iid (CardIdSource c.id) c.actions windows' False
        $ fold
        $ [ActionCost actionCost | actionCost > 0 && not inFastWindow && costStatus /= PaidCost]
        <> additionalCosts
        <> investigateCosts
        <> auxiliaryCosts
        <> resignCosts
        <> sealedChaosTokenCost
        <> [fromMaybe mempty (cdAdditionalCost pcDef) | costStatus /= PaidCost]
    let actionCostDetail = if actionCostOk then Nothing else Just $ "Cannot afford action cost (" <> tshow actionCost <> " action(s) required)"

    let
      checks =
        [ ("Card type", cardTypeDetail)
        , ("Uniqueness", uniquenessDetail)
        , ("Play restrictions", playRestrictionsDetail)
        , ("Resource cost", resourceCostDetail)
        , ("Criteria", criteriaDetail)
        , ("Play window", playWindowDetail)
        , ("Limits", limitsDetail)
        ]
        <> [("Slots", slotsDetail) | not (null (cdSlots pcDef))]
        <> if isOrActions && hasEvade && hasFight
             then [("Fight or Evade", if fightOk || evadeOk then Nothing else Just "No enemy that can be fought or evaded")]
             else [("Evade actions", evadeDetail) | hasEvade]
               <> [("Fight actions", fightDetail) | hasFight]
        <> [("Investigate", investigateDetail) | hasInvestigate]
        <> [("Action cost", actionCostDetail)]
    pure checks
 where
  replaceThisCard' :: Card -> Source -> Source
  replaceThisCard' c' = \case
    ThisCard -> CardCostSource (toCardId c')
    s -> s
