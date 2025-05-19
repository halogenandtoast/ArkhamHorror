module Arkham.Helpers.Criteria where

import Arkham.Ability.Type
import Arkham.Ability.Types
import Arkham.Act.Sequence qualified as AS
import Arkham.Act.Types (Field (..))
import Arkham.Action qualified as Action
import {-# SOURCE #-} Arkham.Asset (createAsset)
import Arkham.Asset.Types (Asset, AssetAttrs (..), Field (..))
import Arkham.Attack.Types
import Arkham.Campaigns.EdgeOfTheEarth.Partner (getPartner)
import Arkham.Capability
import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasGame
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.Query
import Arkham.Cost
import Arkham.Criteria (Criterion)
import Arkham.Criteria qualified as Criteria
import Arkham.Customization
import Arkham.Enemy.Types (Field (..))
import {-# SOURCE #-} Arkham.Entities
import Arkham.Event.Types (Event, Field (..))
import Arkham.Event.Types qualified
import {-# SOURCE #-} Arkham.Game
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers (unDeck)
import Arkham.Helpers.Ability (getCanPerformAbility)
import Arkham.Helpers.Calculation (calculate)
import Arkham.Helpers.Card (cardListMatches, getModifiedCardCost)
import Arkham.Helpers.ChaosBag (
  getRemainingBlessTokens,
  getRemainingCurseTokens,
  getRemainingFrostTokens,
 )
import Arkham.Helpers.Cost (getSpendableResources)
import Arkham.Helpers.Customization (hasCustomization)
import Arkham.Helpers.Doom (getDoomCount)
import Arkham.Helpers.GameValue (gameValueMatches)
import Arkham.Helpers.History (historyMatches)
import Arkham.Helpers.Investigator (getAsIfInHandCards)
import Arkham.Helpers.Location (getCanMoveToMatchingLocations, locationMatches)
import Arkham.Helpers.Log (getHasRecord, getRecordCount, scenarioCount)
import Arkham.Helpers.Modifiers (getModifiers, hasModifier, modified, withModifiers)
import Arkham.Helpers.Phase (matchPhase)
import Arkham.Helpers.Placement (onSameLocation)
import {-# SOURCE #-} Arkham.Helpers.Playable (getIsPlayable, getIsPlayableWithResources)
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Helpers.Scenario (getScenarioDeck, getVictoryDisplay, scenarioField, scenarioFieldMap)
import Arkham.Helpers.SkillTest (skillTestMatches)
import Arkham.Helpers.Source (sourceMatches)
import Arkham.Helpers.Tarot (affectedByTarot)
import Arkham.Helpers.Window (getPassedBy, getWindowAsset)
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Key
import Arkham.Location.Types (Field (..))
import Arkham.Matcher qualified as Matcher
import Arkham.Modifier
import Arkham.Name
import Arkham.Placement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Types (Field (..))
import Arkham.ScenarioLogKey
import Arkham.Scenarios.BeforeTheBlackThrone.Cosmos qualified as Cosmos
import Arkham.Scenarios.BeforeTheBlackThrone.Helpers (getCosmos)
import Arkham.Skill.Types (Field (..))
import Arkham.SkillTest.Base
import Arkham.Source
import Arkham.Story.Types (Field (..))
import Arkham.Target
import Arkham.Trait
import Arkham.Treachery.Types (Field (..))
import Arkham.Window (Window (..), mkWhen)
import Arkham.Window qualified as Window
import Control.Lens (over)
import Control.Monad.Reader (local)
import Control.Monad.Writer.Strict (execWriterT)
import Data.Data.Lens (biplate)
import Data.Map qualified as Map
import Data.Map.Monoidal.Strict (getMonoidalMap)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Typeable

passesCriteria
  :: (HasCallStack, HasGame m)
  => InvestigatorId
  -> Maybe (Card, CostStatus)
  -> Source
  -> Source
  -> [Window]
  -> Criterion
  -> m Bool
passesCriteria iid mcard source' requestor windows' = \case
  Criteria.CanEnterThisVehicle -> do
    mods <- getModifiers iid
    let invalidMatcher =
          let matchers = [matcher | CannotEnterVehicle matcher <- mods]
           in if null matchers then Nothing else Just (Matcher.oneOf matchers)
    case source of
      AssetSource aid -> do
        let
          go = do
            field InvestigatorPlacement iid >>= \case
              AtLocation lid -> do
                mlid' <- field AssetLocation aid
                pure $ Just lid == mlid'
              _ -> pure False
        case invalidMatcher of
          Nothing -> go
          Just matcher -> do
            invalid <- aid <=~> matcher
            if invalid then pure False else go
      _ -> error $ "Unhandled vehicle source: " <> show source
  Criteria.CanLeaveThisVehicle -> do
    case source of
      AssetSource aid -> do
        field InvestigatorPlacement iid >>= \case
          InVehicle aid' | aid == aid' -> pure True
          _ -> pure False
      _ -> error $ "Unhandled vehicle source: " <> show source
  Criteria.PartnerHasStatus cCode status -> do
    p <- getPartner cCode
    pure $ p.status == status
  Criteria.NotInEliminatedBearersThreatArea -> do
    case source of
      EnemySource eid -> do
        field EnemyBearer eid >>= \case
          Just iid' ->
            field EnemyPlacement eid >>= \case
              InThreatArea iid'' | iid' == iid'' -> iid' <=~> Matcher.UneliminatedInvestigator
              _ -> pure True
          _ -> error $ "No enemy bearer for enemy: " <> show eid
      _ -> error $ "Unhandled bearer source: " <> show source
  Criteria.InThisVehicle -> do
    case source of
      AssetSource aid -> do
        field InvestigatorPlacement iid >>= \case
          InVehicle aid' | aid == aid' -> pure True
          _ -> pure False
      _ -> error $ "Unhandled vehicle source: " <> show source
  Criteria.KeyIsSetAside key -> (elem key) <$> scenarioField ScenarioSetAsideKeys
  Criteria.UnrevealedKeyIsSetAside -> do
    let
      unrevealedKey = \case
        UnrevealedKey _ -> True
        _ -> False
    any unrevealedKey . setToList <$> scenarioField ScenarioSetAsideKeys
  Criteria.TabooCriteria tabooList cIf cElse -> do
    mtabooList <- field InvestigatorTaboo iid
    passesCriteria iid mcard source' requestor windows'
      $ if maybe False (>= tabooList) mtabooList then cIf else cElse
  Criteria.ElectrostaticDetonation -> do
    iids <- select Matcher.UneliminatedInvestigator
    groupings <- for iids \iid' -> do
      mlid <- field InvestigatorLocation iid'
      seals <- filter (\s -> s.active) . toList <$> field InvestigatorSeals iid'
      pure (iid', mlid, seals)

    let sealMap =
          foldl'
            (\acc (_, mlid, seals) -> maybe acc (\k -> Map.insertWith (<>) k seals acc) mlid)
            mempty
            groupings
    pure $ any ((> 1) . length) $ Map.elems sealMap
  Criteria.IfYouOweBiancaDieKatz -> do
    let
      isValid = \case
        (YouOweBiancaResources (Labeled _ iid') _) -> iid == iid'
        _ -> False
    any isValid <$> scenarioField ScenarioRemembered
  Criteria.OnlySources mtchr -> sourceMatches requestor mtchr
  Criteria.HasCustomization c -> do
    case mcard of
      (Just (PlayerCard card, _)) -> pure $ hasCustomization_ (cdCustomizations $ toCardDef card) card.customizations c
      _ -> case source' of
        EventSource aid -> do
          attrs <- getAttrs @Event aid
          pure $ hasCustomization attrs c
        _ -> error $ "Unhandled source: " <> show source' <> " " <> show mcard
  Criteria.ChosenCustomizationCardIsInPlay -> do
    case mcard of
      (Just (PlayerCard card, _)) -> do
        let customizations = pcCustomizations card
        let titles = [t | ChosenCard t <- concatMap snd (toList customizations)]
        selectAny
          $ Matcher.basic (Matcher.oneOf $ Matcher.CardWithTitle <$> titles)
          <> Matcher.InPlayAreaOf (Matcher.InvestigatorWithId iid)
      _ -> case source' of
        EventSource aid -> do
          customizations <- field EventCustomizations aid
          let titles = [t | ChosenCard t <- concatMap snd (toList customizations)]
          selectAny
            $ Matcher.basic (Matcher.oneOf $ Matcher.CardWithTitle <$> titles)
            <> Matcher.InPlayAreaOf (Matcher.InvestigatorWithId iid)
        _ -> error $ "Unhandled source: " <> show source' <> " " <> show mcard
  Criteria.HasTrueMagick -> do
    case source' of
      AssetSource trueMagick -> do
        attrs <- getAttrs @Asset trueMagick
        hand <- fieldMap InvestigatorHand (filterCards (card_ $ #asset <> #spell)) iid
        let
          replaceAssetId = const attrs.id
          replaceAssetIds = over biplate replaceAssetId
        let handEntities =
              map
                ( \c ->
                    overAttrs (\attrs' -> attrs {assetCardCode = assetCardCode attrs'})
                      $ createAsset c
                      $ unsafeFromCardId c.id
                )
                hand
        getGame >>= runReaderT do
          anyM
            ( \a -> do
                local (entitiesL %~ addEntity a) do
                  modifiers <- getMonoidalMap <$> execWriterT (getModifiersFor a)
                  local (modifiersL <>~ modifiers) do
                    let handAbilities = map (overCost replaceAssetIds) (getAbilities a)
                    anyM (getCanPerformAbility iid windows') handAbilities
            )
            handEntities
      _ -> error "wrong source"
  Criteria.HasCalculation c valueMatcher -> do
    value <- calculate c
    gameValueMatches value valueMatcher
  Criteria.HasRemainingFrostTokens -> (> 0) <$> getRemainingFrostTokens
  Criteria.HasRemainingBlessTokens -> (> 0) <$> getRemainingBlessTokens
  Criteria.HasRemainingCurseTokens -> (> 0) <$> getRemainingCurseTokens
  Criteria.HasNRemainingCurseTokens valueMatcher -> (`gameValueMatches` valueMatcher) =<< getRemainingCurseTokens
  Criteria.HasMoreBlessThanCurseTokens ->
    (>)
      <$> selectCount (Matcher.ChaosTokenFaceIs #bless)
      <*> selectCount (Matcher.ChaosTokenFaceIs #curse)
  Criteria.HasMoreCurseThanBlessTokens ->
    (>)
      <$> selectCount (Matcher.ChaosTokenFaceIs #curse)
      <*> selectCount (Matcher.ChaosTokenFaceIs #bless)
  Criteria.CanMoveTo matcher -> notNull <$> getCanMoveToMatchingLocations iid source matcher
  Criteria.CanMoveThis dir -> do
    case source of
      LocationSource lid -> do
        cosmos' <- getCosmos
        case Cosmos.findInCosmos lid cosmos' of
          Nothing -> pure False
          Just pos -> pure $ Cosmos.isEmpty $ Cosmos.viewCosmos (Cosmos.updatePosition pos dir) cosmos'
      _ -> error "Only works on locations"
  Criteria.ChaosTokenCountIs tokenMatcher valueMatcher -> do
    n <- selectCount tokenMatcher
    gameValueMatches n valueMatcher
  Criteria.HasHistory hType iMatcher historyMatcher -> do
    investigators <- select iMatcher
    histories <- traverse (getHistory hType) investigators
    anyM (historyMatches historyMatcher) histories
  Criteria.HasScenarioCount key valueMatcher -> do
    n <- scenarioCount key
    gameValueMatches n valueMatcher
  Criteria.HasCampaignCount key valueMatcher -> do
    n <- getRecordCount key
    gameValueMatches n valueMatcher
  Criteria.NotYetRecorded key -> do
    recorded <- getHasRecord key
    pure $ not recorded
  Criteria.HasRecord key -> getHasRecord key
  Criteria.DuringPhase phaseMatcher -> do
    p <- getPhase
    matchPhase p phaseMatcher
  Criteria.ActionCanBeUndone -> getActionCanBeUndone
  Criteria.EncounterDeckIsNotEmpty -> do
    deck <- scenarioField ScenarioEncounterDeck
    pure $ not $ null deck
  Criteria.EncounterDeckWith cardListMatcher -> do
    deck <- scenarioFieldMap ScenarioEncounterDeck (map toCard . unDeck)
    cardListMatches deck cardListMatcher
  Criteria.DoomCountIs valueMatcher -> do
    doomCount <- getDoomCount
    gameValueMatches doomCount valueMatcher
  Criteria.PlayerCountIs n -> (== n) <$> getPlayerCount
  Criteria.Negate restriction ->
    not <$> passesCriteria iid mcard source requestor windows' restriction
  Criteria.AllUndefeatedInvestigatorsResigned ->
    andM
      [ selectNone Matcher.UneliminatedInvestigator
      , selectAny Matcher.ResignedInvestigator -- at least one investigator should have resigned
      ]
  Criteria.EachUndefeatedInvestigator investigatorMatcher -> do
    uneliminated <- select Matcher.UneliminatedInvestigator
    if null uneliminated
      then pure False
      else (== uneliminated) <$> select investigatorMatcher
  Criteria.Never -> pure False
  Criteria.InYourHand -> do
    hand <-
      liftA2 (<>) (fieldMap InvestigatorHand (map toCardId) iid) (map toCardId <$> getAsIfInHandCards iid)
    case source of
      EventSource eid -> do
        mCardId <- fieldMay InHandEventCardId eid
        case mCardId of
          Nothing -> pure False
          Just cardId -> pure $ cardId `elem` hand
      SkillSource sid -> do
        mCardId <- fieldMay InHandSkillCardId sid
        case mCardId of
          Nothing -> pure False
          Just cardId -> pure $ cardId `elem` hand
      AssetSource aid -> do
        inPlay <- selectAny $ Matcher.AssetWithId aid
        if inPlay
          then pure False
          else do
            -- todo we should make a cleaner method for this
            maybe False (`elem` hand) <$> fieldMay InHandAssetCardId aid
      TreacherySource tid -> elem tid <$> select (Matcher.treacheryInHandOf iid)
      EnemySource eid -> elem eid <$> select (Matcher.enemyInHandOf iid)
      _ -> error $ "source not handled for in your hand: " <> show source
  Criteria.InYourDiscard -> do
    inSetup <- getInSetup
    if inSetup
      then pure False
      else do
        discard <- fieldMap InvestigatorDiscard (map toCardId) iid
        case source of
          AssetSource aid -> do
            inPlay <- selectAny $ Matcher.AssetWithId aid
            if inPlay
              then pure False
              else do
                -- todo we should make a cleaner method for this
                fieldMap InDiscardAssetCardId (`elem` discard) aid
          SkillSource aid -> do
            inPlay <- selectAny $ Matcher.SkillWithId aid
            if inPlay
              then pure False
              else pure $ unsafeToCardId aid `elem` discard
          InvestigatorSource _ -> case mcard of
            Just (card, _) -> pure $ toCardId card `elem` discard
            _ -> error "No card available to check"
          _ -> error $ "source not handled for in your discard: " <> show source
  Criteria.InThreatAreaOf (Matcher.replaceYouMatcher iid -> who) -> do
    case source of
      TreacherySource tid ->
        elem tid <$> select (Matcher.TreacheryInThreatAreaOf who)
      StorySource sid -> do
        placement <- field StoryPlacement sid
        case placement of
          InThreatArea iid' -> elem iid' <$> select who
          _ -> pure False
      EventSource eid -> do
        placement <- field EventPlacement eid
        case placement of
          InThreatArea iid' -> elem iid' <$> select who
          _ -> pure False
      _ ->
        error
          $ "Can not check if "
          <> show source
          <> " is in players threat area"
  Criteria.NotSetup -> not <$> getInSetup
  Criteria.Self -> case source of
    InvestigatorSource iid' -> pure $ iid == iid'
    _ -> pure False
  Criteria.ValueIs val valueMatcher -> gameValueMatches val valueMatcher
  Criteria.UnderneathCardCount valueMatcher zone cardMatcher -> do
    let
      getCards = \case
        Criteria.UnderAgendaDeck -> scenarioField ScenarioCardsUnderAgendaDeck
        Criteria.UnderActDeck -> scenarioField ScenarioCardsUnderActDeck
        Criteria.UnderZones zs -> concatMapM getCards zs
    cardCount <- length . filter (`cardMatch` cardMatcher) <$> getCards zone
    gameValueMatches cardCount valueMatcher
  Criteria.SelfHasModifier modifier -> case source of
    InvestigatorSource iid' ->
      elem modifier <$> getModifiers (InvestigatorTarget iid')
    EnemySource iid' -> elem modifier <$> getModifiers (EnemyTarget iid')
    AssetSource aid' -> elem modifier <$> getModifiers aid'
    _ -> pure False
  Criteria.Here -> case source of
    LocationSource lid -> fieldP InvestigatorLocation (== Just lid) iid
    ProxySource (LocationSource lid) _ ->
      fieldP InvestigatorLocation (== Just lid) iid
    IndexedSource _ (LocationSource lid) ->
      fieldP InvestigatorLocation (== Just lid) iid
    _ -> pure False
  Criteria.HasSupply s -> fieldP InvestigatorSupplies (elem s) iid
  Criteria.ControlsThis ->
    let
      go = \case
        ProxySource (CardIdSource _) s -> go s
        ProxySource (CardCodeSource _) s -> go s
        IndexedSource _ s -> go s
        ProxySource s _ -> go s
        AssetSource aid ->
          elem aid
            <$> select (Matcher.AssetControlledBy $ Matcher.InvestigatorWithId iid)
        EventSource eid ->
          elem eid
            <$> select (Matcher.EventControlledBy $ Matcher.InvestigatorWithId iid)
        SkillSource sid ->
          elem sid
            <$> select (Matcher.SkillControlledBy $ Matcher.InvestigatorWithId iid)
        _ -> pure False
     in
      go source
  Criteria.OwnsThis ->
    let
      go = \case
        ProxySource (CardIdSource _) s -> go s
        IndexedSource _ s -> go s
        ProxySource s _ -> go s
        AssetSource aid ->
          elem aid
            <$> select (Matcher.AssetOwnedBy $ Matcher.InvestigatorWithId iid)
        EventSource eid ->
          elem eid
            <$> select (Matcher.EventOwnedBy $ Matcher.InvestigatorWithId iid)
        SkillSource sid ->
          elem sid
            <$> select (Matcher.SkillOwnedBy $ Matcher.InvestigatorWithId iid)
        EnemySource eid ->
          elem eid
            <$> select (Matcher.EnemyOwnedBy $ Matcher.InvestigatorWithId iid)
        _ -> pure False
     in
      go source
  Criteria.DuringSkillTest skillTestMatcher -> do
    mSkillTest <- getSkillTest
    case mSkillTest of
      Nothing -> pure False
      Just skillTest -> skillTestMatches iid source skillTest skillTestMatcher
  Criteria.ChargesOnThis valueMatcher -> case source of
    TreacherySource tid ->
      (`gameValueMatches` valueMatcher) =<< field TreacheryResources tid
    _ -> error "missing ChargesOnThis check"
  Criteria.ResourcesOnThis valueMatcher -> case source of
    TreacherySource tid ->
      (`gameValueMatches` valueMatcher) =<< field TreacheryResources tid
    AssetSource aid ->
      (`gameValueMatches` valueMatcher) =<< field AssetResources aid
    LocationSource aid ->
      (`gameValueMatches` valueMatcher) =<< field LocationResources aid
    _ -> error $ "missing ResourcesOnThis check: " <> show source
  Criteria.ResourcesOnLocation locationMatcher valueMatcher -> do
    total <- getSum <$> selectAgg Sum LocationResources locationMatcher
    gameValueMatches total valueMatcher
  Criteria.CluesOnThis valueMatcher -> case source of
    LocationSource lid ->
      (`gameValueMatches` valueMatcher) =<< field LocationClues lid
    ActSource aid -> (`gameValueMatches` valueMatcher) =<< field ActClues aid
    AssetSource aid ->
      (`gameValueMatches` valueMatcher) =<< field AssetClues aid
    TreacherySource tid ->
      (`gameValueMatches` valueMatcher) =<< field TreacheryClues tid
    _ -> error "missing CluesOnThis check"
  Criteria.HorrorOnThis valueMatcher -> case source of
    AssetSource aid ->
      (`gameValueMatches` valueMatcher) =<< field AssetHorror aid
    _ -> error $ "missing HorrorOnThis check for " <> show source
  Criteria.DamageOnThis valueMatcher -> case source of
    AssetSource aid ->
      (`gameValueMatches` valueMatcher) =<< field AssetDamage aid
    _ -> error $ "missing DamageOnThis check for " <> show source
  Criteria.ScenarioDeckWithCard key -> notNull <$> getScenarioDeck key
  Criteria.Uncontrolled -> case source of
    AssetSource aid -> fieldP AssetController isNothing aid
    ProxySource (CardIdSource _) (AssetSource aid) -> fieldP AssetController isNothing aid
    ProxySource (AssetSource aid) _ -> fieldP AssetController isNothing aid
    IndexedSource _ (AssetSource aid) -> fieldP AssetController isNothing aid
    _ -> error $ "missing ControlsThis check for source: " <> show source
  Criteria.OnSameLocation -> do
    ignored <- hasModifier iid IgnoreOnSameLocation
    if ignored
      then pure True
      else do
        let
          go = \case
            StorySource sid -> onSameLocation iid =<< field StoryPlacement sid
            AssetSource aid -> onSameLocation iid =<< field AssetPlacement aid
            EnemySource eid -> onSameLocation iid =<< field EnemyPlacement eid
            TreacherySource tid -> maybe (pure False) (onSameLocation iid) =<< fieldMay TreacheryPlacement tid
            ProxySource (CardIdSource _) (AssetSource aid) -> go (AssetSource aid)
            ProxySource (CardCodeSource _) (AssetSource aid) -> go (AssetSource aid)
            ProxySource inner _ -> go inner
            IndexedSource _ inner -> go inner
            _ -> error $ "missing OnSameLocation check for source: " <> show source
        go source
  Criteria.DuringTurn (Matcher.replaceYouMatcher iid -> who) -> selectAny (Matcher.TurnInvestigator <> who)
  Criteria.CardExists cardMatcher -> selectAny cardMatcher
  Criteria.ExtendedCardExists cardMatcher ->
    case mcard of
      Just (card, _) -> selectAny (Matcher.replaceYouMatcher iid $ Matcher.replaceThisCard (toCardId card) cardMatcher)
      _ -> selectAny cardMatcher
  Criteria.CommitedCardsMatch cardListMatcher -> do
    mSkillTest <- getSkillTest
    case mSkillTest of
      Nothing -> pure False
      Just st -> cardListMatches (concat $ toList (skillTestCommittedCards st)) cardListMatcher
  Criteria.PlayableCardExistsWithCostReduction reduction cardMatcher -> do
    mTurnInvestigator <- selectOne Matcher.TurnInvestigator
    let n = case reduction of
          Criteria.Reduce x -> x
          Criteria.ReduceBySuccessAmount -> getPassedBy windows'
    let
      updatedWindows = case mTurnInvestigator of
        Nothing -> windows'
        Just tIid ->
          nub $ mkWhen (Window.DuringTurn tIid) : windows'
    availableResources <- getSpendableResources iid
    results <- select cardMatcher
    -- GameSource is important because it allows us to skip the action cost
    anyM
      ( getIsPlayableWithResources
          iid
          GameSource
          (availableResources + n)
          (UnpaidCost NoAction)
          updatedWindows
      )
      results
  Criteria.PlayableCardExists costStatus cardMatcher -> do
    mTurnInvestigator <- selectOne Matcher.TurnInvestigator
    let
      updatedWindows = case mTurnInvestigator of
        Nothing -> windows'
        Just tIid ->
          nub $ mkWhen (Window.DuringTurn tIid) : windows'
    results <- select cardMatcher
    anyM (getIsPlayable iid source' costStatus updatedWindows) results
  Criteria.PlayableCardInDiscard discardSignifier cardMatcher -> do
    let
      investigatorMatcher = case discardSignifier of
        Criteria.DiscardOf matcher -> matcher
        Criteria.AnyPlayerDiscard -> Matcher.Anyone
      windows'' =
        [ mkWhen (Window.DuringTurn iid)
        , mkWhen Window.FastPlayerWindow
        ]
    investigatorIds <-
      filterM
        ( fmap (notElem CardsCannotLeaveYourDiscardPile)
            . getModifiers
            . InvestigatorTarget
        )
        =<< select investigatorMatcher
    discards <-
      filter (`cardMatch` cardMatcher)
        <$> concatMapM (field InvestigatorDiscard) investigatorIds
    anyM (getIsPlayable iid source (UnpaidCost NoAction) windows'' . PlayerCard) discards
  Criteria.FirstAction -> fieldP InvestigatorActionsTaken null iid
  Criteria.NoRestriction -> pure True
  Criteria.OnLocation locationMatcher -> do
    ignored <- hasModifier iid IgnoreOnSameLocation
    if ignored
      then pure True
      else do
        mlid <- field InvestigatorLocation iid
        case mlid of
          Nothing -> pure False
          Just lid ->
            anyM
              (\window -> locationMatches iid source window lid locationMatcher)
              windows'
  Criteria.ReturnableCardInDiscard discardSignifier traits -> do
    let
      investigatorMatcher = case discardSignifier of
        Criteria.DiscardOf matcher -> matcher
        Criteria.AnyPlayerDiscard -> Matcher.Anyone
    investigatorIds <- select (investigatorMatcher <> can.have.cards.leaveDiscard)
    discards <- concatMapM (field InvestigatorDiscard) investigatorIds
    let
      filteredDiscards = case traits of
        [] -> discards
        traitsToMatch ->
          filter (any (`elem` traitsToMatch) . toTraits) discards
    pure $ notNull filteredDiscards
  Criteria.CanAffordCostIncrease n -> do
    let
      go :: HasGame n => Maybe (Card, CostStatus) -> n Bool
      go = \case
        Just (card, AuxiliaryCost aux inner) -> do
          withModifiers
            card
            ( modified
                GameSource
                [IncreaseCostOf (Matcher.basic $ Matcher.CardWithId card.id) $ totalResourceCost aux]
            )
            $ go (Just (card, inner))
        Just (card, UnpaidCost _) -> do
          cost <- getModifiedCardCost iid card
          resources <- getSpendableResources iid
          pure $ resources >= cost + n
        Just (_, PaidCost) -> pure True
        Nothing -> error $ "no card for CanAffordCostIncrease: " <> show source
    go mcard
  Criteria.CardInDiscard discardSignifier cardMatcher -> do
    let
      investigatorMatcher = case discardSignifier of
        Criteria.DiscardOf matcher -> matcher
        Criteria.AnyPlayerDiscard -> Matcher.Anyone
    investigatorIds <- select investigatorMatcher
    discards <- concatMapM (field InvestigatorDiscard) investigatorIds
    let filteredDiscards = filter (`cardMatch` cardMatcher) discards
    pure $ notNull filteredDiscards
  Criteria.ClueOnLocation ->
    maybe (pure False) (fmap (maybe False (> 0)) . fieldMay LocationClues)
      =<< field InvestigatorLocation iid
  Criteria.EnemyCriteria enemyCriteria ->
    passesEnemyCriteria iid source windows' enemyCriteria
  Criteria.SetAsideCardExists matcher -> selectAny (Matcher.SetAsideCardMatch matcher)
  Criteria.OutOfPlayEnemyExists outOfPlayZone matcher ->
    selectAny $ Matcher.OutOfPlayEnemy outOfPlayZone matcher
  Criteria.OnAct step -> do
    actId <- selectJust Matcher.AnyAct
    (== AS.ActStep step) . AS.actStep <$> field ActSequence actId
  Criteria.AgendaExists matcher -> selectAny matcher
  Criteria.AbilityExists matcher -> selectAny matcher
  Criteria.SkillExists matcher -> selectAny matcher
  Criteria.StoryExists matcher -> selectAny matcher
  Criteria.ActExists matcher -> selectAny matcher
  Criteria.CardWithDoomExists -> do
    orM
      [ selectAny $ Matcher.AssetWithDoom (Matcher.atLeast 1)
      , selectAny $ Matcher.InvestigatorWithDoom (Matcher.atLeast 1)
      , selectAny $ Matcher.EnemyWithDoom (Matcher.atLeast 1)
      , selectAny $ Matcher.EventWithDoom (Matcher.atLeast 1)
      , selectAny $ Matcher.LocationWithDoom (Matcher.atLeast 1)
      , selectAny $ Matcher.TreacheryWithDoom (Matcher.atLeast 1)
      , selectAny $ Matcher.AgendaWithDoom (Matcher.atLeast 1)
      ]
  Criteria.ChaosTokenExists matcher -> selectAny matcher
  Criteria.AssetExists matcher -> do
    -- N.B. Old Shotgun (2) needs to have a different uses when playing an
    -- event We add the event card to the "game" when asking for a matching
    -- asset so that ActiveEvent is set
    case mcard of
      Just (card, _) | card `cardMatch` card_ #event -> do
        g <- getGame
        let
          setPlacement :: forall a. Typeable a => a -> a
          setPlacement a = case eqT @a @Event of
            Just Refl -> overAttrs (Arkham.Event.Types.placementL .~ Limbo) a
            _ -> a
        runReaderT
          (selectAny (Matcher.replaceYouMatcher iid matcher))
          (g & entitiesL %~ (<> addCardEntityWith iid setPlacement mempty card))
      _ -> selectAny (Matcher.replaceYouMatcher iid matcher)
  Criteria.TargetExists matcher -> do
    selectAny (Matcher.replaceYouMatcher iid matcher)
  Criteria.IsReturnTo -> do
    campaign <- selectJust Matcher.TheCampaign
    pure $ "5" `T.isPrefixOf` coerce campaign
  Criteria.DifferentAssetsExist matcher1 matcher2 -> do
    m1 <- select (Matcher.replaceYouMatcher iid matcher1)
    m2 <- select (Matcher.replaceYouMatcher iid matcher2)
    case (m1, m2) of
      ([], _) -> pure False
      (_, []) -> pure False
      ([x], [y]) -> pure $ x /= y
      _ -> pure True
  Criteria.DifferentEnemiesExist matcher1 matcher2 -> do
    m1 <- select (Matcher.replaceYouMatcher iid matcher1)
    m2 <- select (Matcher.replaceYouMatcher iid matcher2)
    case (m1, m2) of
      ([], _) -> pure False
      (_, []) -> pure False
      ([x], [y]) -> pure $ x /= y
      _ -> pure True
  Criteria.EventExists matcher -> do
    selectAny (Matcher.replaceYouMatcher iid matcher)
  Criteria.EventWindowInvestigatorIs whoMatcher -> do
    windows'' :: [[Window]] <- drop 1 <$> getWindowStack
    case windows'' of
      ((windowType -> x) : _) : _ -> case x of
        Window.DrawCard iid' _ _ -> iid' <=~> Matcher.replaceYouMatcher iid whoMatcher
        _ -> pure False
      _ -> pure False
  Criteria.ExcludeWindowAssetExists matcher -> case getWindowAsset windows' of
    Nothing -> pure False
    Just aid -> do
      selectAny
        $ Matcher.NotAsset (Matcher.AssetWithId aid)
        <> Matcher.replaceYouMatcher iid matcher
  Criteria.TreacheryExists matcher -> selectAny matcher
  Criteria.InvestigatorExists matcher ->
    -- Because the matcher can't tell who is asking, we need to replace
    -- The You matcher by the Id of the investigator asking
    selectAny (Matcher.replaceYouMatcher iid matcher)
  Criteria.InvestigatorsHaveSpendableClues valueMatcher -> do
    total <-
      getSum
        <$> selectAgg
          Sum
          InvestigatorClues
          (Matcher.InvestigatorWithoutModifier CannotSpendClues)
    total `gameValueMatches` valueMatcher
  Criteria.Criteria rs -> allM (passesCriteria iid mcard source' requestor windows') rs
  Criteria.AnyCriterion rs -> anyM (passesCriteria iid mcard source' requestor windows') rs
  Criteria.LocationExists matcher -> selectAny (Matcher.replaceYouMatcher iid matcher)
  Criteria.LocationCount n matcher -> do
    (>= n) <$> selectCount (Matcher.replaceYouMatcher iid matcher)
  Criteria.AssetCount n matcher -> do
    (>= n) <$> selectCount (Matcher.replaceYouMatcher iid matcher)
  Criteria.BearerNotEliminated -> pure False
  Criteria.EnemyCount n matcher -> do
    (>= n) <$> selectCount (Matcher.replaceYouMatcher iid matcher)
  Criteria.EventCount valueMatcher matcher -> do
    n <- selectCount (Matcher.replaceYouMatcher iid matcher)
    gameValueMatches n valueMatcher
  Criteria.ExtendedCardCount n matcher ->
    (>= n) <$> selectCount matcher
  Criteria.AllLocationsMatch targetMatcher locationMatcher -> do
    targets <- select (Matcher.replaceYouMatcher iid targetMatcher)
    actual <- select (Matcher.replaceYouMatcher iid locationMatcher)
    pure $ all (`elem` actual) targets
  Criteria.InvestigatorIsAlone ->
    (== 1) <$> selectCount (Matcher.colocatedWith iid)
  Criteria.InVictoryDisplay cardMatcher valueMatcher -> do
    vCards <- filter (`cardMatch` cardMatcher) <$> getVictoryDisplay
    gameValueMatches (length vCards) valueMatcher
  Criteria.OwnCardWithDoom -> do
    anyAssetsHaveDoom <-
      selectAny
        (Matcher.AssetControlledBy Matcher.You <> Matcher.AssetWithAnyDoom)
    investigatorHasDoom <- fieldP InvestigatorDoom (> 0) iid
    pure $ investigatorHasDoom || anyAssetsHaveDoom
  Criteria.ScenarioCardHasResignAbility -> do
    actions' <- getAllAbilities
    pure $ flip
      any
      actions'
      \ability -> case abilityType ability of
        ActionAbility [Action.Resign] _ -> True
        _ -> False
  Criteria.Remembered logKey -> do
    elem logKey <$> scenarioFieldMap ScenarioRemembered Set.toList
  Criteria.RememberedAtLeast value logKeys -> do
    n <-
      length
        . filter (`elem` logKeys)
        <$> scenarioFieldMap ScenarioRemembered Set.toList
    gameValueMatches n (Matcher.AtLeast value)
  Criteria.AtLeastNCriteriaMet n criteria -> do
    m <- countM (passesCriteria iid mcard source requestor windows') criteria
    pure $ m >= n
  Criteria.DuringAction -> case mcard of
    Just (_, PaidCost) -> pure False -- If the cost is paid we're in a play action so we have to assume it is always False or it will never trigger
    _ -> getGameInAction
  Criteria.AffectedByTarot -> case source of
    TarotSource card -> affectedByTarot iid card
    _ -> pure False
 where
  source = case source' of
    AbilitySource s _ -> s
    UseAbilitySource _ s _ -> s
    _ -> source'

-- | Build a matcher and check the list
passesEnemyCriteria
  :: HasGame m
  => InvestigatorId
  -> Source
  -> [Window]
  -> Criteria.EnemyCriterion
  -> m Bool
passesEnemyCriteria iid source windows' criterion = do
  bountiesOnly <- hasModifier iid BountiesOnly
  let matcherF = if bountiesOnly then (Matcher.EnemyWithBounty <>) else id
  selectAny . matcherF . Matcher.replaceYouMatcher iid =<< matcher criterion
 where
  matcher = \case
    Criteria.EnemyMatchesCriteria ms -> mconcatMapM matcher ms
    Criteria.EnemyExists m -> pure m
    Criteria.EnemyExistsAtAttachedLocation m -> case source of
      EventSource e -> do
        fieldMap EventPlacement placementToAttached e >>= \case
          Just (LocationTarget lid) ->
            pure $ m <> Matcher.EnemyAt (Matcher.LocationWithId lid)
          Just _ -> error "Event must be attached to a location"
          Nothing -> error "Event must be attached to a location"
      _ -> error $ "Does not handle source: " <> show source
    Criteria.ThisEnemy enemyMatcher -> case source of
      EnemySource eid -> pure $ Matcher.EnemyWithId eid <> enemyMatcher
      _ -> error "Invalid source for ThisEnemy"
    Criteria.NotAttackingEnemy ->
      -- TODO: should not be multiple enemies, but if so need to OR not AND matcher
      let
        getAttackingEnemy = \case
          Window _ (Window.EnemyAttacks details) _ -> Just $ attackEnemy details
          _ -> Nothing
       in
        case mapMaybe getAttackingEnemy windows' of
          [] -> error "can not be called without enemy source"
          xs -> pure $ Matcher.NotEnemy (concatMap Matcher.EnemyWithId xs)
