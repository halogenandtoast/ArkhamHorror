{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Investigator.Runner (module Arkham.Investigator.Runner, module X) where

import Arkham.Prelude

import Arkham.ClassSymbol as X
import Arkham.Classes as X
import Arkham.Investigator.Attrs as X
import Arkham.Name as X
import Arkham.Stats as X
import Arkham.Token as X
import Arkham.Trait as X hiding (Cultist)

import Arkham.Ability
import Arkham.Action (Action)
import Arkham.Action qualified as Action
import Arkham.Asset.Uses (UseType)
import Arkham.LocationSymbol
import Arkham.ScenarioLogKey
import Arkham.SkillTest
import Arkham.SkillType
import Arkham.Card
import Arkham.Card.EncounterCard
import Arkham.Card.Id
import Arkham.Card.PlayerCard
import Arkham.CommitRestriction
import Arkham.Cost
import Arkham.DamageEffect
import Arkham.Deck
import Arkham.Direction
import Arkham.Game.Helpers hiding (windows)
import Arkham.Game.Helpers qualified as Helpers
import Arkham.Helpers
import Arkham.Id
import Arkham.Keyword
import Arkham.Matcher
  ( AssetMatcher(..)
  , CardMatcher(..)
  , EnemyMatcher(..)
  , EventMatcher
  , InvestigatorMatcher(..)
  , LocationMatcher(..)
  , SkillMatcher
  , assetIs
  )
import Arkham.Message
import Arkham.Modifier
import Arkham.Query
import Arkham.Slot
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Window (Window(..))
import Arkham.Window qualified as Window
import Arkham.Zone (Zone)
import Arkham.Zone qualified as Zone
import Data.HashMap.Strict qualified as HashMap
import Data.Monoid

type InvestigatorRunner env
  = ( HasQueue env
    , CanBeWeakness env TreacheryId
    , CanCheckPlayable env
    , HasTokenValue env ()
    , Query AssetMatcher env
    , Query InvestigatorMatcher env
    , HasCount UsesCount env (AssetId, UseType)
    , HasList SlotType env AssetId
    , HasAbilities env
    , ( HasCount ActionTakenCount env InvestigatorId
      , HasCount ActionRemainingCount env InvestigatorId
      , HasCount
          ActionRemainingCount
          env
          (Maybe Action, [Trait], InvestigatorId)
      , HasCount CardCount env InvestigatorId
      , HasCount ClueCount env InvestigatorId
      , HasCount ClueCount env LocationId
      , HasCount DamageCount env InvestigatorId
      , HasCount DiscardCount env InvestigatorId
      , HasCount DoomCount env AssetId
      , HasCount DoomCount env InvestigatorId
      , HasCount FightCount env EnemyId
      , HasCount HealthDamageCount env EnemyId
      , HasCount HorrorCount env InvestigatorId
      , HasCount PlayerCount env ()
      , HasCount RemainingSanity env InvestigatorId
      , HasCount RemainingSanity env AssetId
      , HasCount RemainingHealth env AssetId
      , HasCount ResourceCount env InvestigatorId
      , HasCount SanityDamageCount env EnemyId
      , HasCount SetAsideCount env CardCode
      , HasCount Shroud env LocationId
      , HasCount SpendableClueCount env InvestigatorId
      , HasCount SpendableClueCount env ()
      , HasCount UsesCount env AssetId
      )
    , ( HasId (Maybe AssetId) env CardCode
      , HasId (Maybe LocationId) env (Direction, LocationId)
      , HasId (Maybe LocationId) env AssetId
      , HasId (Maybe LocationId) env LocationMatcher
      , HasId ActiveInvestigatorId env ()
      , HasId CardCode env AssetId
      , HasId CardCode env EnemyId
      , GetCardDef env LocationId
      , HasId LeadInvestigatorId env ()
      , HasId LocationId env InvestigatorId
      )
    , ( HasList CommittedCard env InvestigatorId
      , HasList CommittedSkillIcon env InvestigatorId
      , HasList DeckCard env InvestigatorId
      , HasList DiscardedEncounterCard env ()
      , HasList DiscardableHandCard env InvestigatorId
      , HasList DiscardedPlayerCard env InvestigatorId
      , HasList HandCard env InvestigatorId
      , HasList InPlayCard env InvestigatorId
      , HasList LocationName env ()
      , HasList UnderneathCard env InvestigatorId
      , HasList UsedAbility env ()
      , HasList SetAsideCard env ()
      )
    , HasModifiersFor env ()
    , (HasName env AssetId, HasName env LocationId)
    , HasPlayerCard env AssetId
    , HasPlayerCard env EventId
    , HasRecord env ()
    , ( ( HasSet AccessibleLocationId env LocationId
        , HasSet ActId env TreacheryCardCode
        , HasSet ActId env ()
        , HasSet AgendaId env ()
        , HasSet AgendaId env TreacheryCardCode
        , HasSet ClassSymbol env InvestigatorId
        , HasSet EventId env EventMatcher
        , HasSet SkillId env SkillMatcher
        , Query AssetMatcher env
        , HasSet BlockedLocationId env ()
        , HasSet ClosestEnemyId env (LocationId, [Trait])
        , HasSet ClosestLocationId env (LocationId, [Trait])
        , HasSet ClosestLocationId env (InvestigatorId, LocationMatcher)
        , HasSet ClosestPathLocationId env (LocationId, LocationId)
        , HasSet
            ClosestPathLocationId
            env
            (LocationId, LocationId, HashMap LocationId [LocationId])
        , HasSet CommittedCardCode env ()
        , HasSet CommittedCardId env InvestigatorId
        , HasSet CommittedSkillId env InvestigatorId
        , HasSet ConnectedLocationId env LocationId
        , HasSet EmptyLocationId env ()
        , HasSet EnemyAccessibleLocationId env (EnemyId, LocationId)
        , HasSet EnemyId env CardCode
        , HasSet EnemyId env InvestigatorId
        , HasSet EnemyId env LocationId
        , HasSet EnemyId env Trait
        , HasSet EnemyId env ()
        , HasSet EnemyId env ([Trait], LocationId)
        , HasSet EnemyId env EnemyMatcher
        , HasSet EventId env ()
        , HasSet FarthestEnemyId env (InvestigatorId, EnemyTrait)
        , HasSet FarthestLocationId env InvestigatorId
        , HasSet FarthestLocationId env (InvestigatorId, LocationMatcher)
        , HasSet FarthestLocationId env [InvestigatorId]
        , HasSet FightableEnemyId env (InvestigatorId, Source)
        , HasSet HandCardId env (InvestigatorId, CardType)
        , HasSet HandCardId env InvestigatorId
        , HasSet InScenarioInvestigatorId env ()
        )
      , ( HasSet InvestigatorId env ()
        , HasSet InvestigatorId env EnemyId
        , HasSet InvestigatorId env LocationId
        , HasSet InvestigatorId env TreacheryCardCode
        , HasSet InvestigatorId env (HashSet LocationId)
        , HasSet Keyword env EnemyId
        , HasSet LocationId env LocationMatcher
        , HasSet LocationId env TreacheryCardCode
        , HasSet LocationId env (HashSet LocationSymbol)
        , HasSet LocationId env [Trait]
        , HasSet LocationId env ()
        , HasSet RevealedLocationId env ()
        , HasSet ScenarioLogKey env ()
        , HasSet Trait env AssetId
        , HasSet Trait env EnemyId
        , HasSet Trait env LocationId
        , HasSet Trait env Source
        , HasSet Trait env (InvestigatorId, CardId)
        , HasSet TreacheryId env LocationId
        , HasSet UnrevealedLocationId env ()
        , HasSet UnrevealedLocationId env LocationMatcher
        )
      )
    , (HasStep ActStep env (), HasStep AgendaStep env ())
    , HasSkillTest env
    , GetCardDef env EnemyId
    )

instance InvestigatorRunner env => RunMessage env InvestigatorAttrs where
  runMessage = runInvestigatorMessage

runInvestigatorMessage
  :: ( InvestigatorRunner env
     , MonadReader env m
     , MonadRandom m
     , MonadIO m
     , HasGameLogger env
     )
  => Message
  -> InvestigatorAttrs
  -> m InvestigatorAttrs
runInvestigatorMessage msg a@InvestigatorAttrs {..} = case msg of
  ResetGame ->pure $ (cbCardBuilder (investigator id (toCardDef a) (getAttrStats a)) ())
    { investigatorXp = investigatorXp
    , investigatorPhysicalTrauma = investigatorPhysicalTrauma
    , investigatorMentalTrauma = investigatorMentalTrauma
    , investigatorSanityDamage = investigatorMentalTrauma
    , investigatorHealthDamage = investigatorPhysicalTrauma
    , investigatorStartsWith = investigatorStartsWith
    }
  SetupInvestigators -> do
    let
      (startsWithMsgs, deck') = foldl'
        (\(msgs, currentDeck) cardDef ->
          let
            (before, after) =
              break ((== cardDef) . toCardDef) (unDeck currentDeck)
          in
            case after of
              (card : rest) ->
                ( PutCardIntoPlay investigatorId (PlayerCard card) Nothing
                  : msgs
                , Deck (before <> rest)
                )
              _ ->
                error
                  $ "Did not find starting card "
                  <> show (toName cardDef)
                  <> " in deck"
        )
        ([], investigatorDeck)
        investigatorStartsWith
      (permanentCards, deck'') =
        partition (cdPermanent . toCardDef) (unDeck deck')
    pushAll
      $ startsWithMsgs
      <> [ PutCardIntoPlay investigatorId (PlayerCard card) Nothing
         | card <- permanentCards
         ]
      <> [DrawStartingHand investigatorId, TakeStartingResources investigatorId]
    pure $ a & (deckL .~ Deck deck'')
  DrawStartingHand iid | iid == investigatorId -> do
    let (discard, hand, deck) = drawOpeningHand a 5
    pure $ a & (discardL .~ discard) & (handL .~ hand) & (deckL .~ Deck deck)
  ReturnToHand iid (CardIdTarget cid) | iid == investigatorId -> do
    -- Card is assumed to be in your discard
    -- but since find card can also return cards in your hand
    -- we filter again just in case
    let
      card = findCard cid a
      discardFilter = case preview _PlayerCard card of
        Just pc -> filter (/= pc)
        Nothing -> id
    pure
      $ a
      & (discardL %~ discardFilter)
      & (handL %~ filter (/= card))
      & (handL %~ (card :))
  CheckAdditionalActionCosts iid _ source action msgs | iid == investigatorId ->
    do
      modifiers' <- getModifiers source (toTarget a)
      let
        additionalCosts = mapMaybe
          (\case
            ActionCostOf (IsAction action') n | action == action' ->
              Just (ActionCost n)
            _ -> Nothing
          )
          modifiers'
      a <$ if null additionalCosts
        then pushAll msgs
        else do
          canPay <- getCanAffordCost
            iid
            (toSource a)
            Nothing
            [Window Timing.When Window.NonFast]
            (mconcat additionalCosts)
          when
            canPay
            (pushAll
            $ [ CreatePayAbilityCostEffect
                  (abilityEffect a $ mconcat additionalCosts)
                  (toSource a)
                  (toTarget a)
                  []
              ]
            <> msgs
            )
  TakeStartingResources iid | iid == investigatorId -> do
    modifiers' <- getModifiers (toSource a) (toTarget a)
    let
      startingResources = foldl'
        (\total -> \case
          StartingResources n -> max 0 (total + n)
          _ -> total
        )
        5
        modifiers'
    pure $ a & resourcesL .~ startingResources
  InvestigatorMulligan iid | iid == investigatorId -> a <$ push
    (if null investigatorHand
      then FinishedWithMulligan investigatorId
      else
        chooseOne iid
        $ Run
            [Continue "Done With Mulligan", FinishedWithMulligan investigatorId]
        : [ Run [DiscardCard iid (toCardId card), InvestigatorMulligan iid]
          | card <- investigatorHand
          ]
    )
  BeginTrade iid (AssetTarget aid) iids | iid == investigatorId -> a <$ push
    (chooseOne
      iid
      [ TargetLabel (InvestigatorTarget iid') [TakeControlOfAsset iid' aid]
      | iid' <- iids
      ]
    )
  BeginTrade iid ResourceTarget iids | iid == investigatorId -> a <$ push
    (chooseOne
      iid
      [ TargetLabel
          (InvestigatorTarget iid')
          [TakeResources iid' 1 False, SpendResources iid 1]
      | iid' <- iids
      ]
    )
  SetRole iid role | iid == investigatorId -> do
    pure $ a { investigatorClass = role }
  AllRandomDiscard | not (a ^. defeatedL || a ^. resignedL) ->
    a <$ push (RandomDiscard investigatorId)
  RandomDiscard iid | iid == investigatorId -> do
    n <- getRandomR (0, length investigatorHand - 1)
    case investigatorHand !!? n of
      Nothing -> pure a
      Just c -> a <$ push (DiscardCard investigatorId (toCardId c))
  FinishedWithMulligan iid | iid == investigatorId -> do
    modifiers' <- getModifiers (toSource a) (toTarget a)
    let (discard, hand, deck) = drawOpeningHand a (5 - length investigatorHand)
    let
      startingResources = foldl'
        (\total -> \case
          StartingResources n -> max 0 (total + n)
          _ -> total
        )
        5
        modifiers'
    window <- checkWindows
      [Window Timing.After (Window.DrawingStartingHand iid)]
    pushAll [ShuffleDiscardBackIn iid, window]
    pure
      $ a
      & (resourcesL .~ startingResources)
      & (discardL .~ discard)
      & (handL .~ hand)
      & (deckL .~ Deck deck)
  ShuffleDiscardBackIn iid | iid == investigatorId -> do
    modifiers' <- getModifiers (toSource a) (toTarget a)
    if null investigatorDiscard
        || CardsCannotLeaveYourDiscardPile
        `elem` modifiers'
      then pure a
      else do
        deck <- shuffleM (investigatorDiscard <> coerce investigatorDeck)
        pure $ a & discardL .~ [] & deckL .~ Deck deck
  Resign iid | iid == investigatorId -> do
    pushAll $ resolve $ InvestigatorResigned iid
    pure $ a & resignedL .~ True
  InvestigatorDefeated source iid | iid == investigatorId -> do
    windowMsg <- checkWindows
      ((`Window` Window.InvestigatorDefeated source iid)
      <$> [Timing.When, Timing.After]
      )
    pushAll [windowMsg, InvestigatorWhenEliminated (toSource a) iid]
    pure $ a & defeatedL .~ True
  InvestigatorResigned iid | iid == investigatorId -> do
    push (InvestigatorWhenEliminated (toSource a) iid)
    pure $ a & resignedL .~ True
  -- InvestigatorWhenEliminated is handled by the scenario
  InvestigatorEliminated iid | iid == investigatorId -> do
    push (PlaceClues (LocationTarget investigatorLocation) investigatorClues)
    pure $ a & cluesL .~ 0 & resourcesL .~ 0
  EnemyMove eid lid | lid /= investigatorLocation ->
    pure $ a & engagedEnemiesL %~ deleteSet eid
  EnemyEngageInvestigator eid iid | iid == investigatorId ->
    pure $ a & engagedEnemiesL %~ insertSet eid
  RemoveEnemy eid -> pure $ a & engagedEnemiesL %~ deleteSet eid
  TakeControlOfAsset iid aid | iid == investigatorId -> do
    slots <- getList aid
    traits <- getSetList aid
    a <$ push (InvestigatorPlayAsset iid aid slots traits)
  TakeControlOfAsset iid aid | iid /= investigatorId ->
    pure $ a & (assetsL %~ deleteSet aid) & (slotsL %~ removeFromSlots aid)
  ChooseAndDiscardAsset iid assetMatcher | iid == investigatorId -> do
    discardableAssetIds <- selectList
      (assetMatcher <> DiscardableAsset <> AssetControlledBy You)
    a <$ push
      (chooseOrRunOne iid $ map (Discard . AssetTarget) discardableAssetIds)
  AttachAsset aid _ | aid `member` investigatorAssets ->
    pure $ a & (assetsL %~ deleteSet aid) & (slotsL %~ removeFromSlots aid)
  AttachTreachery tid (InvestigatorTarget iid) | iid == investigatorId ->
    pure $ a & treacheriesL %~ insertSet tid
  AllCheckHandSize | not (a ^. defeatedL || a ^. resignedL) -> do
    handSize <- getHandSize a
    inHandCount <- getInHandCount a
    when (inHandCount > handSize) $ push (CheckHandSize investigatorId)
    pure a
  CheckHandSize iid | iid == investigatorId -> do
    handSize <- getHandSize a
    inHandCount <- getInHandCount a
    when (inHandCount > handSize) $ push
      (chooseOne
        iid
        [ Run [DiscardCard iid (toCardId card), CheckHandSize iid]
        | card <- filter (isNothing . cdCardSubType . toCardDef)
          $ mapMaybe (preview _PlayerCard) investigatorHand
        ]
      )
    pure a
  AddToDiscard iid pc | iid == investigatorId -> pure $ a & discardL %~ (pc :)
  ChooseAndDiscardCard iid | iid == investigatorId -> a <$ push
    (chooseOne iid
    $ [ DiscardCard iid (toCardId card) | card <- discardableCards a ]
    )
  Discard (CardIdTarget cardId)
    | isJust (find ((== cardId) . toCardId) investigatorHand) -> a
    <$ push (DiscardCard investigatorId cardId)
  DiscardHand iid | iid == investigatorId ->
    a <$ pushAll (map (DiscardCard iid . toCardId) investigatorHand)
  DiscardCard iid cardId | iid == investigatorId -> do
    let
      card = fromJustNote "must be in hand"
        $ find ((== cardId) . toCardId) investigatorHand
    case card of
      PlayerCard pc ->
        pure $ a & handL %~ filter ((/= cardId) . toCardId) & discardL %~ (pc :)
      EncounterCard _ -> pure $ a & handL %~ filter ((/= cardId) . toCardId) -- TODO: This should discard to the encounter discard
  RemoveCardFromHand iid cardId | iid == investigatorId ->
    pure $ a & handL %~ filter ((/= cardId) . toCardId)
  ShuffleIntoDeck iid (TreacheryTarget tid) | iid == investigatorId ->
    pure $ a & treacheriesL %~ deleteSet tid
  ShuffleIntoDeck iid (AssetTarget aid) | iid == investigatorId -> do
    card <- fromJustNote "missing card" <$> getPlayerCard aid
    deck' <- shuffleM (card : unDeck investigatorDeck)
    push $ After msg
    pure
      $ a
      & (assetsL %~ deleteSet aid)
      & (deckL .~ Deck deck')
      & (slotsL %~ removeFromSlots aid)
  ShuffleIntoDeck iid (EventTarget eid) | iid == investigatorId -> do
    card <- fromJustNote "missing card" <$> getPlayerCard eid
    deck' <- shuffleM (card : unDeck investigatorDeck)
    push $ After msg
    pure $ a & (deckL .~ Deck deck')
  AddTreacheryToHand iid tid | iid == investigatorId ->
    pure $ a & inHandTreacheriesL %~ insertSet tid
  Discard (TreacheryTarget tid) ->
    pure
      $ a
      & treacheriesL
      %~ deleteSet tid
      & inHandTreacheriesL
      %~ deleteSet tid
  Discarded (EnemyTarget eid) _ -> pure $ a & engagedEnemiesL %~ deleteSet eid
  PlaceEnemyInVoid eid -> pure $ a & engagedEnemiesL %~ deleteSet eid
  Discarded (AssetTarget aid) (PlayerCard card)
    | aid `elem` investigatorAssets -> do
      let
        slotTypes = cdSlots $ toCardDef card
        slots slotType = findWithDefault [] slotType investigatorSlots
        assetIds slotType = mapMaybe slotItem $ slots slotType
      pushAll
        [ RefillSlots
            investigatorId
            slotType
            (filter (/= aid) $ assetIds slotType)
        | slotType <- slotTypes
        ]
      pure
        $ a
        & (assetsL %~ deleteSet aid)
        & (discardL %~ (card :))
        & (slotsL %~ removeFromSlots aid)
  Discarded (AssetTarget aid) (EncounterCard _)
    | aid `elem` investigatorAssets
    -> pure $ a & (assetsL %~ deleteSet aid) & (slotsL %~ removeFromSlots aid)
  Exiled (AssetTarget aid) _ | aid `elem` investigatorAssets ->
    pure $ a & (assetsL %~ deleteSet aid) & (slotsL %~ removeFromSlots aid)
  RemoveFromGame (AssetTarget aid) ->
    pure $ a & (assetsL %~ deleteSet aid) & (slotsL %~ removeFromSlots aid)
  RemoveFromGame (CardIdTarget cid) ->
    pure $ a & cardsUnderneathL %~ filter ((/= cid) . toCardId)
  ChooseFightEnemy iid source mTarget skillType enemyMatcher isAction
    | iid == investigatorId -> do
      enemyIds <- selectList (CanFightEnemy <> enemyMatcher)
      a <$ push
        (chooseOne
          iid
          [ FightEnemy iid eid source mTarget skillType isAction
          | eid <- enemyIds
          ]
        )
  EngageEnemy iid eid True | iid == investigatorId -> do
    modifiers' <- getModifiers (toSource a) (toTarget a)
    a <$ pushAll
      ([TakeAction iid (Just Action.Engage) (ActionCost 1)]
      <> [ CheckAttackOfOpportunity iid False
         | ActionDoesNotCauseAttacksOfOpportunity Action.Engage
           `notElem` modifiers'
         ]
      <> [EngageEnemy iid eid False]
      )
  EngageEnemy iid eid False | iid == investigatorId ->
    pure $ a & engagedEnemiesL %~ insertSet eid
  EngageEnemy iid eid False | iid /= investigatorId ->
    pure $ a & engagedEnemiesL %~ deleteSet eid
  FightEnemy iid eid source mTarget skillType True | iid == investigatorId -> do
    modifiers' <- getModifiers (EnemySource eid) (toTarget a)
    let
      takenActions = setFromList @(HashSet Action) investigatorActionsTaken
      applyFightCostModifiers :: Cost -> ModifierType -> Cost
      applyFightCostModifiers costToEnter (ActionCostOf actionTarget n) =
        case actionTarget of
          FirstOneOf as
            | Action.Fight `elem` as && null
              (takenActions `intersect` setFromList as)
            -> increaseActionCost costToEnter n
          IsAction Action.Fight -> increaseActionCost costToEnter n
          _ -> costToEnter
      applyFightCostModifiers costToEnter _ = costToEnter
    a <$ pushAll
      [ TakeAction
        iid
        (Just Action.Fight)
        (foldl' applyFightCostModifiers (ActionCost 1) modifiers')
      , FightEnemy iid eid source mTarget skillType False
      ]
  FightEnemy iid eid source mTarget skillType False | iid == investigatorId ->
    do
      a <$ push (AttackEnemy iid eid source mTarget skillType)
  FailedAttackEnemy iid eid | iid == investigatorId -> do
    doesNotDamageOtherInvestigators <- hasModifier
      a
      DoesNotDamageOtherInvestigator
    unless doesNotDamageOtherInvestigators $ do
      investigatorIds <- getSetList eid
      case investigatorIds of
        [x] | x /= iid -> push (InvestigatorDamageInvestigator iid x)
        _ -> pure ()
    pure a
  InvestigatorDamageInvestigator iid xid | iid == investigatorId -> do
    damage <- damageValueFor 1 a
    a
      <$ push
           (InvestigatorAssignDamage
             xid
             (InvestigatorSource iid)
             DamageAny
             damage
             0
           )
  InvestigatorDamageEnemy iid eid source | iid == investigatorId -> do
    damage <- damageValueFor 1 a
    a <$ push (EnemyDamage eid iid source AttackDamageEffect damage)
  EnemyEvaded iid eid | iid == investigatorId -> do
    push =<< checkWindows [Window Timing.After (Window.EnemyEvaded iid eid)]
    pure $ a & engagedEnemiesL %~ deleteSet eid
  AddToVictory (EnemyTarget eid) -> pure $ a & engagedEnemiesL %~ deleteSet eid
  -- TODO: WARNING: HERE BE DRAGONS
  ChooseEvadeEnemy iid source mTarget skillType enemyMatcher isAction
    | iid == investigatorId -> do
      enemyIds <- selectList (CanEvadeEnemy <> enemyMatcher)
      a <$ push
        (chooseOne
          iid
          [ EvadeLabel
              eid
              [ ChosenEvadeEnemy source eid
              , EvadeEnemy iid eid source mTarget skillType isAction
              ]
          | eid <- enemyIds
          ]
        )
  EvadeEnemy iid eid source mTarget skillType True | iid == investigatorId -> do
    modifiers' <- getModifiers (EnemySource eid) (toTarget a)
    let
      takenActions = setFromList @(HashSet Action) investigatorActionsTaken
      applyEvadeCostModifiers :: Cost -> ModifierType -> Cost
      applyEvadeCostModifiers costToEnter (ActionCostOf actionTarget n) =
        case actionTarget of
          FirstOneOf as
            | Action.Evade `elem` as && null
              (takenActions `intersect` setFromList as)
            -> increaseActionCost costToEnter n
          IsAction Action.Evade -> increaseActionCost costToEnter n
          _ -> costToEnter
      applyEvadeCostModifiers costToEnter _ = costToEnter
    a <$ pushAll
      [ TakeAction
        iid
        (Just Action.Evade)
        (foldl' applyEvadeCostModifiers (ActionCost 1) modifiers')
      , EvadeEnemy iid eid source mTarget skillType False
      ]
  EvadeEnemy iid eid source mTarget skillType False | iid == investigatorId ->
    a <$ pushAll
      [TryEvadeEnemy iid eid source mTarget skillType, AfterEvadeEnemy iid eid]
  MoveAction iid lid cost True | iid == investigatorId -> a <$ pushAll
    [TakeAction iid (Just Action.Move) cost, MoveAction iid lid cost False]
  MoveAction iid lid _cost False | iid == investigatorId -> do
    afterWindowMsg <- Helpers.checkWindows
      [Window Timing.After $ Window.MoveAction iid investigatorLocation lid]
    a <$ pushAll
      (resolve (Move (toSource a) iid investigatorLocation lid)
      <> [afterWindowMsg]
      )
  Move source iid fromLocationId destinationLocationId
    | iid == investigatorId -> do
      windowMsgs <- Helpers.windows
        [Window.Moves iid fromLocationId destinationLocationId]
      a <$ pushAll
        ([ Will (MoveFrom source iid fromLocationId)
         , Will (MoveTo source iid destinationLocationId)
         , MoveFrom source iid fromLocationId
         , MoveTo source iid destinationLocationId
         ]
        <> windowMsgs
        )
  Will (FailedSkillTest iid _ _ (InvestigatorTarget iid') _ _)
    | iid == iid' && iid == investigatorId -> do
      window <- checkWindows
        [Window Timing.When (Window.WouldFailSkillTest iid)]
      a <$ push window
  CancelDamage iid n | iid == investigatorId -> do
    a <$ withQueue_ \queue -> flip
      map
      queue
      \case
        InvestigatorDamage iid' s damage' horror' ->
          InvestigatorDamage iid' s (max 0 (damage' - n)) horror'
        InvestigatorDoAssignDamage iid' s t damage' horror' aa b ->
          InvestigatorDoAssignDamage iid' s t (max 0 (damage' - n)) horror' aa b
        other -> other
  CancelHorror iid n | iid == investigatorId -> do
    a <$ withQueue_ \queue -> flip
      map
      queue
      \case
        InvestigatorDamage iid' s damage' horror' ->
          InvestigatorDamage iid' s damage' (max 0 (horror' - n))
        InvestigatorDoAssignDamage iid' s t damage' horror' aa b ->
          InvestigatorDoAssignDamage iid' s t damage' (max 0 (horror' - n)) aa b
        other -> other
  InvestigatorDirectDamage iid source damage horror
    | iid == investigatorId && not
      (investigatorDefeated || investigatorResigned)
    -> a <$ pushAll
      ([ CheckWindow
           [iid]
           [Window Timing.When (Window.WouldTakeDamage source (toTarget a))]
       | damage > 0
       ]
      <> [ CheckWindow
             [iid]
             [Window Timing.When (Window.WouldTakeHorror source (toTarget a))]
         | horror > 0
         ]
      <> [ CheckWindow
             [iid]
             [ Window
                 Timing.When
                 (Window.WouldTakeDamageOrHorror
                   source
                   (toTarget a)
                   damage
                   horror
                 )
             ]
         | horror > 0 || damage > 0
         ]
      <> [InvestigatorDamage iid source damage horror, CheckDefeated source]
      <> [After (InvestigatorTakeDamage iid source damage horror)]
      <> [ CheckWindow
             [iid]
             [Window Timing.When (Window.DealtHorror source (toTarget a))]
         | horror > 0
         ]
      <> [ CheckWindow
             [iid]
             [ Window
                 Timing.When
                 (Window.DealtDamage source NonAttackDamageEffect (toTarget a)
                 )
             ]
         | damage > 0
         ]
      )
  InvestigatorAssignDamage iid source strategy damage horror
    | iid == investigatorId && not
      (investigatorDefeated || investigatorResigned)
    -> do
      modifiers <- getModifiers (toSource a) (toTarget a)
      if TreatAllDamageAsDirect `elem` modifiers
        then a <$ push (InvestigatorDirectDamage iid source damage horror)
        else a <$ pushAll
          ([ CheckWindow
               [iid]
               [Window Timing.When (Window.WouldTakeDamage source (toTarget a))]
           | damage > 0
           ]
          <> [ CheckWindow
                 [iid]
                 [ Window
                     Timing.When
                     (Window.WouldTakeHorror source (toTarget a))
                 ]
             | horror > 0
             ]
          <> [ CheckWindow
                 [iid]
                 [ Window
                     Timing.When
                     (Window.WouldTakeDamageOrHorror
                       source
                       (toTarget a)
                       damage
                       horror
                     )
                 ]
             | horror > 0 || damage > 0
             ]
          <> [ InvestigatorDoAssignDamage
               iid
               source
               strategy
               damage
               horror
               []
               []
             , CheckDefeated source
             ]
          <> [After (InvestigatorTakeDamage iid source damage horror)]
          )
  After (InvestigatorTakeDamage iid _ damage horror)
    | iid == investigatorId && (damage > 0 || horror > 0) -> do
      let
        windows =
          [ Window.PlacedDamage iid damage | damage > 0 ]
          <> [ Window.PlacedHorror iid horror | horror > 0 ]
      push =<< checkWindows
        (concatMap (\t -> map (Window t) windows) [Timing.When, Timing.After])
      pure a
  InvestigatorDoAssignDamage iid source _ 0 0 damageTargets horrorTargets
    | iid == investigatorId -> a <$ push
      (CheckWindow [iid]
      $ [ Window
            Timing.When
            (Window.DealtDamage source NonAttackDamageEffect target)
        | target <- nub damageTargets
        ]
      <> [ Window Timing.When (Window.DealtHorror source target)
         | target <- nub horrorTargets
         ]
      <> [Window Timing.When (Window.AssignedHorror source iid horrorTargets)]
      )

  InvestigatorDoAssignDamage iid source SingleTarget health sanity damageTargets horrorTargets
    | iid == investigatorId
    -> do
      healthDamageableAssets <- if health > 0
        then select (AssetCanBeAssignedDamageBy iid)
        else pure mempty
      sanityDamageableAssets <- if sanity > 0
        then select (AssetCanBeAssignedHorrorBy iid)
        else pure mempty
      let
        damageableAssets =
          toList $ healthDamageableAssets `union` sanityDamageableAssets
        continue h s t = InvestigatorDoAssignDamage
          iid
          source
          SingleTarget
          (max 0 $ health - h)
          (max 0 $ sanity - s)
          (damageTargets <> [ t | h > 0 ])
          (horrorTargets <> [ t | s > 0 ])
        toAssetMessage (asset, (h, s)) = TargetLabel
          (AssetTarget asset)
          [ AssetDamage asset source (min h health) (min s sanity)
          , continue h s (AssetTarget asset)
          ]
      assetsWithCounts <- for damageableAssets $ \asset -> do
        health' <- unRemainingHealth <$> getCount asset
        sanity' <- unRemainingSanity <$> getCount asset
        pure (asset, (health', sanity'))

      push
        $ chooseOne iid
        $ TargetLabel
            (toTarget a)
            [ InvestigatorDamage investigatorId source health sanity
            , continue health sanity (toTarget a)
            ]
        : map toAssetMessage assetsWithCounts
      pure a
  InvestigatorDoAssignDamage iid source strategy health sanity damageTargets horrorTargets
    | iid == investigatorId
    -> do
      healthDamageMessages <- if health > 0
        then do
          healthDamageableAssets <- selectList (AssetCanBeAssignedDamageBy iid)
          let
            assignRestOfHealthDamage = InvestigatorDoAssignDamage
              investigatorId
              source
              strategy
              (health - 1)
              sanity
            damageAsset aid = Run
              [ AssetDamage aid source 1 0
              , assignRestOfHealthDamage
                (AssetTarget aid : damageTargets)
                horrorTargets
              ]
            damageInvestigator = Run
              [ InvestigatorDamage investigatorId source 1 0
              , assignRestOfHealthDamage
                (InvestigatorTarget investigatorId : damageTargets)
                horrorTargets
              ]
          case strategy of
            DamageAssetsFirst -> do
              pure
                $ [ damageInvestigator | null healthDamageableAssets ]
                <> map damageAsset healthDamageableAssets
            DamageAny ->
              pure $ damageInvestigator : map damageAsset healthDamageableAssets
            DamageFirst def -> do
              validAssets <-
                setToList
                . intersection (setFromList healthDamageableAssets)
                <$> select (AssetControlledBy You <> assetIs def)
              pure $ if null validAssets
                then damageInvestigator : map damageAsset healthDamageableAssets
                else map damageAsset validAssets
            SingleTarget -> error "handled elsewhere"
        else pure []
      sanityDamageMessages <- if sanity > 0
        then do
          sanityDamageableAssets <- selectList (AssetCanBeAssignedHorrorBy iid)
          let
            assignRestOfSanityDamage = InvestigatorDoAssignDamage
              investigatorId
              source
              strategy
              health
              (sanity - 1)
            damageInvestigator = Run
              [ InvestigatorDamage investigatorId source 0 1
              , assignRestOfSanityDamage
                damageTargets
                (InvestigatorTarget investigatorId : horrorTargets)
              ]
            damageAsset aid = Run
              [ AssetDamage aid source 0 1
              , assignRestOfSanityDamage
                damageTargets
                (AssetTarget aid : horrorTargets)
              ]
          case strategy of
            DamageAssetsFirst ->
              pure
                $ [ damageInvestigator | null sanityDamageableAssets ]
                <> map damageAsset sanityDamageableAssets
            DamageAny -> do
              mustBeDamagedFirstBeforeInvestigator <- selectList
                (AssetCanBeAssignedHorrorBy iid
                <> AssetWithModifier NonDirectHorrorMustBeAssignToThisFirst
                )
              pure
                $ [ damageInvestigator
                  | null mustBeDamagedFirstBeforeInvestigator
                  ]
                <> map damageAsset sanityDamageableAssets
            DamageFirst def -> do
              validAssets <-
                setToList
                . intersection (setFromList sanityDamageableAssets)
                <$> select (AssetControlledBy You <> assetIs def)
              pure $ if null validAssets
                then damageInvestigator : map damageAsset sanityDamageableAssets
                else map damageAsset validAssets
            SingleTarget -> error "handled elsewhere"
        else pure []
      a <$ push (chooseOne iid $ healthDamageMessages <> sanityDamageMessages)
  Investigate iid lid source mTarget skillType True | iid == investigatorId ->
    do
      modifiers <- getModifiers (toSource a) (LocationTarget lid)
      modifiers' <- getModifiers (toSource a) (toTarget a)
      let
        investigateCost = foldr applyModifier 1 modifiers
        applyModifier (ActionCostOf (IsAction Action.Investigate) m) n =
          max 0 (n + m)
        applyModifier _ n = n
      a <$ pushAll
        ([TakeAction iid (Just Action.Investigate) (ActionCost investigateCost)]
        <> [ CheckAttackOfOpportunity iid False
           | ActionDoesNotCauseAttacksOfOpportunity Action.Investigate
             `notElem` modifiers'
           ]
        <> [Investigate iid lid source mTarget skillType False]
        )
  InvestigatorDiscoverCluesAtTheirLocation iid n maction
    | iid == investigatorId -> runMessage
      (InvestigatorDiscoverClues iid investigatorLocation n maction)
      a
  InvestigatorDiscoverClues iid lid n maction | iid == investigatorId -> do
    canDiscoverClues <- getCanDiscoverClues a
    if canDiscoverClues
      then do
        modifiedCluesToDiscover <- cluesToDiscover a n
        a <$ push
          (DiscoverCluesAtLocation iid lid modifiedCluesToDiscover maction)
      else pure a
  GainClues iid n | iid == investigatorId -> do
    window <- checkWindows
      ((`Window` Window.GainsClues iid n) <$> [Timing.When, Timing.After])
    a <$ pushAll
      [window, PlaceClues (InvestigatorTarget iid) n, After (GainClues iid n)]
  PlaceClues (InvestigatorTarget iid) n | iid == investigatorId -> do
    pure $ a & cluesL +~ n
  DiscoverClues iid lid n maction | iid == investigatorId ->
    a <$ push (Do $ DiscoverClues iid lid n maction)
  Do (DiscoverClues iid _ n _) | iid == investigatorId -> do
    push (After (GainClues iid n))
    pure $ a & cluesL +~ n
  InvestigatorDiscardAllClues iid | iid == investigatorId ->
    pure $ a & cluesL .~ 0
  MoveAllCluesTo target | not (isTarget a target) -> do
    when (investigatorClues > 0) (push $ PlaceClues target investigatorClues)
    pure $ a & cluesL .~ 0
  PayCardCost iid cardId | iid == investigatorId -> do
    let card = findCard cardId a
    cost <- getModifiedCardCost iid card
    iids <- filter (/= iid) <$> getInvestigatorIds
    iidsWithModifiers <- for iids $ \iid' -> do
      modifiers <- getModifiers
        (InvestigatorSource iid')
        (InvestigatorTarget iid')
      pure (iid', modifiers)
    canHelpPay <- flip filterM iidsWithModifiers $ \(_, modifiers) -> do
      flip anyM modifiers $ \case
        CanSpendResourcesOnCardFromInvestigator iMatcher cMatcher -> liftA2
          (&&)
          (member iid <$> select iMatcher)
          (pure $ cardMatch card cMatcher)
        _ -> pure False
    if null canHelpPay
      then pure $ a & resourcesL -~ cost
      else do
        iidsWithResources <- traverse
          (traverseToSnd (fmap unResourceCount . getCount))
          (iid : map fst canHelpPay)
        a <$ push
          (Ask iid
          $ ChoosePaymentAmounts
              ("Pay " <> tshow cost <> " resources")
              (Just cost)
          $ map
              (\(iid', resources) ->
                (iid', (0, resources), SpendResources iid' 1)
              )
              iidsWithResources
          )
  PayDynamicCardCost iid cardId _n beforePlayMessages | iid == investigatorId ->
    a <$ push
      (Ask iid $ ChooseDynamicCardAmounts
        iid
        cardId
        (0, investigatorResources)
        False
        beforePlayMessages
      )
  PayedForDynamicCard iid cardId n False | iid == investigatorId -> do
    push (PlayDynamicCard iid cardId n Nothing False)
    pure $ a & resourcesL -~ n
  InitiatePlayDynamicCard iid cardId n mtarget asAction
    | iid == investigatorId -> do
      let card = findCard cardId a
      a <$ pushAll
        [ CheckWindow [iid] [Window Timing.When (Window.PlayCard iid card)]
        , PlayDynamicCard iid cardId n mtarget asAction
        ]
  PlayDynamicCard iid cardId _n _mtarget True | iid == investigatorId -> do
    let
      card = findCard cardId a
      isFast = case card of
        PlayerCard pc -> isJust (cdFastWindow $ toCardDef pc)
        _ -> False
      maction = case card of
        PlayerCard pc -> cdAction (toCardDef pc)
        _ -> Nothing
      actionProvokesAttackOfOpportunities =
        maction
          `notElem` [ Just Action.Evade
                    , Just Action.Parley
                    , Just Action.Resign
                    , Just Action.Fight
                    ]
      provokesAttackOfOpportunities = case card of
        PlayerCard pc ->
          actionProvokesAttackOfOpportunities
            && DoesNotProvokeAttacksOfOpportunity
            `notElem` cdAttackOfOpportunityModifiers (toCardDef pc)
        _ -> actionProvokesAttackOfOpportunities
      aooMessage =
        [ CheckAttackOfOpportunity iid isFast | provokesAttackOfOpportunities ]
    actionCost <- if isFast
      then pure 0
      else maybe (pure 1) (getActionCost a) maction
    a <$ pushAll
      [ TakeAction iid (Just Action.Play) (ActionCost actionCost)
      , PayDynamicCardCost iid cardId 0 aooMessage
      ]
  InitiatePlayCardAsChoose iid cardId choices msgs asAction
    | iid == investigatorId -> do
      a <$ push
        (chooseOne
          iid
          [ TargetLabel
              (CardIdTarget $ toCardId choice)
              [ ReturnToHand iid (EventTarget $ EventId cardId)
              , InitiatePlayCardAs iid cardId choice msgs asAction
              ]
          | choice <- choices
          ]
        )
  InitiatePlayCardAs iid cardId choice msgs asAction | iid == investigatorId ->
    do
      let
        card = findCard cardId a
        choiceDef = toCardDef choice
        choiceAsCard = (lookupPlayerCard choiceDef cardId)
          { pcOriginalCardCode = toCardCode card
          }
      pushAll $ msgs <> [InitiatePlayCard iid cardId Nothing asAction]
      pure $ a & handL %~ (PlayerCard choiceAsCard :) . filter
        ((/= cardId) . toCardId)
  InitiatePlayCard iid cardId mtarget asAction | iid == investigatorId -> do
    let card = findCard cardId a
    a <$ pushAll
      [ CheckWindow [iid] [Window Timing.When (Window.PlayCard iid card)]
      , if isFastCard card && toCardType card == EventType
        then PlayFastEvent
          iid
          cardId
          mtarget
          [Window Timing.When Window.FastPlayerWindow]
        else PlayCard iid cardId mtarget asAction
      ]
  PlayCard iid cardId mtarget True | iid == investigatorId -> do
    modifiers' <- getModifiers (InvestigatorSource iid) (CardIdTarget cardId)
    let
      card = findCard cardId a
      isFast = case card of
        PlayerCard pc ->
          isJust (cdFastWindow $ toCardDef pc) || BecomesFast `elem` modifiers'
        _ -> False
      maction = case card of
        PlayerCard pc -> cdAction (toCardDef pc)
        _ -> Nothing
      actionProvokesAttackOfOpportunities =
        maction
          `notElem` [ Just Action.Evade
                    , Just Action.Parley
                    , Just Action.Resign
                    , Just Action.Fight
                    ]
      provokesAttackOfOpportunities = case card of
        PlayerCard pc ->
          actionProvokesAttackOfOpportunities
            && DoesNotProvokeAttacksOfOpportunity
            `notElem` cdAttackOfOpportunityModifiers (toCardDef pc)
        _ -> actionProvokesAttackOfOpportunities
      aooMessage =
        [ CheckAttackOfOpportunity iid isFast | provokesAttackOfOpportunities ]
    actionCost <- if isFast
      then pure 0
      else maybe (pure 1) (getActionCost a) maction

    iids <- filter (/= iid) <$> getInvestigatorIds
    iidsWithModifiers <- for iids $ \iid' -> do
      modifiers <- getModifiers
        (InvestigatorSource iid')
        (InvestigatorTarget iid')
      pure (iid', modifiers)
    canHelpPay <- flip filterM iidsWithModifiers $ \(_, modifiers) -> do
      flip anyM modifiers $ \case
        CanSpendResourcesOnCardFromInvestigator iMatcher cMatcher -> liftA2
          (&&)
          (member iid <$> select iMatcher)
          (pure $ cardMatch card cMatcher)
        _ -> pure False
    additionalResources <-
      sum <$> traverse ((fmap unResourceCount . getCount) . fst) canHelpPay

    if investigatorRemainingActions
        >= actionCost
        && (investigatorResources + additionalResources >= getCost card)
      then a <$ pushAll
        ([ TakeAction iid (Just Action.Play) (ActionCost actionCost)
         , PayCardCost iid cardId
         ]
        <> aooMessage
        <> [PlayCard iid cardId mtarget False]
        )
      else pure a
  PlayedCard iid card | iid == investigatorId -> do
    send $ format a <> " played " <> format card
    push =<< checkWindows [Window Timing.After (Window.PlayCard iid card)]
    pure
      $ a
      & (handL %~ filter (/= card))
      & (discardL %~ filter ((/= card) . PlayerCard))
      & (deckL %~ Deck . filter ((/= card) . PlayerCard) . unDeck)
  InvestigatorPlayAsset iid aid slotTypes traits | iid == investigatorId -> do
    a <$ if fitsAvailableSlots slotTypes traits a
      then push (InvestigatorPlayedAsset iid aid slotTypes traits)
      else do
        let
          missingSlotTypes = slotTypes \\ concatMap
            (\slotType -> availableSlotTypesFor slotType traits a)
            (nub slotTypes)
        assetsThatCanProvideSlots <-
          selectList
          $ AssetControlledBy (InvestigatorWithId iid)
          <> DiscardableAsset
          <> AssetOneOf (map AssetInSlot missingSlotTypes)
        if null assetsThatCanProvideSlots
          then push $ InvestigatorPlayedAsset iid aid slotTypes traits
          else push
            (chooseOne
              iid
              [ Run
                  [ Discard (AssetTarget aid')
                  , InvestigatorPlayAsset iid aid slotTypes traits
                  ]
              | aid' <- assetsThatCanProvideSlots
              ]
            )
  InvestigatorPlayedAsset iid aid slotTypes traits | iid == investigatorId -> do
    let assetsUpdate = assetsL %~ insertSet aid
    pure $ foldl'
      (\a' slotType ->
        a' & slotsL . ix slotType %~ placeInAvailableSlot aid traits
      )
      (a & assetsUpdate)
      slotTypes
  RemoveAllCopiesOfCardFromGame iid cardCode | iid == investigatorId -> do
    for_ investigatorAssets $ \assetId -> do
      cardCode' <- getId @CardCode assetId
      when (cardCode == cardCode') (push $ RemoveFromGame (AssetTarget assetId))
    pure
      $ a
      & (deckL %~ Deck . filter ((/= cardCode) . toCardCode) . unDeck)
      & (discardL %~ filter ((/= cardCode) . toCardCode))
      & (handL %~ filter ((/= cardCode) . toCardCode))
  InvestigatorDamage iid _ health sanity | iid == investigatorId ->
    pure $ a & healthDamageL +~ health & sanityDamageL +~ sanity
  DrivenInsane iid | iid == investigatorId ->
    pure $ a & mentalTraumaL .~ investigatorSanity
  CheckDefeated source -> do
    facingDefeat <- getFacingDefeat a
    if facingDefeat
      then do
        modifiedHealth <- getModifiedHealth a
        modifiedSanity <- getModifiedSanity a
        push (InvestigatorWhenDefeated source investigatorId)
        let
          physicalTrauma =
            if investigatorHealthDamage >= modifiedHealth then 1 else 0
          mentalTrauma =
            if investigatorSanityDamage >= modifiedSanity then 1 else 0
        pure
          $ a
          & physicalTraumaL
          +~ physicalTrauma
          & mentalTraumaL
          +~ mentalTrauma
      else pure a
  HealDamage (InvestigatorTarget iid) amount | iid == investigatorId ->
    pure $ a & healthDamageL %~ max 0 . subtract amount
  HealHorror (InvestigatorTarget iid) amount | iid == investigatorId -> do
    cannotHealHorror <- hasModifier a CannotHealHorror
    pure $ if cannotHealHorror
      then a
      else a & sanityDamageL %~ max 0 . subtract amount
  InvestigatorWhenDefeated source iid | iid == investigatorId -> do
    push (InvestigatorDefeated source iid)
    pure $ a & defeatedL .~ True & endedTurnL .~ True
  InvestigatorKilled source iid | iid == investigatorId -> do
    unless investigatorDefeated $ push (InvestigatorDefeated source iid)
    pure $ a & defeatedL .~ True & endedTurnL .~ True
  MoveAllTo source lid | not (a ^. defeatedL || a ^. resignedL) ->
    a <$ push (MoveTo source investigatorId lid)
  MoveTo source iid lid | iid == investigatorId -> do
    movedByWindows <- Helpers.windows [Window.MovedBy source lid iid]
    afterMoveButBeforeEnemyEngagement <- Helpers.checkWindows
      [Window Timing.After (Window.MovedButBeforeEnemyEngagement iid lid)]
    afterEnterWindow <- checkWindows
      [Window Timing.After (Window.Entering iid lid)]
    pushAll
      $ movedByWindows
      <> [ WhenWillEnterLocation iid lid
         , EnterLocation iid lid
         , afterEnterWindow
         , afterMoveButBeforeEnemyEngagement
         , CheckEnemyEngagement iid
         ]
    pure $ a & locationL .~ lid
  CheckEnemyEngagement iid | iid == investigatorId -> do
    enemies <- selectList $ EnemyAt $ LocationWithId investigatorLocation
    a <$ pushAll [ EnemyCheckEngagement eid | eid <- enemies ]
  SetLocationAsIf iid lid | iid == investigatorId ->
    -- In the as if situation we want to avoid callbacks
    -- so this sets the value directly
    pure $ a & locationL .~ lid
  AddSlot iid slotType slot | iid == investigatorId -> do
    let
      slots = findWithDefault [] slotType investigatorSlots
      assetIds = mapMaybe slotItem slots
      emptiedSlots = sort $ slot : map emptySlot slots
    push (RefillSlots iid slotType assetIds)
    pure $ a & slotsL %~ insertMap slotType emptiedSlots
  RefillSlots iid slotType assetIds | iid == investigatorId -> do
    let
      slots = findWithDefault [] slotType investigatorSlots
      emptiedSlots = sort $ map emptySlot slots
    assetsWithTraits <- for assetIds $ \assetId -> do
      traits <- getSetList assetId
      pure (assetId, traits)
    let
      updatedSlots = foldl'
        (\s (aid, ts) -> if any (canPutIntoSlot ts) s
          then placeInAvailableSlot aid ts s
          else s
        )
        emptiedSlots
        assetsWithTraits
    if length (mapMaybe slotItem updatedSlots) == length assetIds
      then pure $ a & slotsL %~ insertMap slotType updatedSlots
      else do
        push
          (chooseOne
            iid
            [ Run
                [ Discard (AssetTarget aid')
                , RefillSlots iid slotType (filter (/= aid') assetIds)
                ]
            | aid' <- assetIds
            ]
          )
        pure a
  ChooseEndTurn iid | iid == investigatorId -> pure $ a & endedTurnL .~ True
  BeginRound -> do
    actionsForTurn <- getAbilitiesForTurn a
    pure
      $ a
      & (endedTurnL .~ False)
      & (remainingActionsL .~ actionsForTurn)
      & (actionsTakenL .~ mempty)
  DiscardTopOfDeck iid n mTarget | iid == investigatorId -> do
    let (cs, deck') = splitAt n (traceShowId $ unDeck investigatorDeck)
    windowMsgs <- if null deck'
      then pure <$> checkWindows
        ((`Window` Window.DeckHasNoCards iid) <$> [Timing.When, Timing.After])
      else pure []
    pushAll
      $ windowMsgs
      <> [ DeckHasNoCards investigatorId mTarget | null deck' ]
      <> [ DiscardedTopOfDeck iid cs target | target <- maybeToList mTarget ]
    pure $ a & deckL .~ Deck deck' & discardL %~ (reverse cs <>)
  DrawCards iid n True | iid == investigatorId -> a <$ pushAll
    [ TakeAction iid (Just Action.Draw) (ActionCost 1)
    , CheckAttackOfOpportunity iid False
    , DrawCards iid n False
    ]
  MoveTopOfDeckToBottom _ (InvestigatorDeck iid) n | iid == investigatorId -> do
    let (cards, deck) = splitAt n (unDeck investigatorDeck)
    pure $ a & deckL .~ Deck (deck <> cards)
  DrawCards iid 0 False | iid == investigatorId -> pure a
  DrawCards iid n False | iid == investigatorId -> do
    modifiers' <- getModifiers (toSource a) (toTarget a)
    if null (unDeck investigatorDeck)
      then
        if null investigatorDiscard
          || CardsCannotLeaveYourDiscardPile
          `elem` modifiers'
        then
          pure a
        else
          a <$ pushAll [EmptyDeck iid, DrawCards iid n False]
      else do
        let
          (mcard, deck) = drawCard (coerce investigatorDeck)
          handUpdate = maybe id ((:) . PlayerCard) mcard
        case mcard of
          Just card -> do
            when (toCardType card == PlayerTreacheryType)
              $ push (DrewTreachery iid $ PlayerCard card)
            when (toCardType card == PlayerEnemyType)
              $ push (DrewPlayerEnemy iid $ PlayerCard card)
            when
                (toCardType card
                `notElem` [PlayerTreacheryType, PlayerEnemyType]
                && isJust (cdCardSubType $ toCardDef card)
                )
              $ push (Revelation iid $ PlayerCardSource card)
          Nothing -> pure ()

        windowMsgs <- if null deck
          then pure <$> checkWindows
            ((`Window` Window.DeckHasNoCards iid)
            <$> [Timing.When, Timing.After]
            )
          else pure []
        pushAll
          $ windowMsgs
          <> [ DeckHasNoCards iid Nothing | null deck ]
          <> [ InvestigatorDrewPlayerCard iid card | card <- maybeToList mcard ]
          <> [DrawCards iid (n - 1) False]
        pure $ a & handL %~ handUpdate & deckL .~ Deck deck
  InvestigatorDrewPlayerCard iid card -> do
    windowMsg <- checkWindows
      [ Window
          Timing.After
          (Window.DrawCard iid (PlayerCard card) $ InvestigatorDeck iid)
      ]
    a <$ push windowMsg
  InvestigatorSpendClues iid n | iid == investigatorId -> pure $ a & cluesL -~ n
  SpendResources iid n | iid == investigatorId ->
    pure $ a & resourcesL %~ max 0 . subtract n
  LoseResources iid n | iid == investigatorId ->
    pure $ a & resourcesL %~ max 0 . subtract n
  LoseAllResources iid | iid == investigatorId -> pure $ a & resourcesL .~ 0
  TakeResources iid n True | iid == investigatorId -> do
    unlessM (hasModifier a CannotGainResources) $ pushAll
      [ TakeAction iid (Just Action.Resource) (ActionCost 1)
      , CheckAttackOfOpportunity iid False
      , TakeResources iid n False
      ]
    pure a
  TakeResources iid n False | iid == investigatorId -> do
    cannotGainResources <- hasModifier a CannotGainResources
    pure $ if cannotGainResources then a else a & resourcesL +~ n
  EmptyDeck iid | iid == investigatorId -> do
    modifiers' <- getModifiers (toSource a) (toTarget a)
    a <$ when
      (CardsCannotLeaveYourDiscardPile `notElem` modifiers')
      (pushAll
        [ShuffleDiscardBackIn iid, InvestigatorDamage iid EmptyDeckSource 0 1]
      )
  UseAbility iid ability@Ability {..} windows | iid == investigatorId ->
    a <$ push
      (CreatePayAbilityCostEffect ability abilitySource (toTarget a) windows)
  AllDrawCardAndResource | not (a ^. defeatedL || a ^. resignedL) -> do
    unlessM (hasModifier a CannotDrawCards)
      $ push (DrawCards investigatorId 1 False)
    mayChooseNotToTakeResources <-
      elem MayChooseNotToTakeUpkeepResources
        <$> getModifiers (toSource a) (InvestigatorTarget investigatorId)
    if mayChooseNotToTakeResources
      then a <$ push
        (chooseOne
          investigatorId
          [ Label "Do not take resource(s)" []
          , Label "Take resource(s)" [TakeResources investigatorId 1 False]
          ]
        )
      else pure $ a & resourcesL +~ 1
  LoadDeck iid deck | iid == investigatorId -> do
    shuffled <- shuffleM $ flip map (unDeck deck) $ \card ->
      if isJust (cdCardSubType $ toCardDef card)
        then card { pcBearer = Just iid }
        else card
    pure $ a & deckL .~ Deck shuffled
  InvestigatorCommittedCard iid card | iid == investigatorId -> do
    commitedCardWindows <- Helpers.windows [Window.CommittedCard iid card]
    pushAll $ FocusCards [card] : commitedCardWindows <> [UnfocusCards]
    pure
      $ a
      & handL
      %~ filter (/= card)
      & deckL
      %~ Deck
      . filter ((/= card) . PlayerCard)
      . unDeck
  PlaceUnderneath target cards | isTarget a target -> do
    let
      update = appEndo $ foldMap
        (\card ->
          Endo
            $ (assetsL %~ deleteSet (AssetId $ toCardId card))
            . (slotsL %~ removeFromSlots (AssetId $ toCardId card))
        )
        cards
    pure $ a & cardsUnderneathL <>~ cards & update
  PlaceUnderneath _ cards -> do
    let
      update = appEndo . mconcat $ map
        (\card ->
          Endo
            $ (assetsL %~ deleteSet (AssetId $ toCardId card))
            . (slotsL %~ removeFromSlots (AssetId $ toCardId card))
        )
        cards
    pure $ a & update
  BeforeSkillTest iid skillType skillDifficulty | iid == investigatorId -> do
    modifiers' <- getModifiers (toSource a) (toTarget a)
    committedCardIds <- map unCommittedCardId <$> getSetList iid
    committedCardCodes <- mapSet unCommittedCardCode <$> getSet ()
    let window = Window Timing.When (Window.SkillTest skillType)
    actions <- getActions iid window
    isScenarioAbility <- getIsScenarioAbility
    clueCount <- unClueCount <$> getCount investigatorLocation
    source <- fromJustNote "damage outside skill test" <$> getSkillTestSource

    skillTestModifiers' <- getModifiers (toSource a) SkillTestTarget
    cannotCommitCards <- elem (CannotCommitCards AnyCard)
      <$> getModifiers source (InvestigatorTarget investigatorId)
    let
      triggerMessage =
        [ StartSkillTest investigatorId
        | CannotPerformSkillTest `notElem` skillTestModifiers'
        ]
      beginMessage = BeforeSkillTest iid skillType skillDifficulty
    committableCards <- if cannotCommitCards
      then pure []
      else flip filterM investigatorHand $ \case
        PlayerCard card -> do
          let
            passesCommitRestriction = \case
              MaxOnePerTest ->
                pure $ toCardCode card `notElem` committedCardCodes
              OnlyYourTest -> pure True
              OnlyIfYourLocationHasClues -> pure $ clueCount > 0
              ScenarioAbility -> pure isScenarioAbility
              SelfCanCommitWhen matcher -> notNull <$> select (You <> matcher)
              MinSkillTestValueDifference n ->
                pure
                  $ (skillDifficulty - baseSkillValueFor skillType Nothing [] a)
                  >= n
            prevented = flip
              any
              modifiers'
              \case
                CanOnlyUseCardsInRole role ->
                  cdClassSymbol (toCardDef card)
                    `notElem` [Just Neutral, Just role, Nothing]
                CannotCommitCards matcher -> cardMatch card matcher
                _ -> False
          passesCommitRestrictions <- allM
            passesCommitRestriction
            (cdCommitRestrictions $ toCardDef card)
          pure
            $ toCardId card
            `notElem` committedCardIds
            && (SkillWild
               `elem` cdSkills (toCardDef card)
               || skillType
               `elem` cdSkills (toCardDef card)
               || (null (cdSkills $ toCardDef card)
                  && toCardType card
                  == SkillType
                  )
               )
            && passesCommitRestrictions
            && not prevented
        _ -> pure False
    if notNull committableCards || notNull committedCardIds || notNull actions
      then push
        (SkillTestAsk $ chooseOne
          iid
          (map
              (\card ->
                Run [SkillTestCommitCard iid (toCardId card), beginMessage]
              )
              committableCards
          <> map
               (\cardId ->
                 Run [SkillTestUncommitCard iid cardId, beginMessage]
               )
               committedCardIds
          <> map
               (\action -> Run [UseAbility iid action [window], beginMessage])
               actions
          <> triggerMessage
          )
        )
      else when
        (notNull triggerMessage)
        (push (SkillTestAsk $ chooseOne iid triggerMessage))
    pure a
  BeforeSkillTest iid skillType skillDifficulty | iid /= investigatorId -> do
    locationId <- getId iid
    isScenarioAbility <- getIsScenarioAbility
    clueCount <- unClueCount <$> getCount locationId
    canCommit <- canCommitToAnotherLocation a
    when (locationId == investigatorLocation || canCommit) $ do
      committedCardIds <- map unCommittedCardId <$> getSetList investigatorId
      committedCardCodes <- mapSet unCommittedCardCode <$> getSet ()
      modifiers' <- getModifiers (toSource a) (toTarget a)
      let beginMessage = BeforeSkillTest iid skillType skillDifficulty
      committableCards <- if notNull committedCardIds
        then pure []
        else flip
          filterM
          investigatorHand
          \case
            PlayerCard card -> do
              let
                passesCommitRestriction = \case
                  MaxOnePerTest ->
                    pure $ toCardCode card `notElem` committedCardCodes
                  OnlyYourTest -> pure False
                  OnlyIfYourLocationHasClues -> pure $ clueCount > 0
                  ScenarioAbility -> pure isScenarioAbility
                  SelfCanCommitWhen matcher ->
                    notNull <$> select (You <> matcher)
                  MinSkillTestValueDifference n ->
                    pure
                      $ (skillDifficulty
                        - baseSkillValueFor skillType Nothing [] a
                        )
                      >= n
                prevented = flip
                  any
                  modifiers'
                  \case
                    CanOnlyUseCardsInRole role ->
                      cdClassSymbol (toCardDef card)
                        `notElem` [Just Neutral, Just role, Nothing]
                    _ -> False
              passesCriterias <- allM
                passesCommitRestriction
                (cdCommitRestrictions $ toCardDef card)
              pure
                $ toCardId card
                `notElem` committedCardIds
                && (SkillWild
                   `elem` cdSkills (toCardDef card)
                   || skillType
                   `elem` cdSkills (toCardDef card)
                   )
                && passesCriterias
                && not prevented
            _ -> pure False
      when (notNull committableCards || notNull committedCardIds) $ push
        (SkillTestAsk $ chooseOne
          investigatorId
          (map
              (\card ->
                Run
                  [ SkillTestCommitCard investigatorId (toCardId card)
                  , beginMessage
                  ]
              )
              committableCards
          <> map
               (\cardId ->
                 Run
                   [SkillTestUncommitCard investigatorId cardId, beginMessage]
               )
               committedCardIds
          )
        )
    pure a
  CheckWindow iids windows | investigatorId `elem` iids -> do
    a <$ push (RunWindow investigatorId windows)
  RunWindow iid windows | iid == investigatorId -> do
    actions <- nub . concat <$> traverse (getActions iid) windows
    playableCards <- getPlayableCards a UnpaidCost windows
    if notNull playableCards || notNull actions
      then if any isForcedAbility actions
        then do
          -- Silent forced abilities should trigger automatically
          let
            (silent, normal) = partition isSilentForcedAbility actions
            toUseAbilities = map (($ windows) . UseAbility iid)
          a <$ pushAll
            (toUseAbilities silent
            <> [ chooseOne iid (toUseAbilities normal) | notNull normal ]
            <> [RunWindow iid windows]
            )
        else do
          actionsWithMatchingWindows <- for actions $ \ability@Ability {..} ->
            (ability, )
              <$> filterM
                    (\w -> windowMatches iid abilitySource w abilityWindow)
                    windows
          a <$ push
            (chooseOne iid
            $ [ Run
                  $ if isJust (cdFastWindow $ toCardDef c)
                      && toCardType c
                      == EventType
                    then
                      [ PlayFastEvent iid (toCardId c) Nothing windows
                      , RunWindow iid windows
                      ]
                    else
                      [ PayCardCost iid (toCardId c)
                        , PlayCard iid (toCardId c) Nothing False
                        ]
                        <> [RunWindow iid windows]
              | c <- playableCards
              ]
            <> map
                 (\(ability, windows') ->
                   Run
                     . (: [RunWindow iid windows]) -- original set of windows
                     $ UseAbility iid ability windows'
                 )
                 actionsWithMatchingWindows
            <> [Continue "Skip playing fast cards or using reactions"]
            )
      else pure a
  SpendActions iid _ n | iid == investigatorId ->
    pure $ a & remainingActionsL %~ max 0 . subtract n
  LoseActions iid _ n | iid == investigatorId ->
    pure $ a & remainingActionsL %~ max 0 . subtract n
  SetActions iid _ n | iid == investigatorId ->
    pure $ a & remainingActionsL .~ n
  GainActions iid _ n | iid == investigatorId ->
    pure $ a & remainingActionsL +~ n
  TakeAction iid mAction cost | iid == investigatorId -> a <$ pushAll
    ([ CreatePayAbilityCostEffect
         (abilityEffect a cost)
         (toSource a)
         (toTarget a)
         []
     ]
    <> [ TakenAction iid action | action <- maybeToList mAction ]
    )
  TakenAction iid action | iid == investigatorId ->
    pure $ a & actionsTakenL %~ (<> [action])
  PutOnTopOfDeck iid card | iid == investigatorId ->
    pure $ a & deckL %~ Deck . (card :) . unDeck & handL %~ filter
      ((/= Just card) . preview _PlayerCard)
  AddToHand iid card | iid == investigatorId -> do
    case card of
      PlayerCard card' -> do
        when (cdRevelation (toCardDef card'))
          $ if toCardType card' == PlayerTreacheryType
              then push (DrewTreachery iid card)
              else push (Revelation iid $ PlayerCardSource card')
        when (toCardType card' == PlayerEnemyType)
          $ push (DrewPlayerEnemy iid card)
      _ -> pure ()
    pure
      $ a
      & (handL %~ (card :))
      & (cardsUnderneathL %~ filter (/= card))
      & (assetsL %~ deleteSet (AssetId $ toCardId card))
      & (slotsL %~ removeFromSlots (AssetId $ toCardId card))
      & (discardL %~ filter ((/= card) . PlayerCard))
  ShuffleCardsIntoDeck iid cards | iid == investigatorId -> do
    deck <- shuffleM (cards <> unDeck investigatorDeck)
    pure $ a & deckL .~ Deck deck
  PlaceOnBottomOfDeck iid card | iid == investigatorId ->
    pure $ a & deckL %~ Deck . (<> [card]) . unDeck
  AddFocusedToHand _ (InvestigatorTarget iid') cardSource cardId
    | iid' == investigatorId -> do
      let
        isDeck = case cardSource of
          Zone.FromDeck -> True
          Zone.FromTopOfDeck _ -> True
          _ -> False
        card = fromJustNote "missing card" $ find
          ((== cardId) . toCardId)
          (findWithDefault [] cardSource investigatorFoundCards)
        foundCards = investigatorFoundCards & ix cardSource %~ filter (/= card)
        putBack =
          if isDeck then ShuffleAllFocusedIntoDeck else PutAllFocusedIntoDiscard
      pushAll [AddToHand iid' card, putBack iid' (InvestigatorTarget iid')]
      pure $ a & foundCardsL .~ foundCards
  AddFocusedToTopOfDeck _ (InvestigatorTarget iid') cardId
    | iid' == investigatorId -> do
      let
        card =
          fromJustNote "missing card"
            $ find
                ((== cardId) . toCardId)
                (concat $ toList investigatorFoundCards)
            >>= toPlayerCard
        foundCards =
          HashMap.map (filter ((/= cardId) . toCardId)) investigatorFoundCards
      push (PutOnTopOfDeck iid' card)
      pure $ a & foundCardsL .~ foundCards
  ShuffleAllFocusedIntoDeck _ (InvestigatorTarget iid')
    | iid' == investigatorId -> do
      let
        cards = mapMaybe (preview _PlayerCard)
          $ findWithDefault [] Zone.FromDeck investigatorFoundCards
      push (ShuffleCardsIntoDeck iid' cards)
      pure $ a & foundCardsL %~ deleteMap Zone.FromDeck
  PutAllFocusedIntoDiscard _ (InvestigatorTarget iid')
    | iid' == investigatorId -> do
      let
        cards = mapMaybe (preview _PlayerCard)
          $ findWithDefault [] Zone.FromDiscard investigatorFoundCards
      pure $ a & foundCardsL %~ deleteMap Zone.FromDiscard & discardL <>~ cards
  DisengageEnemy iid eid | iid == investigatorId ->
    pure $ a & engagedEnemiesL %~ deleteSet eid
  EndSearch iid _ (InvestigatorTarget iid') cardSources
    | iid == investigatorId -> do
      push (SearchEnded iid)
      let
        foundKey = \case
          Zone.FromTopOfDeck _ -> Zone.FromDeck
          other -> other
      for_ cardSources $ \(cardSource, returnStrategy) -> case returnStrategy of
        PutBackInAnyOrder -> do
          when (foundKey cardSource /= Zone.FromDeck) (error "Expects a deck")
          push
            (chooseOneAtATime iid $ map
              (AddFocusedToTopOfDeck iid (InvestigatorTarget iid') . toCardId)
              (findWithDefault [] Zone.FromDeck investigatorFoundCards)
            )
        ShuffleBackIn -> do
          when (foundKey cardSource /= Zone.FromDeck) (error "Expects a deck")
          push
            (ShuffleCardsIntoDeck
              iid
              (mapMaybe (preview _PlayerCard)
              $ findWithDefault [] Zone.FromDeck investigatorFoundCards
              )
            )
        PutBack -> when
          (foundKey cardSource == Zone.FromDeck)
          (error "Can not take deck")
      pure a
  SearchEnded iid | iid == investigatorId -> pure $ a & foundCardsL .~ mempty
  Search iid source target@(InvestigatorTarget iid') cardSources cardMatcher foundStrategy
    | iid' == investigatorId
    -> do
      let
        foundCards :: HashMap Zone [Card] = foldl'
          (\hmap (cardSource, _) -> case cardSource of
            Zone.FromDeck -> insertWith
              (<>)
              Zone.FromDeck
              (map PlayerCard $ unDeck investigatorDeck)
              hmap
            Zone.FromTopOfDeck n -> insertWith
              (<>)
              Zone.FromDeck
              (map PlayerCard . take n $ unDeck investigatorDeck)
              hmap
            Zone.FromDiscard -> insertWith
              (<>)
              Zone.FromDiscard
              (map PlayerCard investigatorDiscard)
              hmap
            other -> error $ mconcat ["Zone ", show other, " not yet handled"]
          )
          mempty
          cardSources
        deck = filter
          ((`notElem` findWithDefault [] Zone.FromDeck foundCards) . PlayerCard)
          (unDeck investigatorDeck)
        allFoundCards = concat $ toList foundCards
        targetCards = filter (`cardMatch` cardMatcher) allFoundCards
      push $ EndSearch iid source target cardSources
      case foundStrategy of
        DrawFound who n -> do
          let
            choices =
              [ AddFocusedToHand
                  iid
                  (InvestigatorTarget who)
                  Zone.FromDeck
                  (toCardId card)
              | card <- targetCards
              ]
          push
            (chooseN iid n
            $ if null choices then [Label "No cards found" []] else choices
            )
        PlayFound who n -> do
          let
            windows' =
              [ Window Timing.When Window.NonFast
              , Window Timing.When (Window.DuringTurn iid)
              ]
          playableCards <- filterM
            (getIsPlayable who source UnpaidCost windows')
            targetCards
          let
            choices =
              [ TargetLabel
                  (CardIdTarget $ toCardId card)
                  [ AddToHand who card
                  , PayCardCost iid (toCardId card)
                  , PlayCard iid (toCardId card) Nothing False
                  ]
              | card <- playableCards
              ]
          push
            (chooseN iid n
            $ if null choices then [Label "No cards found" []] else choices
            )
        DeferSearchedToTarget searchTarget -> do
          let
            choices =
              [SearchFound iid searchTarget (InvestigatorDeck iid) targetCards]
          push
            (chooseOne iid $ if null targetCards
              then [Label "No cards found" [SearchNoneFound iid searchTarget]]
              else choices
            )
        ReturnCards -> pure ()

      let
        deckCards = mapMaybe (preview _PlayerCard)
          $ findWithDefault [] Zone.FromDeck foundCards

      unless
        (null deckCards)
        do
          let window = Window Timing.When (Window.AmongSearchedCards iid)
          actions <- filterM
            (windowMatches iid source window . abilityWindow)
            =<< asks getAbilities
          -- TODO: This is for astounding revelation and only one research action is possible
          -- so we are able to short circuit here, but we may have additional cards in the
          -- future so we may want to make this more versatile
          unless (null actions) $ push
            (chooseOne iid
            $ map (($ [window]) . UseAbility iid) actions
            <> [Continue "Skip playing fast cards or using reactions!!!"]
            )
      pure $ a & (deckL .~ Deck deck) & (foundCardsL .~ foundCards)
  RemoveFromDiscard iid cardId | iid == investigatorId ->
    pure $ a & discardL %~ filter ((/= cardId) . toCardId)
  SufferTrauma iid physical mental | iid == investigatorId ->
    pure $ a & physicalTraumaL +~ physical & mentalTraumaL +~ mental
  GainXP iid amount | iid == investigatorId -> pure $ a & xpL +~ amount
  InvestigatorPlaceCluesOnLocation iid n | iid == investigatorId -> do
    let cluesToPlace = min n investigatorClues
    push (PlaceClues (LocationTarget investigatorLocation) cluesToPlace)
    pure $ a & cluesL -~ cluesToPlace
  InvestigatorPlaceAllCluesOnLocation iid | iid == investigatorId -> do
    push (PlaceClues (LocationTarget investigatorLocation) investigatorClues)
    pure $ a & cluesL .~ 0
  RemoveFromBearersDeckOrDiscard card -> do
    if pcBearer card == Just investigatorId
      then
        pure
        $ a
        & (discardL %~ filter (/= card))
        & (deckL %~ Deck . filter (/= card) . unDeck)
      else pure a
  RemoveDiscardFromGame iid | iid == investigatorId -> do
    pushAll $ map (RemovedFromGame . PlayerCard) investigatorDiscard
    pure $ a & discardL .~ []
  After (FailedSkillTest iid mAction _ (InvestigatorTarget iid') _ n)
    | iid == iid' && iid == investigatorId -> do
      mTarget <- getSkillTestTarget
      let
        windows = maybe
          []
          (\case
            Action.Investigate -> case mTarget of
              Just (LocationTarget lid) ->
                [ Window
                    Timing.After
                    (Window.FailInvestigationSkillTest iid lid n)
                ]
              _ ->
                [ Window
                    Timing.After
                    (Window.FailInvestigationSkillTest
                      iid
                      investigatorLocation
                      n
                    )
                ]
            _ -> []
          )
          mAction
      windowMsg <- checkWindows
        (Window Timing.After (Window.FailSkillTest iid n) : windows)
      a <$ push windowMsg
  After (PassedSkillTest iid mAction source (InvestigatorTarget iid') _ n)
    | iid == iid' && iid == investigatorId -> do
      mTarget <- getSkillTestTarget
      let
        windows = maybe
          []
          (\case
            Action.Investigate -> case mTarget of
              Just (ProxyTarget (LocationTarget lid) _) ->
                [ Window
                    Timing.After
                    (Window.PassInvestigationSkillTest iid lid n)
                ]
              Just (LocationTarget lid) ->
                [ Window
                    Timing.After
                    (Window.PassInvestigationSkillTest iid lid n)
                ]
              _ -> error "expecting location source for investigate"
            _ -> []
          )
          mAction
      window <- checkWindows
        (Window Timing.After (Window.PassSkillTest mAction source iid n)
        : windows
        )
      a <$ push window
  PlayerWindow iid additionalActions isAdditional | iid == investigatorId -> do
    let
      windows =
        [ Window Timing.When (Window.DuringTurn iid)
        , Window Timing.When Window.FastPlayerWindow
        , Window Timing.When Window.NonFast
        ]
    actions <- nub <$> concatMapM (getActions iid) windows
    if any isForcedAbility actions
      then a
        <$ push (chooseOne iid $ map (($ windows) . UseAbility iid) actions)
      else do
        modifiers <- getModifiers
          (InvestigatorSource iid)
          (InvestigatorTarget iid)
        canAffordTakeResources <- getCanAfford a Action.Resource
        canAffordDrawCards <- getCanAfford a Action.Draw
        canAffordPlayCard <- getCanAfford a Action.Play
        playableCards <- getPlayableCards a UnpaidCost windows
        let usesAction = not isAdditional
        a <$ push
          (AskPlayer $ chooseOne
            iid
            (additionalActions
            <> [ TakeResources iid 1 usesAction
               | canAffordTakeResources
                 && CannotGainResources
                 `notElem` modifiers
               ]
            <> [ DrawCards iid 1 usesAction
               | canAffordDrawCards
                 && CannotTakeAction (IsAction Action.Draw)
                 `notElem` modifiers
                 && CannotDrawCards
                 `notElem` modifiers
                 && CannotManipulateDeck
                 `notElem` modifiers
               ]
            <> [ InitiatePlayCard iid (toCardId c) Nothing usesAction
               | c <- playableCards
               , canAffordPlayCard || isFastCard c
               , not (isDynamic c)
               ]
            <> [ InitiatePlayDynamicCard iid (toCardId c) 0 Nothing usesAction
               | c <- playableCards
               , canAffordPlayCard || isFastCard c
               , isDynamic c
               ]
            <> [ChooseEndTurn iid]
            <> map (($ windows) . UseAbility iid) actions
            )
          )
  PlayerWindow iid additionalActions isAdditional | iid /= investigatorId -> do
    let
      windows =
        [ Window Timing.When (Window.DuringTurn iid)
        , Window Timing.When Window.FastPlayerWindow
        , Window Timing.When Window.NonFast
        ]
    actions <- nub <$> concatMapM (getActions iid) windows
    if any isForcedAbility actions
      then pure a -- handled by active player
      else do
        playableCards <- getPlayableCards a UnpaidCost windows
        let
          usesAction = not isAdditional
          choices =
            additionalActions
              <> [ InitiatePlayCard
                     investigatorId
                     (toCardId c)
                     Nothing
                     usesAction
                 | c <- playableCards
                 , not (isDynamic c)
                 ]
              <> [ InitiatePlayDynamicCard
                     investigatorId
                     (toCardId c)
                     0
                     Nothing
                     usesAction
                 | c <- playableCards
                 , isDynamic c
                 ]
              <> map (($ windows) . UseAbility investigatorId) actions
        a <$ unless
          (null choices)
          (push $ AskPlayer $ chooseOne investigatorId choices)
  Blanked msg' -> runMessage msg' a
  _ -> pure a

getFacingDefeat
  :: (MonadReader env m, HasModifiersFor env ()) => InvestigatorAttrs -> m Bool
getFacingDefeat a@InvestigatorAttrs {..} = do
  modifiedHealth <- getModifiedHealth a
  modifiedSanity <- getModifiedSanity a
  pure
    $ investigatorHealthDamage
    >= modifiedHealth
    || investigatorSanityDamage
    >= modifiedSanity

