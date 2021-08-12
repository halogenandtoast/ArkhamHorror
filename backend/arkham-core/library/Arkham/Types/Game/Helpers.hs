{-# OPTIONS_GHC -fno-warn-orphans #-}
module Arkham.Types.Game.Helpers where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Action (Action, TakenAction(..))
import qualified Arkham.Types.Action as Action
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Keyword
import qualified Arkham.Types.Keyword as Keyword
import qualified Arkham.Types.Label as Location
import Arkham.Types.Matcher
import qualified Arkham.Types.Matcher as Matcher
import Arkham.Types.Message hiding (When)
import qualified Arkham.Types.Message as Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Phase
import Arkham.Types.Query
import Arkham.Types.Restriction
import {-# SOURCE #-} Arkham.Types.SkillTest
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait (Trait, toTraits)
import Arkham.Types.Window
import qualified Arkham.Types.Window as Window
import Control.Monad.Extra (allM, andM, anyM)
import Data.HashSet (size)
import Data.UUID (nil)
import System.IO.Unsafe

checkWindows
  :: (MonadReader env m, HasSet InvestigatorId env ())
  => [Window]
  -> m [Message]
checkWindows windows = do
  iids <- getInvestigatorIds
  pure $ [ CheckWindow iid windows | iid <- iids ]

cancelToken :: (HasQueue env, MonadIO m, MonadReader env m) => Token -> m ()
cancelToken token = withQueue $ \queue ->
  ( filter
    (\case
      Message.When (Message.RevealToken _ _ token') | token == token' -> False
      Message.RevealToken _ _ token' | token == token' -> False
      Message.After (Message.RevealToken _ _ token') | token == token' -> False
      Message.RequestedTokens _ _ [token'] | token == token' -> False
      Message.RequestedTokens{} -> error "not setup for multiple tokens"
      _ -> True
    )
    queue
  , ()
  )

replaceToken :: (HasQueue env, MonadIO m, MonadReader env m) => Token -> m ()
replaceToken token = withQueue $ \queue ->
  ( map
    (\case
      Message.When (Message.RevealToken s i _) ->
        Message.When (Message.RevealToken s i token)
      Message.RevealToken s i _ -> Message.RevealToken s i token
      Message.After (Message.RevealToken s i _) ->
        Message.After (Message.RevealToken s i token)
      Message.RequestedTokens source' miid [_] ->
        Message.RequestedTokens source' miid [token]
      Message.RequestedTokens{} -> error "not setup for multiple tokens"
      m -> m
    )
    queue
  , ()
  )

getCanPerformAbility
  :: (MonadReader env m, MonadIO m, CanCheckPlayable env)
  => InvestigatorId
  -> Window
  -> Ability
  -> m Bool
getCanPerformAbility iid window ability = do
-- can perform an ability means you can afford it
-- it is in the right window
-- passes restrictions
  let
    mAction = abilityAction ability
    cost = abilityCost ability
  andM
    [ getCanAffordCost iid (abilitySource ability) mAction [window] cost
    , windowMatches iid window (abilityWindow ability)
    , maybe
      (pure True)
      (passesRestriction iid (abilitySource ability) [window])
      (abilityRestrictions ability)
    ]

--   (&&) <$> meetsAbilityRestrictions <*> meetsActionRestrictions abilityType
--  where
--   meetsAbilityRestrictions = case abilityRestrictions of
--     Nothing -> pure True
--     Just restriction ->
--       getCanPerformAbilityRestriction iid [window] restriction
--   meetsActionRestrictions = \case
--     ActionAbilityWithBefore mAction mBeforeAction cost -> liftA2
--       (||)
--       (meetsActionRestrictions $ ActionAbility mAction cost)
--       (meetsActionRestrictions $ ActionAbility mBeforeAction cost)
--     ActionAbilityWithSkill mAction _ cost ->
--       meetsActionRestrictions $ ActionAbility mAction cost
--     ActionAbility (Just action) _ -> case action of
--       Action.Fight -> hasFightActions iid window
--       Action.Evade -> hasEvadeActions iid window
--       Action.Investigate -> hasInvestigateActions iid window
--       -- The below actions may not be handled correctly yet
--       Action.Ability -> pure True
--       Action.Draw -> pure True
--       Action.Engage -> pure True
--       Action.Move -> pure True
--       Action.Parley -> pure True
--       Action.Play -> pure True
--       Action.Resign -> pure True
--       Action.Resource -> pure True
--     ActionAbility Nothing _ -> pure True
--     FastAbility _ -> pure True
--     ReactionAbility _ _ -> pure True
--     ForcedAbility -> pure True
--     AbilityEffect _ -> pure True

-- getCanPerformAbilityRestriction
--   :: (MonadReader env m, CanCheckFast env, CanCheckPlayable env, MonadIO m)
--   => InvestigatorId
--   -> [Window]
--   -> Restriction
--   -> m Bool
-- getCanPerformAbilityRestriction iid windows restrictions = do
--   lid' <- getId @LocationId iid
--   passesRestriction iid lid' windows restrictions
--
-- getCanAffordAbility
--   :: ( MonadReader env m
--      , HasModifiersFor env ()
--      , HasCostPayment env
--      , HasSet Trait env Source
--      , HasList UsedAbility env ()
--      )
--   => InvestigatorId
--   -> Ability
--   -> m Bool
-- getCanAffordAbility iid ability =
--   (&&) <$> getCanAffordUse iid ability <*> getCanAffordAbilityCost iid ability
--
-- getCanAffordAbilityCost
--   :: ( MonadReader env m
--      , HasModifiersFor env ()
--      , HasCostPayment env
--      , HasSet Trait env Source
--      )
--   => InvestigatorId
--   -> Ability
--   -> m Bool
-- getCanAffordAbilityCost iid Ability {..} = case abilityType of
--   ActionAbility mAction cost -> getCanAffordCost iid abilitySource mAction cost
--   ActionAbilityWithSkill mAction _ cost ->
--     getCanAffordCost iid abilitySource mAction cost
--   ActionAbilityWithBefore mAction mBeforeAction cost -> liftA2
--     (||)
--     (getCanAffordCost iid abilitySource mAction cost)
--     (getCanAffordCost iid abilitySource mBeforeAction cost)
--   ReactionAbility _ cost -> getCanAffordCost iid abilitySource Nothing cost
--   FastAbility cost -> getCanAffordCost iid abilitySource Nothing cost
--   ForcedAbility -> pure True
--   AbilityEffect _ -> pure True
--
-- getCanAffordUse
--   :: (MonadReader env m, HasCostPayment env, HasList UsedAbility env ())
--   => InvestigatorId
--   -> Ability
--   -> m Bool
-- getCanAffordUse iid ability = case abilityLimit ability of
--   NoLimit -> case abilityType ability of
--     ReactionAbility _ _ ->
--       notElem (iid, ability) . map unUsedAbility <$> getList ()
--     ForcedAbility -> notElem (iid, ability) . map unUsedAbility <$> getList ()
--     ActionAbility _ _ -> pure True
--     ActionAbilityWithBefore{} -> pure True
--     ActionAbilityWithSkill{} -> pure True
--     FastAbility _ -> pure True
--     AbilityEffect _ -> pure True
--   PlayerLimit (PerSearch (Just _)) n ->
--     (< n)
--       . count ((== abilityLimit ability) . abilityLimit . snd . unUsedAbility)
--       <$> getList ()
--   PlayerLimit _ n ->
--     (< n) . count (== (iid, ability)) . map unUsedAbility <$> getList ()
--   PerInvestigatorLimit _ _ n -> do
--     usedAbilities <- map unUsedAbility <$> getList ()
--     let
--       matchingAbilities = filter (== (iid, ability)) usedAbilities
--       matchingPerInvestigatorCount =
--         count ((== abilityLimit ability) . abilityLimit . snd) matchingAbilities
--     pure $ matchingPerInvestigatorCount < n
--   GroupLimit _ n ->
--     (< n) . count (== ability) . map (snd . unUsedAbility) <$> getList ()

applyActionCostModifier :: Maybe Action -> ModifierType -> Int -> Int
applyActionCostModifier (Just action) (ActionCostOf (IsAction action') m) n
  | action == action' = n + m
applyActionCostModifier _ (ActionCostModifier m) n = n + m
applyActionCostModifier _ _ n = n

getCanAffordCost
  :: ( MonadReader env m
     , HasModifiersFor env ()
     , HasCostPayment env
     , HasSet Trait env Source
     )
  => InvestigatorId
  -> Source
  -> Maybe Action
  -> [Window]
  -> Cost
  -> m Bool
getCanAffordCost iid source mAction windows = \case
  Free -> pure True
  UpTo{} -> pure True
  AdditionalActionsCost{} -> pure True
  Costs xs -> and <$> traverse (getCanAffordCost iid source mAction windows) xs
  DiscardDrawnCardCost -> case windows of
    [Window _ (Window.DrawCard iid' c)] | iid == iid' ->
      elem (DiscardableHandCard c) <$> getList iid
    _ -> pure False
  ExhaustCost target -> case target of
    AssetTarget aid -> do
      readyAssetIds <- selectList AssetReady
      pure $ aid `elem` readyAssetIds
    _ -> error "Not handled ExhaustCost"
  ExhaustThis -> case source of
    AssetSource aid -> do
      readyAssetIds <- selectList AssetReady
      pure $ aid `elem` readyAssetIds
    _ -> error "Not handled ExhaustThis"
  ExhaustAssetCost matcher -> notNull <$> select (matcher <> AssetReady)
  UseCost aid _uType n -> do
    uses <- unUsesCount <$> getCount aid
    pure $ uses >= n
  ActionCost n -> do
    modifiers <- getModifiers source (InvestigatorTarget iid)
    if ActionsAreFree `elem` modifiers
      then pure True
      else do
        let
          modifiedActionCost =
            foldr (applyActionCostModifier mAction) n modifiers
        traits <- getSetList @Trait source
        actionCount <- unActionRemainingCount
          <$> getCount (mAction, traits, iid)
        pure $ actionCount >= modifiedActionCost
  ClueCost n -> do
    spendableClues <- unSpendableClueCount <$> getCount iid
    pure $ spendableClues >= n
  PlaceClueOnLocationCost n -> do
    spendableClues <- unSpendableClueCount <$> getCount iid
    pure $ spendableClues >= n
  GroupClueCost n Nothing -> do
    totalSpendableClues <- unSpendableClueCount <$> getCount ()
    cost <- getPlayerCountValue n
    pure $ totalSpendableClues >= cost
  GroupClueCost n (Just locationMatcher) -> do
    mLocationId <- getId @(Maybe LocationId) locationMatcher
    cost <- getPlayerCountValue n
    case mLocationId of
      Just lid -> do
        iids <- getSetList @InvestigatorId lid
        totalSpendableClues <- sum
          <$> for iids ((unSpendableClueCount <$>) . getCount)
        pure $ totalSpendableClues >= cost
      Nothing -> pure False
  ResourceCost n -> do
    resources <- unResourceCount <$> getCount iid
    pure $ resources >= n
  DiscardCost _ -> pure True -- TODO: Make better
  DiscardCardCost _ -> pure True -- TODO: Make better
  ExileCost _ -> pure True -- TODO: Make better
  RemoveCost _ -> pure True -- TODO: Make better
  HorrorCost{} -> pure True -- TODO: Make better
  DamageCost{} -> pure True -- TODO: Make better
  DirectDamageCost{} -> pure True -- TODO: Make better
  DoomCost{} -> pure True -- TODO: Make better
  SkillIconCost n skillTypes -> do
    handCards <- mapMaybe (preview _PlayerCard) <$> getHandOf iid
    let
      total = sum $ map
        (count (`member` insertSet SkillWild skillTypes) . cdSkills . toCardDef)
        handCards
    pure $ total >= n
  HandDiscardCost n mCardType traits skillTypes -> do
    cards <- mapMaybe (preview _PlayerCard) <$> getHandOf iid
    let
      cardTypeFilter = case mCardType of
        Nothing -> const True
        Just cardType' -> (== cardType') . cdCardType . toCardDef
      traitFilter = if null traits
        then const True
        else notNull . intersect traits . toTraits
      skillTypeFilter = if null skillTypes
        then const True
        else
          not
          . null
          . intersect (insertSet SkillWild skillTypes)
          . setFromList
          . cdSkills
          . toCardDef
    pure
      $ length
          (filter
            (and . sequence [traitFilter, cardTypeFilter, skillTypeFilter])
            cards
          )
      >= n

isForcedAction :: Ability -> Bool
isForcedAction ability = case abilityType ability of
  ForcedAbility _ -> True
  _ -> False

enemyAtInvestigatorLocation
  :: ( MonadReader env m
     , HasId CardCode env EnemyId
     , HasId LocationId env InvestigatorId
     , HasSet EnemyId env LocationId
     )
  => CardCode
  -> InvestigatorId
  -> m Bool
enemyAtInvestigatorLocation cardCode iid = do
  lid <- getId @LocationId iid
  enemyIds <- getSetList @EnemyId lid
  elem cardCode <$> for enemyIds (getId @CardCode)

getHasRecord :: (HasRecord env, MonadReader env m) => CampaignLogKey -> m Bool
getHasRecord = hasRecord

getRecordCount :: (HasRecord env, MonadReader env m) => CampaignLogKey -> m Int
getRecordCount = hasRecordCount

getRecordSet
  :: (HasRecord env, MonadReader env m) => CampaignLogKey -> m [CardCode]
getRecordSet = hasRecordSet

getIsUnused'
  :: (HasList UsedAbility env (), MonadReader env m)
  => InvestigatorId
  -> Ability
  -> m Bool
getIsUnused' iid ability = notElem ability' . map unUsedAbility <$> getList ()
  where ability' = (iid, ability)

getGroupIsUnused
  :: (MonadReader env m, HasList UsedAbility env ()) => Ability -> m Bool
getGroupIsUnused ability =
  notElem ability . map (snd . unUsedAbility) <$> getList ()

getInvestigatorModifiers
  :: (MonadReader env m, HasModifiersFor env ())
  => InvestigatorId
  -> Source
  -> m [ModifierType]
getInvestigatorModifiers iid source =
  getModifiers source (InvestigatorTarget iid)

getXp
  :: ( HasCount XPCount env ()
     , HasModifiersFor env ()
     , HasSet InvestigatorId env ()
     , MonadReader env m
     )
  => m [(InvestigatorId, Int)]
getXp = do
  investigatorIds <- getInvestigatorIds
  for
    investigatorIds
    \iid -> do
      modifiers' <- getModifiers
        (InvestigatorSource iid)
        (InvestigatorTarget iid)
      amount <- unXPCount <$> getCount ()
      pure (iid, foldl' applyModifier amount modifiers')
 where
  applyModifier n (XPModifier m) = max 0 (n + m)
  applyModifier n _ = n

getLeadInvestigatorId
  :: (HasId LeadInvestigatorId env (), MonadReader env m) => m InvestigatorId
getLeadInvestigatorId = unLeadInvestigatorId <$> getId ()

getInvestigatorIds
  :: (HasSet InvestigatorId env (), MonadReader env m) => m [InvestigatorId]
getInvestigatorIds = getSetList ()

getPlayerCount :: (HasCount PlayerCount env (), MonadReader env m) => m Int
getPlayerCount = unPlayerCount <$> getCount ()

getPlayerCountValue
  :: (HasCount PlayerCount env (), MonadReader env m) => GameValue Int -> m Int
getPlayerCountValue gameValue = fromGameValue gameValue <$> getPlayerCount

getLocationSet
  :: (HasSet LocationId env (), MonadReader env m) => m (HashSet LocationId)
getLocationSet = getSet ()

getSpendableClueCount
  :: (MonadReader env m, HasCount SpendableClueCount env InvestigatorId)
  => [InvestigatorId]
  -> m Int
getSpendableClueCount investigatorIds =
  sum <$> for investigatorIds ((unSpendableClueCount <$>) . getCount)

-- TODO: canFight _ a@Attrs {..} = canDo Action.Fight a
getCanFight
  :: ( MonadReader env m
     , HasCostPayment env
     , HasSet InvestigatorId env EnemyId
     , HasSet Keyword env EnemyId
     , HasSet Trait env Source
     , HasId LocationId env InvestigatorId
     , HasId LocationId env EnemyId
     , HasModifiersFor env ()
     )
  => EnemyId
  -> InvestigatorId
  -> m Bool
getCanFight eid iid = do
  locationId <- getId @LocationId iid
  enemyModifiers <- getModifiers (InvestigatorSource iid) (EnemyTarget eid)
  sameLocation <- (== locationId) <$> getId @LocationId eid
  modifiers' <- getModifiers (EnemySource eid) (InvestigatorTarget iid)
  takenActions <- setFromList . map unTakenAction <$> getList iid
  keywords <- getSet eid
  canAffordActions <- getCanAffordCost
    iid
    (EnemySource eid)
    (Just Action.Fight)
    []
    (foldl' (applyFightCostModifiers takenActions) (ActionCost 1) modifiers')
  engagedInvestigators <- getSet eid
  pure
    $ canAffordActions
    && (Keyword.Aloof `notMember` keywords || iid `member` engagedInvestigators)
    && (sameLocation || CanBeFoughtAsIfAtYourLocation `elem` enemyModifiers)
 where
  applyFightCostModifiers
    :: HashSet Action.Action -> Cost -> ModifierType -> Cost
  applyFightCostModifiers takenActions costToFight (ActionCostOf actionTarget n)
    = case actionTarget of
      FirstOneOf as
        | Action.Fight `elem` as && null
          (takenActions `intersect` setFromList as)
        -> increaseActionCost costToFight n
      IsAction Action.Fight -> increaseActionCost costToFight n
      _ -> costToFight
  applyFightCostModifiers _ costToFight _ = costToFight

getCanEngage
  :: ( MonadReader env m
     , HasCostPayment env
     , HasSet InvestigatorId env EnemyId
     , HasSet Trait env Source
     , HasId LocationId env InvestigatorId
     , HasId LocationId env EnemyId
     , HasModifiersFor env ()
     )
  => EnemyId
  -> InvestigatorId
  -> m Bool
getCanEngage eid iid = do
  locationId <- getId @LocationId iid
  sameLocation <- (== locationId) <$> getId @LocationId eid
  notEngaged <- notElem iid <$> getSet eid
  canAffordActions <- getCanAffordCost
    iid
    (EnemySource eid)
    (Just Action.Engage)
    []
    (ActionCost 1)
  pure $ notEngaged && canAffordActions && sameLocation

getCanEvade
  :: ( MonadReader env m
     , HasCostPayment env
     , HasSet InvestigatorId env EnemyId
     , HasSet Trait env Source
     , HasModifiersFor env ()
     )
  => EnemyId
  -> InvestigatorId
  -> m Bool
getCanEvade eid iid = do
  engaged <- elem iid <$> getSet eid
  enemyModifiers <- getModifiers (InvestigatorSource iid) (EnemyTarget eid)
  modifiers' <- getModifiers (EnemySource eid) (InvestigatorTarget iid)
  takenActions <- setFromList . map unTakenAction <$> getList iid
  canAffordActions <- getCanAffordCost
    iid
    (EnemySource eid)
    (Just Action.Evade)
    []
    (foldl' (applyEvadeCostModifiers takenActions) (ActionCost 1) modifiers')
  pure $ engaged && canAffordActions && CannotBeEvaded `notElem` enemyModifiers
 where
  applyEvadeCostModifiers
    :: HashSet Action.Action -> Cost -> ModifierType -> Cost
  applyEvadeCostModifiers takenActions costToFight (ActionCostOf actionTarget n)
    = case actionTarget of
      FirstOneOf as
        | Action.Evade `elem` as && null
          (takenActions `intersect` setFromList as)
        -> increaseActionCost costToFight n
      IsAction Action.Evade -> increaseActionCost costToFight n
      _ -> costToFight
  applyEvadeCostModifiers _ costToFight _ = costToFight

getCanMoveTo
  :: ( MonadReader env m
     , HasCostPayment env
     , HasSet AccessibleLocationId env LocationId
     , HasSet Trait env Source
     , HasId LocationId env InvestigatorId
     , HasModifiersFor env ()
     , HasCallStack
     )
  => LocationId
  -> InvestigatorId
  -> m Bool
getCanMoveTo lid iid = do
  locationId <- getId @LocationId iid
  modifiers' <- getModifiers (LocationSource lid) (InvestigatorTarget iid)
  locationModifiers' <- getModifiers
    (InvestigatorSource iid)
    (LocationTarget lid)
  accessibleLocations <- map unAccessibleLocationId <$> getSetList locationId
  canAffordActions <- getCanAffordCost
    iid
    (LocationSource lid)
    (Just Action.Move)
    []
    (ActionCost 1)
  pure
    $ lid
    `elem` accessibleLocations
    && canAffordActions
    && lid
    /= locationId
    && CannotMove
    `notElem` modifiers'
    && Blocked
    `notElem` locationModifiers'

getResourceCount
  :: (MonadReader env m, HasCount ResourceCount env InvestigatorId)
  => InvestigatorId
  -> m Int
getResourceCount iid = unResourceCount <$> getCount iid

getDiscardOf
  :: (MonadReader env m, HasList DiscardedPlayerCard env InvestigatorId)
  => InvestigatorId
  -> m [PlayerCard]
getDiscardOf iid = map unDiscardedPlayerCard <$> getList iid

getHandOf
  :: (MonadReader env m, HasList HandCard env InvestigatorId)
  => InvestigatorId
  -> m [Card]
getHandOf iid = map unHandCard <$> getList iid

getInPlayOf
  :: (MonadReader env m, HasList InPlayCard env InvestigatorId)
  => InvestigatorId
  -> m [Card]
getInPlayOf iid = map unInPlayCard <$> getList iid

getCardCount
  :: (MonadReader env m, HasCount CardCount env InvestigatorId)
  => InvestigatorId
  -> m Int
getCardCount iid = unCardCount <$> getCount iid

toModifier :: SourceEntity a => a -> ModifierType -> Modifier
toModifier = Modifier . toSource

toModifiers :: SourceEntity a => a -> [ModifierType] -> [Modifier]
toModifiers = map . toModifier

targetToSource :: Target -> Source
targetToSource = \case
  InvestigatorTarget iid -> InvestigatorSource iid
  AssetTarget aid -> AssetSource aid
  EnemyTarget eid -> EnemySource eid
  ScenarioTarget sid -> ScenarioSource sid
  EffectTarget eid -> EffectSource eid
  PhaseTarget _ -> error "no need"
  LocationTarget lid -> LocationSource lid
  (SetAsideLocationsTarget _) -> error "can not convert"
  SkillTestTarget -> error "can not convert"
  AfterSkillTestTarget -> AfterSkillTestSource
  TreacheryTarget tid -> TreacherySource tid
  EncounterDeckTarget -> error "can not covert"
  ScenarioDeckTarget -> error "can not covert"
  AgendaTarget aid -> AgendaSource aid
  ActTarget aid -> ActSource aid
  CardIdTarget _ -> error "can not convert"
  CardCodeTarget _ -> error "can not convert"
  SearchedCardTarget _ -> error "can not convert"
  EventTarget eid -> EventSource eid
  SkillTarget sid -> SkillSource sid
  SkillTestInitiatorTarget _ -> error "can not convert"
  TokenTarget tid -> TokenSource tid
  TokenFaceTarget _ -> error "Not convertable"
  TestTarget -> TestSource mempty
  ResourceTarget -> ResourceSource
  ActDeckTarget -> ActDeckSource
  AgendaDeckTarget -> AgendaDeckSource
  InvestigationTarget{} -> error "not converted"
  YouTarget -> YouSource
  NoTarget -> NoSource

sourceToTarget :: Source -> Target
sourceToTarget = \case
  AssetSource aid -> AssetTarget aid
  EnemySource eid -> EnemyTarget eid
  CardIdSource cid -> CardIdTarget cid
  ScenarioSource sid -> ScenarioTarget sid
  InvestigatorSource iid -> InvestigatorTarget iid
  CardCodeSource cid -> CardCodeTarget cid
  TokenSource t -> TokenTarget t
  TokenEffectSource _ -> error "not implemented"
  AgendaSource aid -> AgendaTarget aid
  LocationSource lid -> LocationTarget lid
  SkillTestSource{} -> SkillTestTarget
  AfterSkillTestSource -> AfterSkillTestTarget
  TreacherySource tid -> TreacheryTarget tid
  EventSource eid -> EventTarget eid
  SkillSource sid -> SkillTarget sid
  EmptyDeckSource -> error "not implemented"
  DeckSource -> error "not implemented"
  GameSource -> error "not implemented"
  ActSource aid -> ActTarget aid
  PlayerCardSource cid -> CardIdTarget cid
  EncounterCardSource _ -> error "not implemented"
  TestSource{} -> TestTarget
  ProxySource _ source -> sourceToTarget source
  AssetProxySource _ source -> sourceToTarget source
  EffectSource eid -> EffectTarget eid
  ResourceSource -> ResourceTarget
  AbilitySource{} -> error "not implemented"
  ActDeckSource -> ActDeckTarget
  AgendaDeckSource -> AgendaDeckTarget
  YouSource -> YouTarget
  NoSource -> NoTarget

addCampaignCardToDeckChoice
  :: InvestigatorId -> [InvestigatorId] -> CardDef -> Message
addCampaignCardToDeckChoice leadInvestigatorId investigatorIds cardDef =
  chooseOne
    leadInvestigatorId
    [ Label
      ("Add " <> display name <> " to a deck")
      [ chooseOne
          leadInvestigatorId
          [ TargetLabel
              (InvestigatorTarget iid)
              [AddCampaignCardToDeck iid cardDef]
          | iid <- investigatorIds
          ]
      ]
    , Label ("Do not add " <> display name <> " to any deck") []
    ]
  where name = cdName cardDef

skillTestModifier
  :: (SourceEntity source, TargetEntity target)
  => source
  -> target
  -> ModifierType
  -> Message
skillTestModifier source target modifier =
  skillTestModifiers source target [modifier]

skillTestModifiers
  :: (SourceEntity source, TargetEntity target)
  => source
  -> target
  -> [ModifierType]
  -> Message
skillTestModifiers source target modifiers = CreateWindowModifierEffect
  EffectSkillTestWindow
  (EffectModifiers $ toModifiers source modifiers)
  (toSource source)
  (toTarget target)

getJustLocationIdByName
  :: (MonadReader env m, HasId (Maybe LocationId) env LocationMatcher)
  => Name
  -> m LocationId
getJustLocationIdByName name =
  fromJustNote ("Missing " <> show name) <$> getLocationIdByName name

getLocationIdByName
  :: (MonadReader env m, HasId (Maybe LocationId) env LocationMatcher)
  => Name
  -> m (Maybe LocationId)
getLocationIdByName name = getId matcher
 where
  matcher = case (nameTitle name, nameSubtitle name) of
    (title, Just subtitle) -> LocationWithFullTitle title subtitle
    (title, Nothing) -> LocationWithTitle title

fightAction :: SourceEntity source => source -> Int -> [Cost] -> Ability
fightAction source n costs =
  mkAbility source n (ActionAbility (Just Action.Fight) (Costs costs))

hasFightActions
  :: (MonadIO m, MonadReader env m, Query ActionMatcher env)
  => InvestigatorId
  -> WindowMatcher
  -> m Bool
hasFightActions _ window =
  notNull <$> select (ActionIs Action.Fight <> ActionWindow window)

hasEvadeActions
  :: (MonadIO m, MonadReader env m, Query ActionMatcher env)
  => InvestigatorId
  -> WindowMatcher
  -> m Bool
hasEvadeActions _ window =
  notNull <$> select (ActionIs Action.Evade <> ActionWindow window)

hasInvestigateActions
  :: forall env m
   . (MonadIO m, MonadReader env m, Query ActionMatcher env)
  => InvestigatorId
  -> WindowMatcher
  -> m Bool
hasInvestigateActions _ window = do
  notNull <$> select (ActionIs Action.Evade <> ActionWindow window)

type CanCheckPlayable env
  = ( HasModifiersFor env ()
    , HasCostPayment env
    , HasSet Trait env Source
    , HasSkillTest env
    , HasSet SetAsideCardId env CardMatcher
    , Query AssetMatcher env
    , Query LocationMatcher env
    , Query InvestigatorMatcher env
    , Query EnemyMatcher env
    , Query ExtendedCardMatcher env
    , HasCount ActionRemainingCount env (Maybe Action, [Trait], InvestigatorId)
    , HasCount SpendableClueCount env ()
    , HasCount SpendableClueCount env InvestigatorId
    , HasCount ClueCount env AssetId
    , CanCheckFast env
    , HasId LocationId env AssetId
    , HasId (Maybe OwnerId) env AssetId
    , HasSet ClassSymbol env AssetId
    , HasList HandCard env InvestigatorId
    , HasList Card env ExtendedCardMatcher
    , HasCount ActionTakenCount env InvestigatorId
    , HasSet InvestigatorId env LocationId
    , HasSet EnemyId env LocationId
    , HasSet EnemyId env EnemyMatcher
    , HasSet LocationId env LocationMatcher
    , HasSet Trait env EnemyId
    , HasSet Trait env EnemyId
    , HasCount ClueCount env LocationId
    , HasCount ResourceCount env LocationId
    , Query ActionMatcher env
    , HasSet EnemyId env InvestigatorId
    , HasCount ResourceCount env InvestigatorId
    , HasCount DoomCount env AssetId
    , HasCount DoomCount env InvestigatorId
    , HasList DiscardedPlayerCard env InvestigatorId
    , HasSet InvestigatorId env InvestigatorMatcher
    , HasSet AssetId env AssetMatcher
    , HasSet InvestigatorId env ()
    )

getIsPlayable
  :: (HasCallStack, MonadReader env m, MonadIO m, CanCheckPlayable env)
  => InvestigatorId
  -> [Window]
  -> Card
  -> m Bool
getIsPlayable iid windows c = do
  availableResources <- unResourceCount <$> getCount iid
  getIsPlayableWithResources iid availableResources windows c

getIsPlayableWithResources
  :: (HasCallStack, MonadReader env m, MonadIO m, CanCheckPlayable env)
  => InvestigatorId
  -> Int
  -> [Window]
  -> Card
  -> m Bool
getIsPlayableWithResources _ _ _ (EncounterCard _) = pure False -- TODO: there might be some playable ones?
getIsPlayableWithResources iid availableResources windows c@(PlayerCard _) = do
  modifiers <- getModifiers (InvestigatorSource iid) (InvestigatorTarget iid)
  let
    notFastWindow = any (`elem` windows) [Window When (Window.DuringTurn iid)]
  modifiedCardCost <- getModifiedCardCost iid c
  passesRestrictions <- maybe
    (pure True)
    (passesRestriction iid (CardIdSource $ toCardId c) windows)
    (cdPlayRestrictions pcDef)
  inFastWindow <- maybe
    (pure False)
    (cardInFastWindows iid c windows)
    (cdFastWindow pcDef)
  canEvade <- hasEvadeActions iid (Matcher.DuringTurn You)
  canFight <- hasFightActions iid (Matcher.DuringTurn You)
  passesLimits <- allM passesLimit (cdLimits pcDef)
  pure
    $ (cdCardType pcDef /= SkillType)
    && (modifiedCardCost <= availableResources)
    && none prevents modifiers
    && ((isNothing (cdFastWindow pcDef) && notFastWindow) || inFastWindow)
    && (cdAction pcDef /= Just Action.Evade || canEvade)
    && (cdAction pcDef /= Just Action.Fight || canFight)
    && passesRestrictions
    && passesLimits
 where
  pcDef = toCardDef c
  prevents (CanOnlyUseCardsInRole role) =
    cdClassSymbol pcDef `notElem` [Just Neutral, Just role, Nothing]
  prevents (CannotPlay typePairs) = any
    (\(cType, traits) ->
      cdCardType pcDef
        == cType
        && (null traits || notNull (intersection (toTraits pcDef) traits))
    )
    typePairs
  prevents _ = False
  passesLimit (LimitPerInvestigator m) = case toCardType c of
    AssetType -> do
      n <- size <$> getSet @AssetId
        (AssetOwnedBy (InvestigatorWithId iid)
        <> AssetWithTitle (nameTitle $ toName c)
        )
      pure $ m > n
    _ -> error $ "Not handling card type: " <> show (toCardType c)
  passesLimit (LimitPerTrait t m) = case toCardType c of
    AssetType -> do
      n <- size <$> getSet @AssetId (AssetWithTrait t)
      pure $ m > n
    _ -> error $ "Not handling card type: " <> show (toCardType c)

passesRestriction
  :: ( HasCallStack
     , MonadReader env m
     , CanCheckFast env
     , CanCheckPlayable env
     , MonadIO m
     )
  => InvestigatorId
  -> Source
  -> [Window]
  -> Restriction
  -> m Bool
passesRestriction iid source windows = \case
  Negate restriction ->
    not <$> passesRestriction iid source windows restriction
  OwnsThis -> case source of
    AssetSource aid -> do
      mOwner <- getId aid
      pure $ Just (OwnerId iid) == mOwner
    _ -> error "missing OwnsThis check"
  DuringSkillTest -> isJust <$> getSkillTest
  CluesOnThis valueMatcher -> case source of
    AssetSource aid -> do
      (`gameValueMatches` valueMatcher) . unClueCount =<< getCount aid
    _ -> error "missing CluesOnThis check"
  Unowned -> case source of
    AssetSource aid -> do
      mOwner <- getId @(Maybe OwnerId) aid
      pure $ isNothing mOwner
    _ -> error "missing OwnsThis check"
  OnSameLocation -> case source of
    AssetSource aid -> do
      liftA2 (==) (getId @LocationId aid) (getId @LocationId iid)
    _ -> error "missing OnSameLocation check"
  CardExists cardMatcher -> notNull <$> getList @Card cardMatcher
  ExtendedCardExists cardMatcher -> notNull <$> getList @Card cardMatcher
  PlayableCardExists cardMatcher -> do
    results <- getList @Card cardMatcher
    anyM (getIsPlayable iid windows) results
  FirstAction -> do
    n <- unActionTakenCount <$> getCount iid
    pure $ n == 0
  NoRestriction -> pure True
  OnLocation lid -> (lid ==) <$> getId iid
  ReturnableCardInDiscard AnyPlayerDiscard traits -> do
    investigatorIds <-
      filterM
          (fmap (notElem CardsCannotLeaveYourDiscardPile)
          . getModifiers GameSource
          . InvestigatorTarget
          )
        =<< getInvestigatorIds
    discards <-
      concat
        <$> traverse
              (fmap (map unDiscardedPlayerCard) . getList)
              investigatorIds
    let
      filteredDiscards = case traits of
        [] -> discards
        traitsToMatch ->
          filter (any (`elem` traitsToMatch) . toTraits) discards
    pure $ notNull filteredDiscards
  CardInDiscard AnyPlayerDiscard traits -> do
    investigatorIds <- getInvestigatorIds
    discards <-
      concat
        <$> traverse
              (fmap (map unDiscardedPlayerCard) . getList)
              investigatorIds
    let
      filteredDiscards = case traits of
        [] -> discards
        traitsToMatch ->
          filter (any (`elem` traitsToMatch) . toTraits) discards
    pure $ notNull filteredDiscards
  ClueOnLocation -> do
    location <- getId iid
    liftA2
      (&&)
      (pure $ location /= LocationId (CardId nil))
      ((> 0) . unClueCount <$> getCount location)
  EnemyExists matcher -> notNull <$> getSet @EnemyId matcher
  SetAsideCardExists matcher -> notNull <$> getSet @SetAsideCardId matcher
  NoEnemyExists matcher -> null <$> getSet @EnemyId matcher
  AssetExists matcher -> notNull <$> getSet @AssetId matcher
  InvestigatorExists matcher -> notNull <$> getSet @InvestigatorId matcher
  Restrictions rs -> allM (passesRestriction iid source windows) rs
  AnyRestriction rs -> anyM (passesRestriction iid source windows) rs
  LocationExists matcher -> notNull <$> getSet @LocationId matcher
  InvestigatorsHaveSpendableClues valueMatcher ->
    (`gameValueMatches` valueMatcher) . unSpendableClueCount =<< getCount ()
  AnotherInvestigatorInSameLocation -> do
    location <- getId iid
    liftA2
      (&&)
      (pure $ location /= LocationId (CardId nil))
      (notNull <$> getSet @InvestigatorId location)
  InvestigatorIsAlone -> do
    location <- getId iid
    liftA2
      (&&)
      (pure $ location /= LocationId (CardId nil))
      ((== 1) . size <$> getSet @InvestigatorId location)
  OwnCardWithDoom -> do
    assetIds <- selectList (AssetOwnedBy You)
    investigatorDoomCount <- unDoomCount <$> getCount iid
    assetsWithDoomCount <- filterM
      (fmap ((> 0) . unDoomCount) . getCount)
      assetIds
    pure $ investigatorDoomCount > 0 || notNull assetsWithDoomCount
  ScenarioCardHasResignAbility -> do
    notNull <$> select (ActionIs Action.Resign <> ActionOnScenarioCard)

getModifiedCardCost
  :: (MonadReader env m, HasModifiersFor env (), HasList Card env CardMatcher)
  => InvestigatorId
  -> Card
  -> m Int
getModifiedCardCost iid c@(PlayerCard _) = do
  modifiers <- getModifiers (InvestigatorSource iid) (InvestigatorTarget iid)
  foldM applyModifier startingCost modifiers
 where
  pcDef = toCardDef c
  startingCost = case cdCost pcDef of
    Just (StaticCost n) -> n
    Just DynamicCost -> 0
    Nothing -> 0
  applyModifier n (ReduceCostOf cardMatcher m) = do
    matches <- getList @Card cardMatcher
    pure $ if c `elem` matches then max 0 (n - m) else n
  applyModifier n _ = pure n
getModifiedCardCost iid c@(EncounterCard _) = do
  modifiers <- getModifiers (InvestigatorSource iid) (InvestigatorTarget iid)
  foldM
    applyModifier
    (error "we need so specify ecCost for this to work")
    modifiers
 where
  applyModifier n (ReduceCostOf cardMatcher m) = do
    matches <- getList @Card cardMatcher
    pure $ if c `elem` matches then max 0 (n - m) else n
  applyModifier n _ = pure n

type CanCheckFast env
  = ( HasSet Trait env EnemyId
    , HasSet InvestigatorId env Who
    , HasList UnderneathCard env InvestigatorId
    , HasList Card env CardMatcher
    , HasCount DamageCount env InvestigatorId
    , HasCount HorrorCount env InvestigatorId
    , HasCount ClueCount env LocationId
    , HasSet AccessibleLocationId env LocationId
    , HasSet ConnectedLocationId env LocationId
    , HasSet InvestigatorId env LocationId
    , HasSet RevealedLocationId env ()
    , HasSet InvestigatorId env EnemyId
    , HasSet EnemyId env LocationId
    , HasSet TreacheryId env LocationId
    , HasId LocationId env InvestigatorId
    , HasId LocationId env EnemyId
    , HasId CardCode env TreacheryId
    , HasId CardCode env EnemyId
    , HasId CardCode env LocationId
    , HasSet Trait env LocationId
    , HasSet Keyword env EnemyId
    , HasSet FarthestLocationId env (InvestigatorId, LocationMatcher)
    , HasName env LocationId
    , HasName env EnemyId
    , HasCount PlayerCount env ()
    , Location.GetLabel env LocationId
    , HasTokenValue env ()
    )

depthGuard :: IORef Int
depthGuard = unsafePerformIO $ newIORef 0
{-# NOINLINE depthGuard #-}

cardInFastWindows
  :: forall env m
   . (MonadReader env m, CanCheckPlayable env, MonadIO m)
  => InvestigatorId
  -> Card
  -> [Window]
  -> WindowMatcher
  -> m Bool
cardInFastWindows iid _ windows matcher =
  anyM (\window -> windowMatches iid window matcher) windows


windowMatches
  :: forall env m
   . (MonadReader env m, CanCheckPlayable env, MonadIO m)
  => InvestigatorId
  -> Window
  -> WindowMatcher
  -> m Bool
windowMatches iid window' = \case
  Matcher.AnyWindow -> pure True
  Matcher.PlayerHasPlayableCard cardMatcher -> do
    cards <- getList cardMatcher -- TODO: this will likely cause infinite recursion
    anyM (getIsPlayable iid [window']) cards
  Matcher.Enters whenMatcher whoMatcher whereMatcher -> case window' of
    Window t (Window.Entering iid' lid) | whenMatcher == t -> liftA2
      (&&)
      (matchWho iid iid' whoMatcher)
      (locationMatches iid lid whereMatcher)
    _ -> pure False
  Matcher.PhaseBegins whenMatcher phaseMatcher -> case window' of
    Window t Window.AnyPhaseBegins | whenMatcher == t ->
      pure $ phaseMatcher == Matcher.AnyPhase
    Window t (Window.PhaseBegins p) | whenMatcher == t ->
      matchPhase p phaseMatcher
    _ -> pure False
  Matcher.PhaseEnds whenMatcher phaseMatcher -> case window' of
    Window t (Window.PhaseEnds p) | whenMatcher == t ->
      matchPhase p phaseMatcher
    _ -> pure False
  Matcher.TurnBegins whenMatcher whoMatcher -> case window' of
    Window t (Window.TurnBegins who) | whenMatcher == t ->
      matchWho iid who whoMatcher
    _ -> pure False
  Matcher.WouldHaveSkillTestResult timing whoMatcher _ skillTestResultMatcher
    -> case skillTestResultMatcher of
      Matcher.FailureResult _ -> case window' of
        Window t (Window.WouldFailSkillTest who) | t == timing ->
          matchWho iid who whoMatcher
        _ -> pure False
      Matcher.SuccessResult _ -> pure False -- no pass window exists yet, add below too if added
      Matcher.AnyResult -> case window' of
        Window When (Window.WouldFailSkillTest who) ->
          matchWho iid who whoMatcher
        -- TODO: Add success window if it exists
        _ -> pure False
  Matcher.SkillTestResult whenMatcher whoMatcher skillMatcher skillTestResultMatcher
    -> case skillTestResultMatcher of
      Matcher.FailureResult gameValueMatcher -> case window' of
        Window t (Window.FailInvestigationSkillTest who n)
          | whenMatcher == t && skillMatcher == Matcher.WhileInvestigating -> liftA2
            (&&)
            (matchWho iid who whoMatcher)
            (gameValueMatches n gameValueMatcher)
        Window t (Window.FailSkillTest who n)
          | whenMatcher == t && skillMatcher == Matcher.AnySkillTest -> liftA2
            (&&)
            (matchWho iid who whoMatcher)
            (gameValueMatches n gameValueMatcher)
        _ -> pure False
      Matcher.SuccessResult gameValueMatcher
        | skillMatcher == Matcher.AnySkillTest -> case window' of
          Window t (Window.PassSkillTest _ _ who n) | whenMatcher == t -> liftA2
            (&&)
            (matchWho iid who whoMatcher)
            (gameValueMatches n gameValueMatcher)
          _ -> pure False
      Matcher.AnyResult -> case window' of
        Window t (Window.FailSkillTest who _) | whenMatcher == t ->
          matchWho iid who whoMatcher
        Window t (Window.PassSkillTest _ _ who _) | whenMatcher == t ->
          matchWho iid who whoMatcher
        _ -> pure False
      _ -> pure False
  Matcher.DuringTurn whoMatcher -> case window' of
    Window When (Window.DuringTurn who) -> matchWho iid who whoMatcher
    Window When Window.FastPlayerWindow -> do
      miid <- selectOne TurnInvestigator
      pure $ Just iid == miid
    _ -> pure False
  Matcher.OrWindowMatcher matchers -> anyM (windowMatches iid window') matchers
  Matcher.EnemyLeaves timingMatcher whereMatcher enemyMatcher ->
    case window' of
      Window t (Window.EnemyLeaves enemyId locationId) | timingMatcher == t ->
        liftA2
          (&&)
          (enemyMatches iid enemyId enemyMatcher)
          (locationMatches iid locationId whereMatcher)
      _ -> pure False
  Matcher.EnemySpawns timingMatcher whereMatcher enemyMatcher ->
    case window' of
      Window t (Window.EnemySpawns enemyId locationId) | timingMatcher == t ->
        liftA2
          (&&)
          (enemyMatches iid enemyId enemyMatcher)
          (locationMatches iid locationId whereMatcher)
      _ -> pure False
  Matcher.EnemyAttacks timingMatcher whoMatcher enemyMatcher -> case window' of
    Window t (Window.EnemyAttacks who enemyId) | timingMatcher == t -> liftA2
      (&&)
      (matchWho iid who whoMatcher)
      (enemyMatches iid enemyId enemyMatcher)
    _ -> pure False
  Matcher.EnemyEvaded timingMatcher whoMatcher enemyMatcher -> case window' of
    Window t (Window.EnemyEvaded who enemyId) | timingMatcher == t -> liftA2
      (&&)
      (enemyMatches iid enemyId enemyMatcher)
      (matchWho iid who whoMatcher)
    _ -> pure False
  Matcher.EnemyEngaged timingMatcher whoMatcher enemyMatcher -> case window' of
    Window t (Window.EnemyEngageInvestigator who enemyId)
      | timingMatcher == t -> liftA2
        (&&)
        (enemyMatches iid enemyId enemyMatcher)
        (matchWho iid who whoMatcher)
    _ -> pure False
  Matcher.MythosStep mythosStepMatcher -> case window' of
    Window t Window.AllDrawEncounterCard | t == When ->
      pure $ mythosStepMatcher == Matcher.WhenAllDrawEncounterCard
    _ -> pure False
  Matcher.WouldRevealChaosToken whenMatcher whoMatcher -> case window' of
    Window t (Window.WouldRevealChaosToken _ who) | whenMatcher == t ->
      matchWho iid who whoMatcher
    _ -> pure False
  Matcher.RevealChaosToken whenMatcher whoMatcher tokenMatcher ->
    case window' of
      Window t (Window.RevealToken who token) | whenMatcher == t -> liftA2
        (&&)
        (matchWho iid who whoMatcher)
        (matchToken who token tokenMatcher)
      _ -> pure False
  Matcher.EnemyDefeated timingMatcher whoMatcher enemyMatcher ->
    case window' of
      Window t (Window.EnemyDefeated who enemyId) | timingMatcher == t -> liftA2
        (&&)
        (enemyMatches iid enemyId enemyMatcher)
        (matchWho iid who whoMatcher)
      _ -> pure False
  Matcher.FastPlayerWindow -> case window' of
    Window When Window.FastPlayerWindow -> pure True
    _ -> pure False
  Matcher.DealtDamageOrHorror whoMatcher -> case whoMatcher of
    You -> case window' of
      Window _ (WouldTakeDamageOrHorror _ (InvestigatorTarget iid') _ _) ->
        pure $ iid == iid'
      _ -> pure False
    _ -> pure False
  Matcher.AssetDealtDamage timingMatcher assetMatcher -> case window' of
    Window t (DealtDamage _ (AssetTarget aid)) | t == timingMatcher ->
      member aid <$> select assetMatcher
    _ -> pure False
  Matcher.DrawCard whenMatcher whoMatcher cardMatcher -> case window' of
    Window t (Window.DrawCard who card) | whenMatcher == t -> liftA2
      (&&)
      (matchWho iid who whoMatcher)
      (member card <$> select cardMatcher)
    _ -> pure False
  Matcher.PlayCard whenMatcher whoMatcher cardMatcher -> case window' of
    Window t (Window.PlayCard who card) | whenMatcher == t -> liftA2
      (&&)
      (matchWho iid who whoMatcher)
      (member card <$> select cardMatcher)
    _ -> pure False
  Matcher.CommittedCards whenMatcher whoMatcher listMatcher -> case window' of
    Window t (Window.CommittedCards who cards) | whenMatcher == t -> liftA2
      (&&)
      (matchWho iid who whoMatcher)
      (pure $ matchList cards listMatcher)
    _ -> pure False
  Matcher.AssetEntersPlay timingMatcher assetMatcher -> case window' of
    Window t (Window.EnterPlay (AssetTarget aid)) | t == timingMatcher ->
      member aid <$> select assetMatcher
    _ -> pure False

matchWho
  :: (MonadReader env m, CanCheckPlayable env)
  => InvestigatorId
  -> InvestigatorId
  -> InvestigatorMatcher
  -> m Bool
matchWho you who = \case
  Anyone -> pure True
  You -> pure $ who == you
  NotYou -> pure $ who /= you
  UnengagedInvestigator -> null <$> getSet @EnemyId who
  TurnInvestigator -> do
    mTurn <- selectOne TurnInvestigator
    pure $ Just who == mTurn
  UneliminatedInvestigator -> member who <$> getSet UneliminatedInvestigator
  InvestigatorEngagedWith enemyMatcher -> do
    engagedInvestigators <- select (InvestigatorEngagedWith enemyMatcher)
    pure $ who `member` engagedInvestigators
  InvestigatorAt locationMatcher -> do
    lid <- getId @LocationId who
    member lid <$> select locationMatcher
  InvestigatorCanMove -> do
    notElem CannotMove
      <$> getModifiers (InvestigatorSource who) (InvestigatorTarget who)
  InvestigatorWithDamage valueMatcher ->
    (`gameValueMatches` valueMatcher) . unDamageCount =<< getCount who
  InvestigatorWithHorror valueMatcher ->
    (`gameValueMatches` valueMatcher) . unHorrorCount =<< getCount who
  InvestigatorWithId iid' -> pure $ who == iid'
  InvestigatorMatches is -> allM (matchWho you who) is
  AnyInvestigator is -> anyM (matchWho you who) is

gameValueMatches
  :: (MonadReader env m, HasCount PlayerCount env ())
  => Int
  -> ValueMatcher
  -> m Bool
gameValueMatches n = \case
  Matcher.AnyValue -> pure True
  Matcher.LessThan gv -> (n <) <$> getPlayerCountValue gv
  Matcher.GreaterThan gv -> (n >) <$> getPlayerCountValue gv
  Matcher.LessThanOrEqualTo gv -> (n <=) <$> getPlayerCountValue gv
  Matcher.GreaterThanOrEqualTo gv -> (n >=) <$> getPlayerCountValue gv
  Matcher.EqualTo gv -> (n ==) <$> getPlayerCountValue gv

enemyMatches
  :: (MonadReader env m, CanCheckPlayable env)
  => InvestigatorId
  -> EnemyId
  -> EnemyMatcher
  -> m Bool
enemyMatches _ enemyId mtchr = member enemyId <$> select mtchr

locationMatches
  :: (MonadReader env m, CanCheckPlayable env)
  => InvestigatorId
  -> LocationId
  -> LocationMatcher
  -> m Bool
locationMatches investigatorId locationId = \case
  LocationWithLabel label ->
    (== label) . Location.unLabel <$> Location.getLabel locationId
  LocationWithTitle title -> (== title) . nameTitle <$> getName locationId
  LocationWithFullTitle title subtitle ->
    (== Name title (Just subtitle)) <$> getName locationId
  LocationWithId lid -> pure $ lid == locationId
  LocationIs cardCode -> (== cardCode) <$> getId locationId
  Anywhere -> pure True
  EmptyLocation -> liftA2
    (&&)
    (null <$> getSet @EnemyId locationId)
    (null <$> getSet @InvestigatorId locationId)
  LocationWithoutInvestigators -> null <$> getSet @InvestigatorId locationId
  LocationWithoutEnemies -> null <$> getSet @EnemyId locationId
  AccessibleLocation -> do
    yourLocationId <- getId @LocationId investigatorId
    member (AccessibleLocationId locationId) <$> getSet yourLocationId
  AccessibleFrom lid' -> do
    member (AccessibleLocationId locationId) <$> getSet lid'
  ConnectedLocation -> do
    yourLocationId <- getId @LocationId investigatorId
    member (ConnectedLocationId locationId) <$> getSet yourLocationId
  RevealedLocation -> member (RevealedLocationId locationId) <$> getSet ()
  LocationWithClues valueMatcher ->
    (`gameValueMatches` valueMatcher) . unClueCount =<< getCount locationId
  LocationWithResources valueMatcher ->
    (`gameValueMatches` valueMatcher) . unResourceCount =<< getCount locationId
  YourLocation -> do
    yourLocationId <- getId @LocationId investigatorId
    pure $ locationId == yourLocationId
  NotYourLocation -> do
    yourLocationId <- getId @LocationId investigatorId
    pure $ locationId /= yourLocationId
  FarthestLocationFromYou matcher' -> member (FarthestLocationId locationId)
    <$> getSet (investigatorId, matcher')
  LocationWithTrait t -> member t <$> getSet locationId
  LocationMatchers ms -> allM (locationMatches investigatorId locationId) ms
  FirstLocation ms -> anyM (locationMatches investigatorId locationId) ms -- a bit weird here since first means nothing
  LocationWithoutTreacheryWithCardCode cCode -> do
    treacheryIds <- getSetList @TreacheryId locationId
    cardCodes <- traverse (getId @CardCode) treacheryIds
    pure $ cCode `notElem` cardCodes
  InvestigatableLocation -> do
    modifiers <- getModifiers
      (InvestigatorSource investigatorId)
      (LocationTarget locationId)
    pure $ CannotInvestigate `notElem` modifiers

matchToken
  :: (HasTokenValue env (), MonadReader env m, MonadIO m)
  => InvestigatorId
  -> Token
  -> TokenMatcher
  -> m Bool
matchToken iid' t = \case
  Matcher.WithNegativeModifier -> do
    tv <- getTokenValue () iid' (tokenFace t)
    case tv of
      TokenValue _ (NegativeModifier _) -> pure True
      TokenValue _ (DoubleNegativeModifier _) -> pure True
      _ -> pure False
  Matcher.TokenFaceIs face -> pure $ face == tokenFace t
  Matcher.TokenFaceIsNot face -> pure $ face /= tokenFace t
  Matcher.AnyToken -> pure True
  Matcher.TokenMatchesAny ms -> anyM (matchToken iid' t) ms
  Matcher.TokenMatches ms -> allM (matchToken iid' t) ms

matchPhase :: Monad m => Phase -> PhaseMatcher -> m Bool
matchPhase p = \case
  Matcher.AnyPhase -> pure True
  Matcher.PhaseIs p' -> pure $ p == p'

getModifiedTokenFaces
  :: (SourceEntity source, MonadReader env m, HasModifiersFor env ())
  => source
  -> [Token]
  -> m [TokenFace]
getModifiedTokenFaces source tokens = flip
  concatMapM
  tokens
  \token -> do
    modifiers' <- getModifiers (toSource source) (TokenTarget token)
    pure $ foldl' applyModifier [tokenFace token] modifiers'
 where
  applyModifier _ (TokenFaceModifier fs') = fs'
  applyModifier [f'] (ForcedTokenChange f fs) | f == f' = fs
  applyModifier fs _ = fs

matchList :: [QueryElement a] -> ListMatcher a -> Bool
matchList [_] ExactlyOne = True
matchList _ _ = False
