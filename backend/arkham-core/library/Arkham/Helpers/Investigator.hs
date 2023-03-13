module Arkham.Helpers.Investigator where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Treacheries
import Data.UUID qualified as UUID
import Data.Monoid
import Arkham.Action
import Arkham.Card
import Arkham.Card.Id
import Arkham.Classes.Entity
import Arkham.Classes.Query
import Arkham.Damage
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Slot
import Arkham.Id
import Arkham.Investigator.Types
import Arkham.Matcher hiding ( InvestigatorDefeated )
import Arkham.Message (Message(HealHorror))
import Arkham.Projection
import Arkham.SkillTest.Base
import Arkham.SkillType
import Arkham.Source
import Arkham.Stats
import Arkham.Target
import Data.Foldable ( foldrM )
import Data.UUID ( nil )

getSkillValue :: HasGame m => SkillType -> InvestigatorId -> m Int
getSkillValue st iid = case st of
  SkillWillpower -> field InvestigatorWillpower iid
  SkillIntellect -> field InvestigatorIntellect iid
  SkillCombat -> field InvestigatorCombat iid
  SkillAgility -> field InvestigatorAgility iid

skillValueFor
  :: forall m. HasGame m
  => SkillType
  -> Maybe Action
  -> [ModifierType]
  -> InvestigatorId
  -> m Int
skillValueFor skill maction tempModifiers iid = go 2 skill
 where
  go :: Int -> SkillType -> m Int
  go 0 _ = error "possible skillValueFor infinite loop"
  go depth s = do
    base <- baseSkillValueFor s maction tempModifiers iid
    foldrM applyModifier base tempModifiers
   where
    canBeIncreased = SkillCannotBeIncreased skill `notElem` tempModifiers
    applyModifier (AnySkillValue m) n | canBeIncreased || m < 0 =
      pure $ max 0 (n + m)
    applyModifier (AddSkillValue sv) n | canBeIncreased = do
      m <- getSkillValue sv iid
      pure $ max 0 (n + m)
    applyModifier (AddSkillToOtherSkill svAdd svType) n | canBeIncreased && svType == skill = do
      m <- go (depth - 1) svAdd
      pure $ max 0 (n + m)
    applyModifier (SkillModifier skillType m) n | canBeIncreased || m < 0 =
      pure $ if skillType == skill then max 0 (n + m) else n
    applyModifier (ActionSkillModifier action skillType m) n
      | canBeIncreased || m < 0
      = pure $ if skillType == skill && Just action == maction
        then max 0 (n + m)
        else n
    applyModifier _ n = pure n

baseSkillValueFor
  :: HasGame m
  => SkillType
  -> Maybe Action
  -> [ModifierType]
  -> InvestigatorId
  -> m Int
baseSkillValueFor skill _maction tempModifiers iid = do
  baseValue <- getSkillValue skill iid
  pure $ foldr applyModifier baseValue tempModifiers
 where
  applyModifier (BaseSkillOf skillType m) _ | skillType == skill = m
  applyModifier _ n = n

damageValueFor :: HasGame m => Int -> InvestigatorId -> m Int
damageValueFor baseValue iid = do
  modifiers <- getModifiers (InvestigatorTarget iid)
  pure $ foldr applyModifier baseValue modifiers
 where
  applyModifier (DamageDealt m) n = max 0 (n + m)
  applyModifier NoDamageDealt _ = 0
  applyModifier _ n = n

getHandSize :: HasGame m => InvestigatorAttrs -> m Int
getHandSize attrs = do
  modifiers <- getModifiers (InvestigatorTarget $ investigatorId attrs)
  let ignoreReduction = IgnoreHandSizeReduction `elem` modifiers
  pure $ foldr (applyModifier ignoreReduction) 8 modifiers
 where
  applyModifier ignoreReduction (HandSize m) n | m > 0 || not ignoreReduction =
    max 0 (n + m)
  applyModifier _ _ n = n

getInHandCount :: HasGame m => InvestigatorAttrs -> m Int
getInHandCount attrs = do
  let
    cards = investigatorHand attrs
    applyModifier n = \case
      HandSizeCardCount m -> m
      _ -> n
    getCardHandSize c = do
      modifiers <- getModifiers (CardTarget c)
      pure $ foldl' applyModifier 1 modifiers
  sum <$> traverse getCardHandSize cards

getAbilitiesForTurn :: HasGame m => InvestigatorAttrs -> m Int
getAbilitiesForTurn attrs = do
  modifiers <- getModifiers (toTarget attrs)
  pure $ foldr applyModifier 3 modifiers
 where
  applyModifier (AdditionalActions m) n = max 0 (n + m)
  applyModifier _ n = n

getCanDiscoverClues
  :: HasGame m => InvestigatorAttrs -> LocationId -> m Bool
getCanDiscoverClues attrs lid = do
  modifiers <- getModifiers (toTarget attrs)
  not <$> anyM match modifiers
 where
  match CannotDiscoverClues{} = pure True
  match (CannotDiscoverCluesAt matcher) = elem lid <$> select matcher
  match _ = pure False

getCanSpendClues :: HasGame m => InvestigatorAttrs -> m Bool
getCanSpendClues attrs = do
  modifiers <- getModifiers (toTarget attrs)
  pure $ not (any match modifiers)
 where
  match CannotSpendClues{} = True
  match _ = False

removeFromSlots
  :: AssetId -> HashMap SlotType [Slot] -> HashMap SlotType [Slot]
removeFromSlots aid = fmap (map (removeIfMatches aid))

fitsAvailableSlots :: IsCard a => [SlotType] -> a -> InvestigatorAttrs -> Bool
fitsAvailableSlots slotTypes cardDef a = null
  (slotTypes \\ concatMap
    (\slotType -> availableSlotTypesFor slotType cardDef a)
    (nub slotTypes)
  )

availableSlotTypesFor
  :: IsCard a => SlotType -> a -> InvestigatorAttrs -> [SlotType]
availableSlotTypesFor slotType a attrs =
  case lookup slotType (attrs ^. slotsL) of
    Nothing -> []
    Just slots -> replicate (length (filter (canPutIntoSlot a) slots)) slotType

placeInAvailableSlot :: IsCard a => AssetId -> a -> [Slot] -> [Slot]
placeInAvailableSlot _ _ [] = []
placeInAvailableSlot aid card (x : xs) = if canPutIntoSlot card x
  then putIntoSlot aid x : xs
  else x : placeInAvailableSlot aid card xs

discardableCards :: InvestigatorAttrs -> [Card]
discardableCards InvestigatorAttrs {..} =
  if all cardIsWeakness investigatorHand
    then investigatorHand
    else filter (not . cardIsWeakness) investigatorHand

getAttrStats :: InvestigatorAttrs -> Stats
getAttrStats InvestigatorAttrs {..} = Stats
  { health = investigatorHealth
  , sanity = investigatorSanity
  , willpower = investigatorWillpower
  , intellect = investigatorIntellect
  , combat = investigatorCombat
  , agility = investigatorAgility
  }

investigatorWith
  :: (InvestigatorAttrs -> a)
  -> CardDef
  -> Stats
  -> (InvestigatorAttrs -> InvestigatorAttrs)
  -> CardBuilder () a
investigatorWith f cardDef stats g = investigator (f . g) cardDef stats

investigator
  :: (InvestigatorAttrs -> a) -> CardDef -> Stats -> CardBuilder () a
investigator f cardDef Stats {..} =
  let iid = InvestigatorId (cdCardCode cardDef)
  in
    CardBuilder
      { cbCardCode = cdCardCode cardDef
      , cbCardBuilder = \_ -> f $ InvestigatorAttrs
        { investigatorId = iid
        , investigatorName = cdName cardDef
        , investigatorCardCode = cdCardCode cardDef
        , investigatorClass =
          fromJustNote "missing class symbol"
          . headMay
          . setToList
          $ cdClassSymbols cardDef
        , investigatorHealth = health
        , investigatorSanity = sanity
        , investigatorWillpower = willpower
        , investigatorIntellect = intellect
        , investigatorCombat = combat
        , investigatorAgility = agility
        , investigatorHealthDamage = 0
        , investigatorSanityDamage = 0
        , investigatorClues = 0
        , investigatorDoom = 0
        , investigatorResources = 0
        , investigatorLocation = LocationId $ CardId nil
        , investigatorActionsTaken = mempty
        , investigatorRemainingActions = 3
        , investigatorEndedTurn = False
        , investigatorEngagedEnemies = mempty
        , investigatorAssets = mempty
        , investigatorEvents = mempty
        , investigatorDeck = mempty
        , investigatorDecks = mempty
        , investigatorDiscard = mempty
        , investigatorHand = mempty
        , investigatorTraits = cdCardTraits cardDef
        , investigatorTreacheries = mempty
        , investigatorKilled = False
        , investigatorDrivenInsane = False
        , investigatorDefeated = False
        , investigatorResigned = False
        , investigatorSlots = mapFromList
          [ (AccessorySlot, [Slot (InvestigatorSource iid) Nothing])
          , (BodySlot, [Slot (InvestigatorSource iid) Nothing])
          , (AllySlot, [Slot (InvestigatorSource iid) Nothing])
          , ( HandSlot
            , [ Slot (InvestigatorSource iid) Nothing
              , Slot (InvestigatorSource iid) Nothing
              ]
            )
          , ( ArcaneSlot
            , [ Slot (InvestigatorSource iid) Nothing
              , Slot (InvestigatorSource iid) Nothing
              ]
            )
          , (TarotSlot, [Slot (InvestigatorSource iid) Nothing])
          ]
        , investigatorXp = 0
        , investigatorPhysicalTrauma = 0
        , investigatorMentalTrauma = 0
        , investigatorStartsWith = []
        , investigatorStartsWithInHand = []
        , investigatorCardsUnderneath = []
        , investigatorFoundCards = mempty
        , investigatorUsedAbilities = mempty
        , investigatorAdditionalActions = []
        , investigatorHorrorHealed = 0
        , investigatorSupplies = []
        , investigatorAssignedHealthDamage = 0
        , investigatorAssignedSanityDamage = 0
        , investigatorDrawnCards = []
        , investigatorIsYithian = False
        , investigatorDiscarding = Nothing
        }
      }

matchTarget :: InvestigatorAttrs -> ActionTarget -> Action -> Bool
matchTarget attrs (FirstOneOf as) action =
  action `elem` as && all (`notElem` investigatorActionsTaken attrs) as
matchTarget _ (IsAction a) action = action == a
matchTarget _ (EnemyAction a _) action = action == a

getActionCost :: HasGame m => InvestigatorAttrs -> [Action] -> m Int
getActionCost attrs as = do
  modifiers <- getModifiers (toTarget attrs)
  pure $ foldr applyModifier 1 modifiers
 where
  applyModifier (ActionCostOf match m) n =
    if any (matchTarget attrs match) as then n + m else n
  applyModifier _ n = n

getSpendableClueCount :: HasGame m => InvestigatorAttrs -> m Int
getSpendableClueCount a = do
  canSpendClues <- getCanSpendClues a
  pure $ if canSpendClues then investigatorClues a else 0

getCanAfford :: HasGame m => InvestigatorAttrs -> [Action] -> m Bool
getCanAfford a@InvestigatorAttrs {..} as = do
  actionCost <- getActionCost a as
  pure $ actionCost <= investigatorRemainingActions

drawOpeningHand
  :: HasCallStack => InvestigatorAttrs -> Int -> ([PlayerCard], [Card], [PlayerCard])
drawOpeningHand a n = go n (a ^. discardL, a ^. handL, coerce (a ^. deckL))
 where
  go 0 (d, h, cs) = (d, h, cs)
  go _ (_, _, []) =
    error "this should never happen, it means the deck was empty during drawing"
  go m (d, h, c : cs) = if isJust (cdCardSubType $ toCardDef c) && cdCanReplace (toCardDef c)
    then go m (c : d, h, cs)
    else go (m - 1) (d, PlayerCard c : h, cs)

canCommitToAnotherLocation
  :: HasGame m => InvestigatorAttrs -> m Bool
canCommitToAnotherLocation attrs = do
  commitedCards <-
    skillTestCommittedCards . fromJustNote "no skill test" <$> getSkillTest
  let
    committedCardIds =
      concatMap (map toCardId . snd) . filter ((== toId attrs) . fst) $ mapToList commitedCards
  modifiers <- getModifiers (toTarget attrs)
  pure $ any (permit committedCardIds) modifiers
 where
  permit n (CanCommitToSkillTestPerformedByAnInvestigatorAtAnotherLocation m) =
    m > length n
  permit _ _ = False

findCard :: HasCallStack => CardId -> InvestigatorAttrs -> Card
findCard cardId a =
  fromJustNote "not in hand or discard or deck"
    $ findMatch
    $ (a ^. handL)
    <> map PlayerCard (a ^. discardL)
    <> map PlayerCard (unDeck $ a ^. deckL)
  where findMatch = find ((== cardId) . toCardId)

getJustLocation
  :: (HasCallStack, HasGame m) => InvestigatorId -> m LocationId
getJustLocation =
  fieldMap InvestigatorLocation (fromJustNote "must be at a location")

enemiesColocatedWith :: InvestigatorId -> EnemyMatcher
enemiesColocatedWith = EnemyAt . LocationWithInvestigator . InvestigatorWithId

modifiedStatsOf
  :: HasGame m => Maybe Action -> InvestigatorId -> m Stats
modifiedStatsOf maction i = do
  modifiers' <- getModifiers (InvestigatorTarget i)
  remainingHealth <- field InvestigatorRemainingHealth i
  remainingSanity <- field InvestigatorRemainingSanity i
  willpower' <- skillValueFor SkillWillpower maction modifiers' i
  intellect' <- skillValueFor SkillIntellect maction modifiers' i
  combat' <- skillValueFor SkillCombat maction modifiers' i
  agility' <- skillValueFor SkillAgility maction modifiers' i
  pure Stats
    { willpower = willpower'
    , intellect = intellect'
    , combat = combat'
    , agility = agility'
    , health = remainingHealth
    , sanity = remainingSanity
    }

getAvailableSkillsFor
  :: HasGame m => SkillType -> InvestigatorId -> m (HashSet SkillType)
getAvailableSkillsFor skillType iid = do
  modifiers <- getModifiers (InvestigatorTarget iid)
  pure $ foldr applyModifier (singleton skillType) modifiers
 where
  applyModifier (UseSkillInPlaceOf toReplace toUse) skills
    | toReplace == skillType = insertSet toUse skills
  applyModifier _ skills = skills

isEliminated :: HasGame m => InvestigatorId -> m Bool
isEliminated iid =
  orM $ sequence [field InvestigatorResigned, field InvestigatorDefeated] iid

getHandCount :: HasGame m => InvestigatorId -> m Int
getHandCount = fieldMap InvestigatorHand length

getHealHorrorMessage :: (HasGame m, Sourceable a) => a -> Int -> InvestigatorId -> m (Maybe Message)
getHealHorrorMessage a n iid = do
  mHorrorId <- canHaveHorrorHealed a iid
  for mHorrorId $ \horrorId ->
    pure $ HealHorror (InvestigatorTarget horrorId) (toSource a) n

canHaveHorrorHealed :: (HasGame m, Sourceable a) => a -> InvestigatorId -> m (Maybe InvestigatorId)
canHaveHorrorHealed a iid =  do
  result <- selectAny $ HealableInvestigator (toSource a) HorrorType $ InvestigatorWithId iid

  let
    isCannotHealHorrorOnOtherCardsModifiers = \case
      CannotHealHorrorOnOtherCards _ -> True
      _ -> False
  mModifier <- find isCannotHealHorrorOnOtherCardsModifiers <$> getModifiers (InvestigatorTarget iid)
  case mModifier of
    Nothing -> pure $ iid <$ guard result
    Just (CannotHealHorrorOnOtherCards target) -> case target of
      TreacheryTarget tid -> do
        -- we know rational thought is in effect
        let
          asIfInvestigator = \case
            HealHorrorOnThisAsIfInvestigator ii -> First (Just ii)
            _ -> First Nothing
        mAsIfInverstigator <- getFirst . foldMap asIfInvestigator <$> getModifiers target
        case mAsIfInverstigator of
          Just iid' | iid == iid' -> do
            innerResult <- member tid <$> select (treacheryIs Treacheries.rationalThought)
            pure $ InvestigatorId (CardCode $ UUID.toText $ unCardId $ unTreacheryId tid) <$ guard innerResult
          _ -> pure Nothing
      _ -> pure Nothing
    _ -> pure Nothing

canHaveDamageHealed :: (HasGame m, Sourceable a) => a -> InvestigatorId -> m Bool
canHaveDamageHealed a = selectAny . HealableInvestigator (toSource a) HorrorType . InvestigatorWithId

getInvestigatorsWithHealHorror
  :: (HasGame m, Sourceable a)
  => a
  -> Int
  -> InvestigatorMatcher
  -> m [(InvestigatorId, Message)]
getInvestigatorsWithHealHorror a n =
  selectList >=> mapMaybeM (traverseToSndM (getHealHorrorMessage a n))
