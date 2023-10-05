{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Helpers.Investigator where

import Arkham.Prelude

import Arkham.Action
import Arkham.Action.Additional
import Arkham.Asset.Types qualified as Field
import Arkham.Capability
import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.Query
import Arkham.Damage
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Helpers
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Slot
import Arkham.Helpers.Source
import Arkham.Id
import Arkham.Investigator.Types
import Arkham.Matcher hiding (InvestigatorDefeated)
import Arkham.Message (Message (HealHorror))
import Arkham.Projection
import Arkham.SkillType
import Arkham.Source
import Arkham.Stats
import Arkham.Target
import Arkham.Treachery.Cards qualified as Treacheries
import Data.Foldable (foldrM)
import Data.Monoid
import Data.UUID (nil)
import Data.UUID qualified as UUID

getSkillValue :: HasGame m => SkillType -> InvestigatorId -> m Int
getSkillValue st iid = case st of
  SkillWillpower -> field InvestigatorWillpower iid
  SkillIntellect -> field InvestigatorIntellect iid
  SkillCombat -> field InvestigatorCombat iid
  SkillAgility -> field InvestigatorAgility iid

skillValueFor
  :: forall m
   . HasGame m
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
    matchingSkills = skill : mapMaybe maybeAdditionalSkill tempModifiers
    maybeAdditionalSkill = \case
      SkillModifiersAffectOtherSkill s' t | t == skill -> Just s'
      _ -> Nothing
    applyModifier (AnySkillValue m) n
      | canBeIncreased || m < 0 =
          pure $ max 0 (n + m)
    applyModifier (AddSkillValue sv) n | canBeIncreased = do
      m <- getSkillValue sv iid
      pure $ max 0 (n + m)
    applyModifier (AddSkillToOtherSkill svAdd svType) n | canBeIncreased && svType `elem` matchingSkills = do
      m <- go (depth - 1) svAdd
      pure $ max 0 (n + m)
    applyModifier (SkillModifier skillType m) n
      | canBeIncreased || m < 0 =
          pure $ if skillType `elem` matchingSkills then max 0 (n + m) else n
    applyModifier (ActionSkillModifier action skillType m) n
      | canBeIncreased || m < 0 =
          pure
            $ if skillType `elem` matchingSkills && Just action == maction
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
  applyModifier ignoreReduction (HandSize m) n
    | m > 0 || not ignoreReduction =
        max 0 (n + m)
  applyModifier _ _ n = n

getInHandCount :: HasGame m => InvestigatorAttrs -> m Int
getInHandCount attrs = do
  cards <- field InvestigatorHand (toId attrs)
  let
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
  applyModifier (FewerActions m) n = max 0 (n - m)
  applyModifier _ n = n

getCanDiscoverClues
  :: HasGame m => InvestigatorId -> LocationId -> m Bool
getCanDiscoverClues iid lid = do
  modifiers <- getModifiers (toTarget iid)
  not <$> anyM match modifiers
 where
  match CannotDiscoverClues {} = pure True
  match (CannotDiscoverCluesAt matcher) = elem lid <$> select matcher
  match _ = pure False

getCanSpendClues :: HasGame m => InvestigatorAttrs -> m Bool
getCanSpendClues attrs = do
  modifiers <- getModifiers (toTarget attrs)
  pure $ not (any match modifiers)
 where
  match CannotSpendClues {} = True
  match _ = False

removeFromSlots
  :: AssetId -> Map SlotType [Slot] -> Map SlotType [Slot]
removeFromSlots aid = fmap (map (removeIfMatches aid))

data FitsSlots = FitsSlots | MissingSlots [SlotType]

fitsAvailableSlots :: HasGame m => AssetId -> InvestigatorAttrs -> m FitsSlots
fitsAvailableSlots aid a = do
  assetCard <- field Field.AssetCard aid
  slotTypes <- do
    baseSlots <- field Field.AssetSlots aid
    modifiers <- getModifiers aid
    pure $ filter ((`notElem` modifiers) . DoNotTakeUpSlot) baseSlots

  canHoldMap :: Map SlotType [SlotType] <- do
    mods <- getModifiers a
    let
      canHold = \case
        SlotCanBe slotType canBeSlotType -> insertWith (<>) slotType [canBeSlotType]
        _ -> id
    pure $ foldr canHold mempty mods

  -- N.B. we map (const slotType) in order to determine coverage. In other words if
  -- a card like The Hierophant V (3) is in play we have Accessory and Arcane
  -- slots actings as both, but for the sake of this function we'd need to make
  -- sure, for instance, all Arcane slots are covered by a card, so we'd count
  -- every Accessory Slot as an Arcane Slot
  -- WARNING This only works if the slots are bidirectional and if we need it
  -- to work in other cases we'll need to alter this logic
  availableSlots <-
    concatForM
      (nub slotTypes)
      (\slotType -> map (const slotType) <$> availableSlotTypesFor slotType canHoldMap assetCard a)
  let missingSlotTypes = slotTypes \\ availableSlots

  if null missingSlotTypes
    then pure FitsSlots
    else pure $ MissingSlots missingSlotTypes

availableSlotTypesFor
  :: (IsCard a, HasGame m)
  => SlotType
  -> Map SlotType [SlotType]
  -> a
  -> InvestigatorAttrs
  -> m [SlotType]
availableSlotTypesFor slotType canHoldMap a attrs = do
  let possibleSlotTypes = slotType : findWithDefault [] slotType canHoldMap
  concatForM possibleSlotTypes $ \sType -> do
    let slots = findWithDefault [] sType (attrs ^. slotsL)
    xs <- filterM (canPutIntoSlot a) slots
    pure $ replicate (length xs) sType

nonEmptySlotsFirst :: [Slot] -> [Slot]
nonEmptySlotsFirst = sortOn isEmptySlot

placeInAvailableSlot :: (HasGame m, IsCard a) => AssetId -> a -> [Slot] -> m [Slot]
placeInAvailableSlot aid card = go . nonEmptySlotsFirst
 where
  go [] = pure []
  go (x : xs) = do
    fits <- canPutIntoSlot card x
    if fits
      then pure $ putIntoSlot aid x : xs
      else (x :) <$> go xs

discardableCards :: InvestigatorAttrs -> [Card]
discardableCards InvestigatorAttrs {..} =
  if all cardIsWeakness investigatorHand
    then investigatorHand
    else filter (not . cardIsWeakness) investigatorHand

getAttrStats :: InvestigatorAttrs -> Stats
getAttrStats InvestigatorAttrs {..} =
  Stats
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

startsWith
  :: (Entity a, EntityAttrs a ~ InvestigatorAttrs) => [CardDef] -> CardBuilder () a -> CardBuilder () a
startsWith cards = fmap (overAttrs (startsWithL <>~ cards))

startsWithInHand
  :: (Entity a, EntityAttrs a ~ InvestigatorAttrs) => [CardDef] -> CardBuilder () a -> CardBuilder () a
startsWithInHand cards = fmap (overAttrs (startsWithInHandL <>~ cards))

investigator
  :: (InvestigatorAttrs -> a) -> CardDef -> Stats -> CardBuilder () a
investigator f cardDef Stats {..} =
  let iid = InvestigatorId (cdCardCode cardDef)
   in CardBuilder
        { cbCardCode = cdCardCode cardDef
        , cbCardBuilder = \_ _ ->
            f
              $ InvestigatorAttrs
                { investigatorId = iid
                , investigatorName = cdName cardDef
                , investigatorCardCode = cdCardCode cardDef
                , investigatorArt = cdCardCode cardDef
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
                , investigatorTokens = mempty
                , investigatorLocation = LocationId nil
                , investigatorActionsTaken = mempty
                , investigatorActionsPerformed = mempty
                , investigatorRemainingActions = 3
                , investigatorEndedTurn = False
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
                , investigatorSlots =
                    mapFromList
                      [ (AccessorySlot, [Slot (InvestigatorSource iid) []])
                      , (BodySlot, [Slot (InvestigatorSource iid) []])
                      , (AllySlot, [Slot (InvestigatorSource iid) []])
                      ,
                        ( HandSlot
                        ,
                          [ Slot (InvestigatorSource iid) []
                          , Slot (InvestigatorSource iid) []
                          ]
                        )
                      ,
                        ( ArcaneSlot
                        ,
                          [ Slot (InvestigatorSource iid) []
                          , Slot (InvestigatorSource iid) []
                          ]
                        )
                      , (TarotSlot, [Slot (InvestigatorSource iid) []])
                      ]
                , investigatorXp = 0
                , investigatorPhysicalTrauma = 0
                , investigatorMentalTrauma = 0
                , investigatorStartsWith = []
                , investigatorStartsWithInHand = []
                , investigatorCardsUnderneath = []
                , investigatorFoundCards = mempty
                , investigatorUsedAbilities = mempty
                , investigatorUsedAdditionalActions = mempty
                , investigatorMulligansTaken = 0
                , investigatorHorrorHealed = 0
                , investigatorSupplies = []
                , investigatorKeys = mempty
                , investigatorAssignedHealthDamage = 0
                , investigatorAssignedSanityDamage = 0
                , investigatorDrawnCards = []
                , investigatorIsYithian = False
                , investigatorDiscarding = Nothing
                }
        }

matchTarget :: InvestigatorAttrs -> ActionTarget -> Action -> Bool
matchTarget attrs (FirstOneOfPerformed as) action =
  action `elem` as && all (`notElem` investigatorActionsPerformed attrs) as
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

getCanSpendNClues :: HasGame m => InvestigatorId -> Int -> m Bool
getCanSpendNClues iid n = iid <=~> InvestigatorCanSpendClues (Static n)

getAdditionalActions :: HasGame m => InvestigatorAttrs -> m [AdditionalAction]
getAdditionalActions attrs = do
  mods <- getModifiers attrs
  let
    toAdditionalAction = \case
      GiveAdditionalAction ac -> [ac]
      AdditionalActions label source n -> replicate n $ AdditionalAction label source AnyAdditionalAction
      _ -> []
    additionalActions = concatMap toAdditionalAction mods

  pure $ filter (`notElem` investigatorUsedAdditionalActions attrs) additionalActions

getCanAfford :: HasGame m => InvestigatorAttrs -> [Action] -> m Bool
getCanAfford a@InvestigatorAttrs {..} as = do
  actionCost <- getActionCost a as
  additionalActions <- getAdditionalActions a
  additionalActionCount <-
    countM
      (\aa -> anyM (\ac -> additionalActionCovers (toSource a) (Just ac) aa) as)
      additionalActions
  pure $ actionCost <= (investigatorRemainingActions + additionalActionCount)

drawOpeningHand
  :: (HasCallStack, HasGame m) => InvestigatorAttrs -> Int -> m ([PlayerCard], [Card], [PlayerCard])
drawOpeningHand a n = do
  replaceWeaknesses <- not <$> hasModifier a CannotReplaceWeaknesses
  pure $ go replaceWeaknesses n (a ^. discardL, a ^. handL, coerce (a ^. deckL))
 where
  go _ 0 (d, h, cs) = (d, h, cs)
  go _ _ (_, _, []) =
    error "this should never happen, it means the deck was empty during drawing"
  go replaceWeaknesses m (d, h, c : cs) =
    if isJust (cdCardSubType $ toCardDef c) && cdCanReplace (toCardDef c) && replaceWeaknesses
      then go replaceWeaknesses m (c : d, h, cs)
      else go replaceWeaknesses (m - 1) (d, PlayerCard c : h, cs)

canCommitToAnotherLocation
  :: HasGame m => InvestigatorAttrs -> LocationId -> m Bool
canCommitToAnotherLocation attrs otherLocation = do
  modifiers <- getModifiers (toTarget attrs)
  anyM permit modifiers
 where
  permit (CanCommitToSkillTestPerformedByAnInvestigatorAt matcher) = elem otherLocation <$> select matcher
  permit _ = pure False

findCard :: HasCallStack => CardId -> InvestigatorAttrs -> Card
findCard cardId a =
  fromJustNote "not in hand or discard or deck"
    $ findMatch
    $ (a ^. handL)
    <> map PlayerCard (a ^. discardL)
    <> map PlayerCard (unDeck $ a ^. deckL)
 where
  findMatch = find ((== cardId) . toCardId)

getJustLocation
  :: (HasCallStack, HasGame m) => InvestigatorId -> m LocationId
getJustLocation = fieldJust InvestigatorLocation

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
  pure
    Stats
      { willpower = willpower'
      , intellect = intellect'
      , combat = combat'
      , agility = agility'
      , health = remainingHealth
      , sanity = remainingSanity
      }

getAvailableSkillsFor
  :: HasGame m => SkillType -> InvestigatorId -> m (Set SkillType)
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
canHaveHorrorHealed a iid = do
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
            innerResult <- elem tid <$> select (treacheryIs Treacheries.rationalThought)
            pure $ InvestigatorId (CardCode $ UUID.toText $ unTreacheryId tid) <$ guard innerResult
          _ -> pure Nothing
      _ -> pure Nothing
    _ -> pure Nothing

canHaveDamageHealed :: (HasGame m, Sourceable a) => a -> InvestigatorId -> m Bool
canHaveDamageHealed a = selectAny . HealableInvestigator (toSource a) DamageType . InvestigatorWithId

getInvestigatorsWithHealHorror
  :: (HasGame m, Sourceable a)
  => a
  -> Int
  -> InvestigatorMatcher
  -> m [(InvestigatorId, Message)]
getInvestigatorsWithHealHorror a n =
  selectList >=> mapMaybeM (traverseToSndM (getHealHorrorMessage a n))

additionalActionCovers
  :: HasGame m => Source -> Maybe Action -> AdditionalAction -> m Bool
additionalActionCovers source maction (AdditionalAction _ _ aType) = case aType of
  TraitRestrictedAdditionalAction t actionRestriction -> case actionRestriction of
    NoRestriction -> member t <$> sourceTraits source
    AbilitiesOnly -> case source of
      AbilitySource {} -> member t <$> sourceTraits source
      _ -> pure False
  ActionRestrictedAdditionalAction a -> pure $ maction == Just a
  EffectAction _ _ -> pure False
  AnyAdditionalAction -> pure True
  BountyAction -> pure False -- Has to be handled by Tony Morgan

-- canFight <- selectAny $ CanFightEnemy source <> EnemyWithBounty
-- canEngage <- selectAny $ CanEngageEnemy <> EnemyWithBounty
-- pure $ (canFight && maction == Just #fight) || (canEngage && maction == Just #engage)

getCanDrawCards :: HasGame m => InvestigatorId -> m Bool
getCanDrawCards = selectAny . InvestigatorCanDrawCards . InvestigatorWithId

eliminationWindow :: InvestigatorId -> WindowMatcher
eliminationWindow iid = OrWindowMatcher [GameEnds #when, InvestigatorEliminated #when (InvestigatorWithId iid)]

getCanShuffleDeck :: HasGame m => InvestigatorId -> m Bool
getCanShuffleDeck iid =
  andM
    [ withoutModifier iid CannotManipulateDeck
    , fieldMap InvestigatorDeck notNull iid
    ]

getModifiedHealth :: HasGame m => InvestigatorAttrs -> m Int
getModifiedHealth attrs@InvestigatorAttrs {..} = do
  foldr applyModifier investigatorHealth <$> getModifiers attrs
 where
  applyModifier (HealthModifier m) n = max 0 (n + m)
  applyModifier _ n = n

getModifiedSanity :: HasGame m => InvestigatorAttrs -> m Int
getModifiedSanity attrs@InvestigatorAttrs {..} = do
  foldr applyModifier investigatorSanity <$> getModifiers attrs
 where
  applyModifier (SanityModifier m) n = max 0 (n + m)
  applyModifier _ n = n

instance Capable (InvestigatorId -> GameT Bool) where
  can =
    let can' = can :: Capabilities InvestigatorMatcher
     in fmap (flip (<=~>)) can'
