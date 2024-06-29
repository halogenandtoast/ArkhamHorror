{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Helpers.Investigator where

import Arkham.Prelude

import Arkham.Action
import Arkham.Action.Additional
import Arkham.Asset.Types qualified as Field
import Arkham.CampaignLog
import Arkham.Capability
import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Damage
import Arkham.Discover (IsInvestigate (..))
import Arkham.GameValue
import Arkham.Helpers
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Slot
import Arkham.Helpers.Source
import Arkham.Id
import Arkham.Investigator.Types
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (InvestigatorDefeated)
import Arkham.Message (Message (InvestigatorMulligan))
import Arkham.Name
import Arkham.Placement
import Arkham.Projection
import Arkham.SkillType
import Arkham.Source
import Arkham.Stats
import Arkham.Target
import Data.Foldable (foldrM)
import Data.Function (on)
import Data.List (nubBy)

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
    base' <- foldrM applyBaseModifier base tempModifiers
    foldrM applyModifier base' tempModifiers
   where
    canBeIncreased = SkillCannotBeIncreased skill `notElem` tempModifiers
    matchingSkills = s : mapMaybe maybeAdditionalSkill tempModifiers -- must be the skill we are looking at
    maybeAdditionalSkill = \case
      SkillModifiersAffectOtherSkill s' t | t == skill -> Just s'
      _ -> Nothing
    applyBaseModifier DoubleBaseSkillValue n | canBeIncreased = pure (n * 2)
    applyBaseModifier _ n = pure n
    applyModifier (AddSkillValue sv) n | canBeIncreased = do
      m <- getSkillValue sv iid
      pure $ max 0 (n + m)
    applyModifier (AddSkillValueOf sv iid') n | canBeIncreased = do
      m <- getSkillValue sv iid'
      pure $ max 0 (n + m)
    applyModifier (AddSkillToOtherSkill svAdd svType) n | canBeIncreased && svType `elem` matchingSkills = do
      m <- go (depth - 1) svAdd
      pure $ max 0 (n + m)
    applyModifier (SkillModifier skillType m) n | canBeIncreased || m < 0 = do
      pure $ if skillType `elem` matchingSkills then max 0 (n + m) else n
    applyModifier (ActionSkillModifier action skillType m) n | canBeIncreased || m < 0 = do
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

data DamageFor = DamageForEnemy | DamageForInvestigator
  deriving stock (Eq)

damageValueFor :: HasGame m => Int -> InvestigatorId -> DamageFor -> m Int
damageValueFor baseValue iid damageFor = do
  modifiers <- getModifiers (InvestigatorTarget iid)
  let baseValue' = if NoStandardDamage `elem` modifiers then 0 else baseValue
  pure $ foldr applyModifier baseValue' modifiers
 where
  applyModifier (DamageDealt m) n = max 0 (n + m)
  applyModifier (DamageDealtToInvestigator m) n | damageFor == DamageForInvestigator = max 0 (n + m)
  applyModifier NoDamageDealt _ = 0
  applyModifier _ n = n

getHandSize :: HasGame m => InvestigatorAttrs -> m Int
getHandSize attrs = do
  modifiers <- getModifiers (InvestigatorTarget $ investigatorId attrs)
  let ignoreReduction = IgnoreHandSizeReduction `elem` modifiers
  pure $ foldr (applyModifier ignoreReduction) 8 modifiers
 where
  applyModifier ignoreReduction (HandSize m) n
    | m > 0 || not ignoreReduction = max 0 (n + m)
  applyModifier _ _ n = n

getInHandCount :: HasGame m => InvestigatorAttrs -> m Int
getInHandCount attrs = do
  onlyFirstCopies <- hasModifier attrs OnlyFirstCopyCardCountsTowardMaximumHandSize
  let f = if onlyFirstCopies then nubBy ((==) `on` toName) else id
  cards <- fieldMap InvestigatorHand f (toId attrs)
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
  :: HasGame m => IsInvestigate -> InvestigatorId -> LocationId -> m Bool
getCanDiscoverClues isInvestigation iid lid = do
  modifiers <- getModifiers iid
  hasClues <- fieldSome LocationClues lid
  (&& hasClues) . not <$> anyM match modifiers
 where
  match CannotDiscoverClues {} = pure True
  match (CannotDiscoverCluesAt matcher) = elem lid <$> select matcher
  match (CannotDiscoverCluesExceptAsResultOfInvestigation matcher) | isInvestigation == NotInvestigate = elem lid <$> select matcher
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
  -> CardBuilder PlayerId a
investigatorWith f cardDef stats g = investigator (f . g) cardDef stats

startsWith
  :: (Entity a, EntityAttrs a ~ InvestigatorAttrs)
  => [CardDef]
  -> CardBuilder PlayerId a
  -> CardBuilder PlayerId a
startsWith cards = fmap (overAttrs (startsWithL <>~ cards))

startsWithInHand
  :: (Entity a, EntityAttrs a ~ InvestigatorAttrs)
  => [CardDef]
  -> CardBuilder PlayerId a
  -> CardBuilder PlayerId a
startsWithInHand cards = fmap (overAttrs (startsWithInHandL <>~ cards))

investigator
  :: (InvestigatorAttrs -> a) -> CardDef -> Stats -> CardBuilder PlayerId a
investigator f cardDef Stats {..} =
  let iid = InvestigatorId (cdCardCode cardDef)
   in CardBuilder
        { cbCardCode = cdCardCode cardDef
        , cbCardBuilder = \_ pid ->
            f
              $ InvestigatorAttrs
                { investigatorId = iid
                , investigatorPlayerId = pid
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
                , investigatorPlacement = Unplaced
                , investigatorActionsTaken = mempty
                , investigatorActionsPerformed = mempty
                , investigatorRemainingActions = 3
                , investigatorEndedTurn = False
                , investigatorDeck = mempty
                , investigatorDecks = mempty
                , investigatorDiscard = mempty
                , investigatorHand = mempty
                , investigatorTraits = cdCardTraits cardDef
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
                , investigatorSearch = Nothing
                , investigatorMovement = Nothing
                , investigatorBondedCards = mempty
                , investigatorMeta = Null
                , investigatorUnhealedHorrorThisRound = 0
                , investigatorUsedAbilities = mempty
                , investigatorUsedAdditionalActions = mempty
                , investigatorMulligansTaken = 0
                , investigatorHorrorHealed = 0
                , investigatorSupplies = []
                , investigatorKeys = mempty
                , investigatorAssignedHealthDamage = 0
                , investigatorAssignedHealthHeal = mempty
                , investigatorAssignedSanityDamage = 0
                , investigatorAssignedSanityHeal = mempty
                , investigatorDrawnCards = []
                , investigatorIsYithian = False
                , investigatorDiscarding = Nothing
                , investigatorDiscover = Nothing
                , investigatorDrawing = Nothing
                , investigatorLog = mkCampaignLog
                , investigatorDeckBuildingAdjustments = mempty
                , investigatorBeganRoundAt = Nothing
                }
        }

matchTarget :: InvestigatorAttrs -> ActionTarget -> Action -> Bool
matchTarget attrs (FirstOneOfPerformed as) action =
  action `elem` as && all (\a -> all (notElem a) $ investigatorActionsPerformed attrs) as
matchTarget _ (IsAction a) action = action == a
matchTarget _ (EnemyAction a _) action = action == a
matchTarget _ IsAnyAction _ = True

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
      (\aa -> anyM (\ac -> additionalActionCovers (toSource a) [ac] aa) as)
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
  :: HasGame m => InvestigatorId -> LocationId -> m Bool
canCommitToAnotherLocation iid otherLocation = do
  modifiers <- getModifiers iid
  if any (`elem` modifiers) [CannotCommitToOtherInvestigatorsSkillTests]
    then pure False
    else anyM permit modifiers
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

getMaybeLocation
  :: (HasCallStack, HasGame m) => InvestigatorId -> m (Maybe LocationId)
getMaybeLocation = field InvestigatorLocation

withLocationOf :: HasGame m => InvestigatorId -> (LocationId -> m ()) -> m ()
withLocationOf iid = forField InvestigatorLocation iid

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

canHaveHorrorHealed :: (HasGame m, Sourceable a) => a -> InvestigatorId -> m Bool
canHaveHorrorHealed a = selectAny . HealableInvestigator (toSource a) HorrorType . InvestigatorWithId

canHaveDamageHealed :: (HasGame m, Sourceable a) => a -> InvestigatorId -> m Bool
canHaveDamageHealed a = selectAny . HealableInvestigator (toSource a) DamageType . InvestigatorWithId

additionalActionCovers
  :: HasGame m => Source -> [Action] -> AdditionalAction -> m Bool
additionalActionCovers source actions (AdditionalAction _ _ aType) = case aType of
  TraitRestrictedAdditionalAction t actionRestriction -> case actionRestriction of
    NoRestriction -> member t <$> sourceTraits source
    AbilitiesOnly -> case source of
      AbilitySource {} -> member t <$> sourceTraits source
      _ -> pure False
  ActionRestrictedAdditionalAction a -> pure $ a `elem` actions
  EffectAction _ _ -> pure False
  AnyAdditionalAction -> pure True
  BountyAction -> pure False -- Has to be handled by Tony Morgan
  BobJenkinsAction -> pure False -- Has to be handled by Bob Jenkins

-- canFight <- selectAny $ CanFightEnemy source <> EnemyWithBounty
-- canEngage <- selectAny $ CanEngageEnemy <> EnemyWithBounty
-- pure $ (canFight && maction == Just #fight) || (canEngage && maction == Just #engage)

eliminationWindow :: InvestigatorId -> WindowMatcher
eliminationWindow iid = OrWindowMatcher [GameEnds #when, InvestigatorEliminated #when (InvestigatorWithId iid)]

getCanShuffleDeck :: HasGame m => InvestigatorId -> m Bool
getCanShuffleDeck iid =
  andM
    [ withoutModifier iid CannotManipulateDeck
    , fieldMap InvestigatorDeck notNull iid
    ]

check :: (EntityId a ~ InvestigatorId, Entity a, HasGame m) => a -> InvestigatorMatcher -> m Bool
check (toId -> iid) capability = iid <=~> capability

checkAll
  :: (EntityId a ~ InvestigatorId, Entity a, HasGame m) => a -> [InvestigatorMatcher] -> m Bool
checkAll (toId -> iid) capabilities = iid <=~> fold capabilities

searchBonded :: (HasGame m, AsId iid, IdOf iid ~ InvestigatorId) => iid -> CardDef -> m [Card]
searchBonded (asId -> iid) def = fieldMap InvestigatorBondedCards (filter ((== def) . toCardDef)) iid

-- TODO: Decide if we want to use or keep these instances, these let you do
-- >       canModifyDeck <- can.manipulate.deck attrs

instance HasGame m => Capable (InvestigatorId -> m Bool) where
  can =
    let can' = can :: Capabilities InvestigatorMatcher
     in fmap (flip (<=~>)) can'

instance HasGame m => Capable (FromSource -> InvestigatorId -> m Bool) where
  can =
    let can' = can :: Capabilities (FromSource -> InvestigatorMatcher)
     in fmap (\m fSource iid -> iid <=~> m fSource) can'

instance HasGame m => Capable (InvestigatorAttrs -> m Bool) where
  can =
    let can' = can :: Capabilities InvestigatorMatcher
     in fmap (\c -> (<=~> c) . toId) can'

instance HasGame m => Capable (FromSource -> InvestigatorAttrs -> m Bool) where
  can =
    let can' = can :: Capabilities (FromSource -> InvestigatorMatcher)
     in fmap (\c fSource attrs -> toId attrs <=~> c fSource) can'

guardAffectsOthers :: HasGame m => InvestigatorId -> InvestigatorMatcher -> m InvestigatorMatcher
guardAffectsOthers iid matcher = do
  -- This is mainly for self centered
  selfCentered <- hasModifier iid CannotAffectOtherPlayersWithPlayerEffectsExceptDamage
  pure $ if selfCentered then matcher <> InvestigatorWithId iid else matcher

guardAffectsColocated :: HasGame m => InvestigatorId -> m InvestigatorMatcher
guardAffectsColocated iid = guardAffectsOthers iid (colocatedWith iid)

getInMulligan :: HasQueue Message m => m Bool
getInMulligan = fromQueue (any isMulligan)
 where
  isMulligan = \case
    InvestigatorMulligan {} -> True
    _ -> False

setMeta :: ToJSON a => a -> InvestigatorAttrs -> InvestigatorAttrs
setMeta meta attrs = attrs & metaL .~ toJSON meta
