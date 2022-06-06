module Arkham.Helpers.Investigator where

import Arkham.Prelude

import Arkham.Action
import Arkham.Card
import Arkham.Card.Id
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Investigator.Attrs
import Arkham.Investigator.Attrs ( Field (..) )
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Projection
import Arkham.SkillType
import Arkham.Slot
import Arkham.Source
import Arkham.Stats
import Arkham.Target
import Arkham.Trait

getSkillValue :: SkillType -> InvestigatorId -> GameT Int
getSkillValue st iid = \case
  SkillWillpower -> field InvestigatorWillpower iid
  SkillIntellect -> field InvestigatorIntellect iid
  SkillCombat -> field InvestigatorCombat iid
  SkillAgility -> field InvestigatorAgility iid

skillValueFor
  :: SkillType -> Maybe Action -> [ModifierType] -> InvestigatorAttrs -> Int
skillValueFor skill maction tempModifiers attrs = foldr
  applyModifier
  (baseSkillValueFor skill maction tempModifiers attrs)
  tempModifiers
 where
  canBeIncreased = SkillCannotBeIncreased skill `notElem` tempModifiers
  applyModifier (AnySkillValue m) n | canBeIncreased || m < 0 = max 0 (n + m)
  applyModifier (SkillModifier skillType m) n | canBeIncreased || m < 0 =
    if skillType == skill then max 0 (n + m) else n
  applyModifier (ActionSkillModifier action skillType m) n
    | canBeIncreased || m < 0 = if skillType == skill && Just action == maction
      then max 0 (n + m)
      else n
  applyModifier _ n = n

baseSkillValueFor
  :: SkillType -> Maybe Action -> [ModifierType] -> InvestigatorAttrs -> Int
baseSkillValueFor skill _maction tempModifiers attrs = foldr
  applyModifier
  baseSkillValue
  tempModifiers
 where
  applyModifier (BaseSkillOf skillType m) _ | skillType == skill = m
  applyModifier _ n = n
  baseSkillValue = case skill of
    SkillWillpower -> investigatorWillpower attrs
    SkillIntellect -> investigatorIntellect attrs
    SkillCombat -> investigatorCombat attrs
    SkillAgility -> investigatorAgility attrs
    SkillWild -> error "investigators do not have wild skills"

damageValueFor :: Int -> InvestigatorAttrs -> GameT Int
damageValueFor baseValue attrs = do
  source <- fromJustNote "damage outside skill test" <$> getSkillTestSource
  modifiers <- getModifiers source (InvestigatorTarget $ investigatorId attrs)
  pure $ foldr applyModifier baseValue modifiers
 where
  applyModifier (DamageDealt m) n = max 0 (n + m)
  applyModifier NoDamageDealt _ = 0
  applyModifier _ n = n

getIsScenarioAbility :: GameT Bool
getIsScenarioAbility = do
  source <- fromJustNote "damage outside skill test" <$> getSkillTestSource
  case source of
    SkillTestSource _ _ source' _ -> case source' of
      EnemySource _ -> pure True
      AgendaSource _ -> pure True
      LocationSource _ -> pure True
      TreacherySource tid ->
        -- If treachery has a subtype then it is a weakness not an encounter card
        isNothing . cdCardSubType <$> getCardDef tid
      ActSource _ -> pure True
      _ -> pure False
    _ -> pure False

getHandSize :: InvestigatorAttrs -> GameT Int
getHandSize attrs = do
  source <- fromMaybe (toSource attrs) <$> getSkillTestSource
  modifiers <- getModifiers source (InvestigatorTarget $ investigatorId attrs)
  pure $ foldr applyModifier 8 modifiers
 where
  applyModifier (HandSize m) n = max 0 (n + m)
  applyModifier _ n = n

getInHandCount :: InvestigatorAttrs -> GameT Int
getInHandCount attrs = do
  let
    cards = investigatorHand attrs
    applyModifier n = \case
      HandSizeCardCount m -> m
      _ -> n
    getCardHandSize c = do
      modifiers <- getModifiers (toSource attrs) (CardTarget c)
      pure $ foldl' applyModifier 1 modifiers
  sum <$> traverse getCardHandSize cards

getAbilitiesForTurn :: InvestigatorAttrs -> GameT Int
getAbilitiesForTurn attrs = do
  modifiers <- getModifiers (toSource attrs) (toTarget attrs)
  pure $ foldr applyModifier 3 modifiers
 where
  applyModifier (AdditionalActions m) n = max 0 (n + m)
  applyModifier _ n = n

getCanDiscoverClues :: InvestigatorAttrs -> GameT Bool
getCanDiscoverClues attrs = do
  modifiers <- getModifiers (toSource attrs) (toTarget attrs)
  pure $ not (any match modifiers)
 where
  match CannotDiscoverClues{} = True
  match _ = False

getCanSpendClues :: InvestigatorAttrs -> GameT Bool
getCanSpendClues attrs = do
  modifiers <- getModifiers (toSource attrs) (toTarget attrs)
  pure $ not (any match modifiers)
 where
  match CannotSpendClues{} = True
  match _ = False

getModifiedHealth :: InvestigatorAttrs -> GameT Int
getModifiedHealth attrs@InvestigatorAttrs {..} = do
  modifiers <- getModifiers (toSource attrs) (toTarget attrs)
  pure $ foldr applyModifier investigatorHealth modifiers
 where
  applyModifier (HealthModifier m) n = max 0 (n + m)
  applyModifier _ n = n

getModifiedSanity :: InvestigatorAttrs -> GameT Int
getModifiedSanity attrs@InvestigatorAttrs {..} = do
  modifiers <- getModifiers (toSource attrs) (toTarget attrs)
  pure $ foldr applyModifier investigatorSanity modifiers
 where
  applyModifier (SanityModifier m) n = max 0 (n + m)
  applyModifier _ n = n

removeFromSlots
  :: AssetId -> HashMap SlotType [Slot] -> HashMap SlotType [Slot]
removeFromSlots aid = fmap (map (removeIfMatches aid))

fitsAvailableSlots :: [SlotType] -> [Trait] -> InvestigatorAttrs -> Bool
fitsAvailableSlots slotTypes traits a = null
  (slotTypes \\ concatMap
    (\slotType -> availableSlotTypesFor slotType traits a)
    (nub slotTypes)
  )

availableSlotTypesFor :: SlotType -> [Trait] -> InvestigatorAttrs -> [SlotType]
availableSlotTypesFor slotType traits a = case lookup slotType (a ^. slotsL) of
  Nothing -> []
  Just slots ->
    replicate (length (filter (canPutIntoSlot traits) slots)) slotType

placeInAvailableSlot :: AssetId -> [Trait] -> [Slot] -> [Slot]
placeInAvailableSlot _ _ [] = []
placeInAvailableSlot aid traits (x : xs) = if canPutIntoSlot traits x
  then putIntoSlot aid x : xs
  else x : placeInAvailableSlot aid traits xs

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
        , investigatorClass = fromJustNote "missing class symbol"
          $ cdClassSymbol cardDef
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
        , investigatorDeck = mempty
        , investigatorDiscard = mempty
        , investigatorHand = mempty
        , investigatorTraits = cdCardTraits cardDef
        , investigatorTreacheries = mempty
        , investigatorInHandTreacheries = mempty
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
          ]
        , investigatorXp = 0
        , investigatorPhysicalTrauma = 0
        , investigatorMentalTrauma = 0
        , investigatorStartsWith = []
        , investigatorCardsUnderneath = []
        , investigatorFoundCards = mempty
        , investigatorUsedAbilities = mempty
        , investigatorTomeActions = Nothing
        }
      }

matchTarget :: InvestigatorAttrs -> ActionTarget -> Action -> Bool
matchTarget attrs (FirstOneOf as) action =
  action `elem` as && all (`notElem` investigatorActionsTaken attrs) as
matchTarget _ (IsAction a) action = action == a
matchTarget _ (EnemyAction a _) action = action == a

getActionCost :: InvestigatorAttrs -> Action -> GameT Int
getActionCost attrs a = do
  modifiers <- getModifiers (toSource attrs) (toTarget attrs)
  pure $ foldr applyModifier 1 modifiers
 where
  applyModifier (ActionCostOf match m) n =
    if matchTarget attrs match a then n + m else n
  applyModifier _ n = n

getActionCostModifier :: InvestigatorAttrs -> Maybe Action -> GameT Int
getActionCostModifier _ Nothing = pure 0
getActionCostModifier attrs (Just a) = do
  modifiers <- getModifiers (toSource attrs) (toTarget attrs)
  pure $ foldr applyModifier 0 modifiers
 where
  applyModifier (ActionCostOf match m) n =
    if matchTarget attrs match a then n + m else n
  applyModifier _ n = n

getSpendableClueCount :: InvestigatorAttrs -> GameT Int
getSpendableClueCount a = do
  canSpendClues <- getCanSpendClues a
  pure $ if canSpendClues then investigatorClues a else 0

cluesToDiscover :: InvestigatorAttrs -> Int -> GameT Int
cluesToDiscover attrs startValue = do
  msource <- getSkillTestSource
  case msource of
    Just source -> do
      modifiers <- getModifiers
        source
        (InvestigatorTarget $ investigatorId attrs)
      pure $ foldr applyModifier startValue modifiers
    Nothing -> pure startValue
 where
  applyModifier (DiscoveredClues m) n = n + m
  applyModifier _ n = n

getCanAfford :: InvestigatorAttrs -> Action -> GameT Bool
getCanAfford a@InvestigatorAttrs {..} actionType = do
  actionCost <- getActionCost a actionType
  pure $ actionCost <= investigatorRemainingActions

drawOpeningHand
  :: InvestigatorAttrs -> Int -> ([PlayerCard], [Card], [PlayerCard])
drawOpeningHand a n = go n (a ^. discardL, a ^. handL, coerce (a ^. deckL))
 where
  go 0 (d, h, cs) = (d, h, cs)
  go _ (_, _, []) =
    error "this should never happen, it means the deck was empty during drawing"
  go m (d, h, c : cs) = if isJust (cdCardSubType $ toCardDef c)
    then go m (c : d, h, cs)
    else go (m - 1) (d, PlayerCard c : h, cs)

getPossibleSkillTypeChoices
  :: SkillType -> InvestigatorAttrs -> GameT [SkillType]
getPossibleSkillTypeChoices skillType attrs = do
  modifiers <- getModifiers (toSource attrs) (toTarget attrs)
  pure $ foldr applyModifier [skillType] modifiers
 where
  applyModifier (UseSkillInPlaceOf toReplace toUse) skills
    | toReplace == skillType = toUse : skills
  applyModifier _ skills = skills

canCommitToAnotherLocation :: InvestigatorAttrs -> GameT Bool
canCommitToAnotherLocation attrs = do
  commitedCards <-
    skillTestCommittedCards . fromJustNote "no skill test" <$> getSkillTest
  let
    committedCardIds =
      map snd . filter ((== toId attrs) . fst) $ toList commitedCards
  modifiers <- getModifiers (toSource attrs) (toTarget attrs)
  pure $ any (permit committedCardIds) modifiers
 where
  permit n (CanCommitToSkillTestPerformedByAnInvestigatorAtAnotherLocation m) =
    m > length n
  permit _ _ = False

hasModifier :: InvestigatorAttrs -> ModifierType -> GameT Bool
hasModifier InvestigatorAttrs { investigatorId } m = elem m <$> getModifiers
  (InvestigatorSource investigatorId)
  (InvestigatorTarget investigatorId)

findCard :: HasCallStack => CardId -> InvestigatorAttrs -> Card
findCard cardId a =
  fromJustNote "not in hand or discard or deck"
    $ findMatch
    $ (a ^. handL)
    <> map PlayerCard (a ^. discardL)
    <> map PlayerCard (unDeck $ a ^. deckL)
  where findMatch = find ((== cardId) . toCardId)

getJustLocation :: InvestigatorId -> GameT LocationId
getJustLocation =
  fieldMap InvestigatorLocation (fromJustNote "must be at a location")

enemiesColocatedWith :: InvestigatorId -> EnemyMatcher
enemiesColocatedWith = EnemyAt . LocationWithInvestigator . InvestigatorWithId
