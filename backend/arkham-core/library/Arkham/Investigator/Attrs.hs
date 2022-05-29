{-# LANGUAGE TemplateHaskell #-}
module Arkham.Investigator.Attrs where

import Arkham.Prelude

import Arkham.Action (Action)
import Arkham.Action qualified as Action
import Arkham.Investigator.Cards (allInvestigatorCards)
import Arkham.Json
import Arkham.SkillTest
import Arkham.SkillType
import Arkham.Card
import Arkham.Card.Id
import Arkham.ClassSymbol
import Arkham.Classes
import Arkham.Game.Helpers hiding (windows)
import Arkham.Helpers
import Arkham.Id
import Arkham.Modifier
import Arkham.Name
import Arkham.Projection
import Arkham.Query
import Arkham.Slot
import Arkham.Source
import Arkham.Stats
import Arkham.Target
import Arkham.Trait hiding (Cultist)
import Arkham.Window (Window(..))
import Arkham.Zone (Zone)
import Data.HashSet qualified as HashSet
import Data.Text qualified as T
import Data.UUID (nil)

class IsInvestigator a

type InvestigatorCard a = CardBuilder () a

data instance Field InvestigatorAttrs :: Type -> Type where
  InvestigatorRemainingActions :: Field InvestigatorAttrs Int
  InvestigatorLocation :: Field InvestigatorAttrs (Maybe LocationId)
  InvestigatorHorror :: Field InvestigatorAttrs Int
  InvestigatorResources :: Field InvestigatorAttrs Int

data InvestigatorAttrs = InvestigatorAttrs
  { investigatorId :: InvestigatorId
  , investigatorName :: Name
  , investigatorCardCode :: CardCode
  , investigatorClass :: ClassSymbol
  , investigatorHealth :: Int
  , investigatorSanity :: Int
  , investigatorWillpower :: Int
  , investigatorIntellect :: Int
  , investigatorCombat :: Int
  , investigatorAgility :: Int
  , investigatorHealthDamage :: Int
  , investigatorSanityDamage :: Int
  , investigatorClues :: Int
  , investigatorDoom :: Int
  , investigatorResources :: Int
  , investigatorLocation :: LocationId
  , investigatorActionsTaken :: [Action]
  , investigatorRemainingActions :: Int
  , investigatorEndedTurn :: Bool
  , investigatorEngagedEnemies :: HashSet EnemyId
  , investigatorAssets :: HashSet AssetId
  , investigatorDeck :: Deck PlayerCard
  , investigatorDiscard :: [PlayerCard]
  , investigatorHand :: [Card]
  , investigatorTraits :: HashSet Trait
  , investigatorTreacheries :: HashSet TreacheryId
  , investigatorInHandTreacheries :: HashSet TreacheryId
  , investigatorDefeated :: Bool
  , investigatorResigned :: Bool
  , investigatorSlots :: HashMap SlotType [Slot]
  , investigatorXp :: Int
  , investigatorPhysicalTrauma :: Int
  , investigatorMentalTrauma :: Int
  , investigatorStartsWith :: [CardDef]
  , investigatorCardsUnderneath :: [Card]
  , investigatorFoundCards :: HashMap Zone [Card]
  -- investigator-specific fields
  , investigatorTomeActions :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''InvestigatorAttrs

instance HasTraits InvestigatorAttrs where
  toTraits = investigatorTraits

instance ToGameLoggerFormat InvestigatorAttrs where
  format attrs =
    "{investigator:\""
      <> T.replace "\"" "\\\"" (display $ toName attrs)
      <> "\":"
      <> tshow (toId attrs)
      <> "}"

instance ToJSON InvestigatorAttrs where
  toJSON = genericToJSON $ aesonOptions $ Just "investigator"
  toEncoding = genericToEncoding $ aesonOptions $ Just "investigator"

instance FromJSON InvestigatorAttrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "investigator"

instance HasCount ActionRemainingCount env InvestigatorAttrs where
  getCount = pure . ActionRemainingCount . investigatorRemainingActions

instance HasList Action.TakenAction env InvestigatorAttrs where
  getList = pure . map Action.TakenAction . investigatorActionsTaken

instance Entity InvestigatorAttrs where
  type EntityId InvestigatorAttrs = InvestigatorId
  type EntityAttrs InvestigatorAttrs = InvestigatorAttrs
  toId = investigatorId
  toAttrs = id

instance HasCardDef InvestigatorAttrs where
  toCardDef e = case lookup (investigatorCardCode e) allInvestigatorCards of
    Just def -> def
    Nothing -> error $ "missing card def for enemy " <> show (investigatorCardCode e)

instance Named InvestigatorAttrs where
  toName = investigatorName

instance TargetEntity InvestigatorAttrs where
  toTarget = InvestigatorTarget . toId
  isTarget InvestigatorAttrs { investigatorId } (InvestigatorTarget iid) =
    iid == investigatorId
  isTarget attrs (SkillTestInitiatorTarget target) = isTarget attrs target
  isTarget _ _ = False

instance SourceEntity InvestigatorAttrs where
  toSource = InvestigatorSource . toId
  isSource InvestigatorAttrs { investigatorId } (InvestigatorSource iid) =
    iid == investigatorId
  isSource _ _ = False

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

damageValueFor
  :: (MonadReader env m, HasModifiersFor env (), HasSkillTest env)
  => Int
  -> InvestigatorAttrs
  -> m Int
damageValueFor baseValue attrs = do
  source <- fromJustNote "damage outside skill test" <$> getSkillTestSource
  modifiers <- getModifiers source (InvestigatorTarget $ investigatorId attrs)
  pure $ foldr applyModifier baseValue modifiers
 where
  applyModifier (DamageDealt m) n = max 0 (n + m)
  applyModifier _ n = n

getIsScenarioAbility
  :: (HasSkillTest env, MonadReader env m, CanBeWeakness env TreacheryId)
  => m Bool
getIsScenarioAbility = do
  source <- fromJustNote "damage outside skill test" <$> getSkillTestSource
  case source of
    SkillTestSource _ _ source' _ -> case source' of
      EnemySource _ -> pure True
      AgendaSource _ -> pure True
      LocationSource _ -> pure True
      TreacherySource tid -> not <$> getIsWeakness tid
      ActSource _ -> pure True
      _ -> pure False
    _ -> pure False

getHandSize
  :: (MonadReader env m, HasModifiersFor env (), HasSkillTest env)
  => InvestigatorAttrs
  -> m Int
getHandSize attrs = do
  source <- fromMaybe (toSource attrs) <$> getSkillTestSource
  modifiers <- getModifiers source (InvestigatorTarget $ investigatorId attrs)
  pure $ foldr applyModifier 8 modifiers
 where
  applyModifier (HandSize m) n = max 0 (n + m)
  applyModifier _ n = n

getInHandCount
  :: (MonadReader env m, HasModifiersFor env ()) => InvestigatorAttrs -> m Int
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

getAbilitiesForTurn
  :: (MonadReader env m, HasModifiersFor env ()) => InvestigatorAttrs -> m Int
getAbilitiesForTurn attrs = do
  modifiers <- getModifiers (toSource attrs) (toTarget attrs)
  pure $ foldr applyModifier 3 modifiers
 where
  applyModifier (AdditionalActions m) n = max 0 (n + m)
  applyModifier _ n = n

getCanDiscoverClues
  :: (MonadReader env m, HasModifiersFor env ()) => InvestigatorAttrs -> m Bool
getCanDiscoverClues attrs = do
  modifiers <- getModifiers (toSource attrs) (toTarget attrs)
  pure $ not (any match modifiers)
 where
  match CannotDiscoverClues{} = True
  match _ = False

getCanSpendClues
  :: (MonadReader env m, HasModifiersFor env ()) => InvestigatorAttrs -> m Bool
getCanSpendClues attrs = do
  modifiers <- getModifiers (toSource attrs) (toTarget attrs)
  pure $ not (any match modifiers)
 where
  match CannotSpendClues{} = True
  match _ = False

getModifiedHealth
  :: (MonadReader env m, HasModifiersFor env ()) => InvestigatorAttrs -> m Int
getModifiedHealth attrs@InvestigatorAttrs {..} = do
  modifiers <- getModifiers (toSource attrs) (toTarget attrs)
  pure $ foldr applyModifier investigatorHealth modifiers
 where
  applyModifier (HealthModifier m) n = max 0 (n + m)
  applyModifier _ n = n

getModifiedSanity
  :: (MonadReader env m, HasModifiersFor env ()) => InvestigatorAttrs -> m Int
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
  :: (InvestigatorAttrs -> a)
  -> CardDef
  -> Stats
  -> CardBuilder () a
investigator f cardDef Stats {..} = let iid = InvestigatorId (cdCardCode cardDef) in
  CardBuilder
    { cbCardCode = cdCardCode cardDef
    , cbCardBuilder = \_ -> f $ InvestigatorAttrs
      { investigatorId = iid
      , investigatorName = cdName cardDef
      , investigatorCardCode = cdCardCode cardDef
      , investigatorClass = fromJustNote "missing class symbol" $ cdClassSymbol cardDef
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
      , investigatorTomeActions = Nothing
      }
    }

matchTarget :: InvestigatorAttrs -> ActionTarget -> Action -> Bool
matchTarget attrs (FirstOneOf as) action =
  action `elem` as && all (`notElem` investigatorActionsTaken attrs) as
matchTarget _ (IsAction a) action = action == a
matchTarget _ (EnemyAction a _) action = action == a

getActionCost
  :: (MonadReader env m, HasModifiersFor env ())
  => InvestigatorAttrs
  -> Action
  -> m Int
getActionCost attrs a = do
  modifiers <- getModifiers (toSource attrs) (toTarget attrs)
  pure $ foldr applyModifier 1 modifiers
 where
  applyModifier (ActionCostOf match m) n =
    if matchTarget attrs match a then n + m else n
  applyModifier _ n = n

getActionCostModifier
  :: (MonadReader env m, HasModifiersFor env ())
  => InvestigatorAttrs
  -> Maybe Action
  -> m Int
getActionCostModifier _ Nothing = pure 0
getActionCostModifier attrs (Just a) = do
  modifiers <- getModifiers (toSource attrs) (toTarget attrs)
  pure $ foldr applyModifier 0 modifiers
 where
  applyModifier (ActionCostOf match m) n =
    if matchTarget attrs match a then n + m else n
  applyModifier _ n = n

getSpendableClueCount
  :: (MonadReader env m, HasModifiersFor env ()) => InvestigatorAttrs -> m Int
getSpendableClueCount a = do
  canSpendClues <- getCanSpendClues a
  pure $ if canSpendClues then investigatorClues a else 0

cluesToDiscover
  :: (MonadReader env m, HasModifiersFor env (), HasSkillTest env)
  => InvestigatorAttrs
  -> Int
  -> m Int
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

getCanAfford
  :: (MonadReader env m, HasModifiersFor env ())
  => InvestigatorAttrs
  -> Action
  -> m Bool
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

getPlayableCards
  :: (HasCallStack, MonadReader env m, CanCheckPlayable env)
  => InvestigatorAttrs
  -> CostStatus
  -> [Window]
  -> m [Card]
getPlayableCards a@InvestigatorAttrs {..} costStatus windows = do
  asIfInHandCards <- getAsIfInHandCards a
  playableDiscards <- getPlayableDiscards a costStatus windows
  playableHandCards <- filterM
    (getIsPlayable (toId a) (toSource a) costStatus windows)
    (investigatorHand <> asIfInHandCards)
  pure $ playableHandCards <> playableDiscards

getPlayableDiscards
  :: (MonadReader env m, CanCheckPlayable env)
  => InvestigatorAttrs
  -> CostStatus
  -> [Window]
  -> m [Card]
getPlayableDiscards attrs@InvestigatorAttrs {..} costStatus windows = do
  modifiers <- getModifiers (toSource attrs) (toTarget attrs)
  filterM
    (getIsPlayable (toId attrs) (toSource attrs) costStatus windows)
    (possibleCards modifiers)
 where
  possibleCards modifiers = map (PlayerCard . snd) $ filter
    (canPlayFromDiscard modifiers)
    (zip @_ @Int [0 ..] investigatorDiscard)
  canPlayFromDiscard modifiers (n, card) =
    cdPlayableFromDiscard (toCardDef card)
      || any (allowsPlayFromDiscard n card) modifiers
  allowsPlayFromDiscard 0 card (CanPlayTopOfDiscard (mcardType, traits)) =
    maybe True (== cdCardType (toCardDef card)) mcardType
      && (null traits
         || (setFromList traits `HashSet.isSubsetOf` toTraits (toCardDef card)
            )
         )
  allowsPlayFromDiscard _ _ _ = False

getPossibleSkillTypeChoices
  :: (MonadReader env m, HasModifiersFor env ())
  => SkillType
  -> InvestigatorAttrs
  -> m [SkillType]
getPossibleSkillTypeChoices skillType attrs = do
  modifiers <- getModifiers (toSource attrs) (toTarget attrs)
  pure $ foldr applyModifier [skillType] modifiers
 where
  applyModifier (UseSkillInPlaceOf toReplace toUse) skills
    | toReplace == skillType = toUse : skills
  applyModifier _ skills = skills

canCommitToAnotherLocation
  :: ( MonadReader env m
     , HasModifiersFor env ()
     , HasSet CommittedCardId env InvestigatorId
     )
  => InvestigatorAttrs
  -> m Bool
canCommitToAnotherLocation attrs = do
  modifiers <- getModifiers (toSource attrs) (toTarget attrs)
  committedCardIds <- map unCommittedCardId <$> getSetList (toId attrs)
  pure $ any (permit committedCardIds) modifiers
 where
  permit n (CanCommitToSkillTestPerformedByAnInvestigatorAtAnotherLocation m) =
    m > length n
  permit _ _ = False


hasModifier
  :: (MonadReader env m, HasModifiersFor env ())
  => InvestigatorAttrs
  -> ModifierType
  -> m Bool
hasModifier InvestigatorAttrs { investigatorId } m = elem m <$> getModifiers
  (InvestigatorSource investigatorId)
  (InvestigatorTarget investigatorId)

findCard :: CardId -> InvestigatorAttrs -> Card
findCard cardId a =
  fromJustNote "not in hand or discard or deck"
    $ findMatch
    $ (a ^. handL)
    <> map PlayerCard (a ^. discardL)
    <> map PlayerCard (unDeck $ a ^. deckL)
  where findMatch = find ((== cardId) . toCardId)

getAsIfInHandCards
  :: (MonadReader env m, HasModifiersFor env ())
  => InvestigatorAttrs
  -> m [Card]
getAsIfInHandCards attrs = do
  modifiers <- getModifiers (toSource attrs) (toTarget attrs)
  let
    modifiersPermitPlayOfDiscard c =
      any (modifierPermitsPlayOfDiscard c) modifiers
    modifierPermitsPlayOfDiscard (c, depth) = \case
      CanPlayTopOfDiscard (mType, traits) | depth == 0 ->
        maybe True (== toCardType c) mType
          && (null traits || notNull
               (setFromList traits `intersection` toTraits c)
             )
      _ -> False
    modifiersPermitPlayOfDeck c = any (modifierPermitsPlayOfDeck c) modifiers
    modifierPermitsPlayOfDeck (c, depth) = \case
      CanPlayTopOfDeck cardMatcher | depth == 0 -> cardMatch c cardMatcher
      _ -> False
  pure
    $ map
        (PlayerCard . fst)
        (filter
          modifiersPermitPlayOfDiscard
          (zip (investigatorDiscard attrs) [0 :: Int ..])
        )
    <> map
         (PlayerCard . fst)
         (filter
           modifiersPermitPlayOfDeck
           (zip (unDeck $ investigatorDeck attrs) [0 :: Int ..])
         )
