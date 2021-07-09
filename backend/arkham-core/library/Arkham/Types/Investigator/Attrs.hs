module Arkham.Types.Investigator.Attrs where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Action (Action)
import qualified Arkham.Types.Action as Action
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.ClassSymbol
import Arkham.Types.Classes hiding (discard)
import Arkham.Types.CommitRestriction
import Arkham.Types.Cost
import Arkham.Types.EnemyId
import Arkham.Types.EntityInstance
import Arkham.Types.Helpers
import Arkham.Types.Investigator.Runner
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.PlayRestriction
import Arkham.Types.Query
import Arkham.Types.SkillTest
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.TreacheryId
import Arkham.Types.Window
import Control.Monad.Extra (allM)
import qualified Data.HashSet as HashSet
import Data.UUID (nil)

data InvestigatorAttrs = InvestigatorAttrs
  { investigatorName :: Name
  , investigatorClass :: ClassSymbol
  , investigatorId :: InvestigatorId
  , investigatorHealth :: Int
  , investigatorSanity :: Int
  , investigatorWillpower :: Int
  , investigatorIntellect :: Int
  , investigatorCombat :: Int
  , investigatorAgility :: Int
  , investigatorHealthDamage :: Int
  , investigatorSanityDamage :: Int
  , investigatorClues :: Int
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
  , investigatorConnectedLocations :: HashSet LocationId
  , investigatorTraits :: HashSet Trait
  , investigatorTreacheries :: HashSet TreacheryId
  , investigatorDefeated :: Bool
  , investigatorResigned :: Bool
  , investigatorSlots :: HashMap SlotType [Slot]
  , investigatorXp :: Int
  , investigatorPhysicalTrauma :: Int
  , investigatorMentalTrauma :: Int
  -- investigator-specific fields
  , investigatorTomeActions :: Maybe Int
  }
  deriving stock (Show, Generic)

willpowerL :: Lens' InvestigatorAttrs Int
willpowerL =
  lens investigatorWillpower $ \m x -> m { investigatorWillpower = x }

intellectL :: Lens' InvestigatorAttrs Int
intellectL =
  lens investigatorIntellect $ \m x -> m { investigatorIntellect = x }

combatL :: Lens' InvestigatorAttrs Int
combatL = lens investigatorCombat $ \m x -> m { investigatorCombat = x }

agilityL :: Lens' InvestigatorAttrs Int
agilityL = lens investigatorAgility $ \m x -> m { investigatorAgility = x }

treacheriesL :: Lens' InvestigatorAttrs (HashSet TreacheryId)
treacheriesL =
  lens investigatorTreacheries $ \m x -> m { investigatorTreacheries = x }

assetsL :: Lens' InvestigatorAttrs (HashSet AssetId)
assetsL = lens investigatorAssets $ \m x -> m { investigatorAssets = x }

healthDamageL :: Lens' InvestigatorAttrs Int
healthDamageL =
  lens investigatorHealthDamage $ \m x -> m { investigatorHealthDamage = x }

sanityDamageL :: Lens' InvestigatorAttrs Int
sanityDamageL =
  lens investigatorSanityDamage $ \m x -> m { investigatorSanityDamage = x }

locationL :: Lens' InvestigatorAttrs LocationId
locationL = lens investigatorLocation $ \m x -> m { investigatorLocation = x }

connectedLocationsL :: Lens' InvestigatorAttrs (HashSet LocationId)
connectedLocationsL = lens investigatorConnectedLocations
  $ \m x -> m { investigatorConnectedLocations = x }

slotsL :: Lens' InvestigatorAttrs (HashMap SlotType [Slot])
slotsL = lens investigatorSlots $ \m x -> m { investigatorSlots = x }

endedTurnL :: Lens' InvestigatorAttrs Bool
endedTurnL =
  lens investigatorEndedTurn $ \m x -> m { investigatorEndedTurn = x }

defeatedL :: Lens' InvestigatorAttrs Bool
defeatedL = lens investigatorDefeated $ \m x -> m { investigatorDefeated = x }

resignedL :: Lens' InvestigatorAttrs Bool
resignedL = lens investigatorResigned $ \m x -> m { investigatorResigned = x }

resourcesL :: Lens' InvestigatorAttrs Int
resourcesL =
  lens investigatorResources $ \m x -> m { investigatorResources = x }

remainingActionsL :: Lens' InvestigatorAttrs Int
remainingActionsL = lens investigatorRemainingActions
  $ \m x -> m { investigatorRemainingActions = x }

actionsTakenL :: Lens' InvestigatorAttrs [Action]
actionsTakenL =
  lens investigatorActionsTaken $ \m x -> m { investigatorActionsTaken = x }

handL :: Lens' InvestigatorAttrs [Card]
handL = lens investigatorHand $ \m x -> m { investigatorHand = x }

engagedEnemiesL :: Lens' InvestigatorAttrs (HashSet EnemyId)
engagedEnemiesL =
  lens investigatorEngagedEnemies $ \m x -> m { investigatorEngagedEnemies = x }

deckL :: Lens' InvestigatorAttrs (Deck PlayerCard)
deckL = lens investigatorDeck $ \m x -> m { investigatorDeck = x }

discardL :: Lens' InvestigatorAttrs [PlayerCard]
discardL = lens investigatorDiscard $ \m x -> m { investigatorDiscard = x }

cluesL :: Lens' InvestigatorAttrs Int
cluesL = lens investigatorClues $ \m x -> m { investigatorClues = x }

xpL :: Lens' InvestigatorAttrs Int
xpL = lens investigatorXp $ \m x -> m { investigatorXp = x }

mentalTraumaL :: Lens' InvestigatorAttrs Int
mentalTraumaL =
  lens investigatorMentalTrauma $ \m x -> m { investigatorMentalTrauma = x }

physicalTraumaL :: Lens' InvestigatorAttrs Int
physicalTraumaL =
  lens investigatorPhysicalTrauma $ \m x -> m { investigatorPhysicalTrauma = x }

instance HasTraits InvestigatorAttrs where
  toTraits = investigatorTraits

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

skillValueFor
  :: SkillType -> Maybe Action -> [Modifier] -> InvestigatorAttrs -> Int
skillValueFor skill maction tempModifiers attrs = foldr
  (applyModifier . modifierType)
  (baseSkillValueFor skill maction tempModifiers attrs)
  tempModifiers
 where
  applyModifier (AnySkillValue m) n = max 0 (n + m)
  applyModifier (SkillModifier skillType m) n =
    if skillType == skill then max 0 (n + m) else n
  applyModifier (ActionSkillModifier action skillType m) n =
    if skillType == skill && Just action == maction then max 0 (n + m) else n
  applyModifier _ n = n

baseSkillValueFor
  :: SkillType -> Maybe Action -> [Modifier] -> InvestigatorAttrs -> Int
baseSkillValueFor skill _maction tempModifiers attrs = foldr
  (applyModifier . modifierType)
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
  modifiers <-
    map modifierType
      <$> getModifiersFor source (InvestigatorTarget $ investigatorId attrs) ()
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
    SkillTestSource _ _ source' _ _ -> case source' of
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
  modifiers <-
    map modifierType
      <$> getModifiersFor source (InvestigatorTarget $ investigatorId attrs) ()
  pure $ foldr applyModifier 8 modifiers
 where
  applyModifier (HandSize m) n = max 0 (n + m)
  applyModifier _ n = n

getActionsForTurn
  :: (MonadReader env m, HasModifiersFor env ()) => InvestigatorAttrs -> m Int
getActionsForTurn attrs = do
  modifiers <-
    map modifierType <$> getModifiersFor (toSource attrs) (toTarget attrs) ()
  pure $ foldr applyModifier 3 modifiers
 where
  applyModifier (AdditionalActions m) n = max 0 (n + m)
  applyModifier _ n = n

getCanDiscoverClues
  :: (MonadReader env m, HasModifiersFor env ()) => InvestigatorAttrs -> m Bool
getCanDiscoverClues attrs = do
  modifiers <-
    map modifierType <$> getModifiersFor (toSource attrs) (toTarget attrs) ()
  pure $ not (any match modifiers)
 where
  match CannotDiscoverClues{} = True
  match _ = False

getCanSpendClues
  :: (MonadReader env m, HasModifiersFor env ()) => InvestigatorAttrs -> m Bool
getCanSpendClues attrs = do
  modifiers <-
    map modifierType <$> getModifiersFor (toSource attrs) (toTarget attrs) ()
  pure $ not (any match modifiers)
 where
  match CannotSpendClues{} = True
  match _ = False

getModifiedHealth
  :: (MonadReader env m, HasModifiersFor env ()) => InvestigatorAttrs -> m Int
getModifiedHealth attrs@InvestigatorAttrs {..} = do
  modifiers <-
    map modifierType <$> getModifiersFor (toSource attrs) (toTarget attrs) ()
  pure $ foldr applyModifier investigatorHealth modifiers
 where
  applyModifier (HealthModifier m) n = max 0 (n + m)
  applyModifier _ n = n

getModifiedSanity
  :: (MonadReader env m, HasModifiersFor env ()) => InvestigatorAttrs -> m Int
getModifiedSanity attrs@InvestigatorAttrs {..} = do
  modifiers <-
    map modifierType <$> getModifiersFor (toSource attrs) (toTarget attrs) ()
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
placeInAvailableSlot _ _ [] = error "could not find empty slot"
placeInAvailableSlot aid traits (x : xs) = if canPutIntoSlot traits x
  then putIntoSlot aid x : xs
  else x : placeInAvailableSlot aid traits xs

discardableAssets :: SlotType -> InvestigatorAttrs -> [AssetId]
discardableAssets slotType a = case lookup slotType (a ^. slotsL) of
  Nothing -> []
  Just slots -> mapMaybe slotItem slots

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

baseAttrs
  :: InvestigatorId
  -> Name
  -> ClassSymbol
  -> Stats
  -> [Trait]
  -> InvestigatorAttrs
baseAttrs iid name classSymbol Stats {..} traits = InvestigatorAttrs
  { investigatorName = name
  , investigatorId = iid
  , investigatorClass = classSymbol
  , investigatorHealth = health
  , investigatorSanity = sanity
  , investigatorWillpower = willpower
  , investigatorIntellect = intellect
  , investigatorCombat = combat
  , investigatorAgility = agility
  , investigatorHealthDamage = 0
  , investigatorSanityDamage = 0
  , investigatorClues = 0
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
  , investigatorConnectedLocations = mempty
  , investigatorTraits = setFromList traits
  , investigatorTreacheries = mempty
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
  , investigatorTomeActions = Nothing
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
  modifiers <-
    map modifierType <$> getModifiersFor (toSource attrs) (toTarget attrs) ()
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
  modifiers <-
    map modifierType <$> getModifiersFor (toSource attrs) (toTarget attrs) ()
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
  source <- fromJustNote "damage outside skill test" <$> getSkillTestSource
  modifiers <-
    map modifierType
      <$> getModifiersFor source (InvestigatorTarget $ investigatorId attrs) ()
  pure $ foldr applyModifier startValue modifiers
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

getFastIsPlayable
  :: ( MonadReader env m
     , HasModifiersFor env ()
     , HasSet InvestigatorId env LocationId
     , HasCount ClueCount env LocationId
     , HasActions env ActionType
     , MonadIO m
     )
  => InvestigatorAttrs
  -> [WindowType]
  -> Card
  -> m Bool
getFastIsPlayable _ _ (EncounterCard _) = pure False -- TODO: there might be some playable ones?
getFastIsPlayable attrs windows c@(PlayerCard MkPlayerCard {..}) = do
  modifiers <-
    map modifierType <$> getModifiersFor (toSource attrs) (toTarget attrs) ()
  isPlayable <- getIsPlayable attrs windows c
  pure $ (cdFast pcDef || canBecomeFast modifiers) && isPlayable
 where
  canBecomeFast modifiers = foldr applyModifier False modifiers
  applyModifier (CanBecomeFast (mcardType, traits)) _
    | maybe True (== cdCardType pcDef) mcardType
      && notNull (setFromList traits `intersect` toTraits pcDef)
    = True
  applyModifier _ val = val

getModifiedCardCost
  :: (MonadReader env m, HasModifiersFor env ())
  => InvestigatorAttrs
  -> Card
  -> m Int
getModifiedCardCost attrs (PlayerCard MkPlayerCard {..}) = do
  modifiers <-
    map modifierType <$> getModifiersFor (toSource attrs) (toTarget attrs) ()
  pure $ foldr applyModifier startingCost modifiers
 where
  startingCost = case cdCost pcDef of
    Just (StaticCost n) -> n
    Just DynamicCost -> 0
    Nothing -> 0
  applyModifier (ReduceCostOf traits m) n
    | notNull (setFromList traits `intersection` toTraits pcDef) = max 0 (n - m)
  applyModifier (ReduceCostOfCardType cardType m) n
    | cardType == cdCardType pcDef = max 0 (n - m)
  applyModifier _ n = n
getModifiedCardCost attrs (EncounterCard MkEncounterCard {..}) = do
  modifiers <-
    map modifierType <$> getModifiersFor (toSource attrs) (toTarget attrs) ()
  pure $ foldr
    applyModifier
    (error "we need so specify ecCost for this to work")
    modifiers
 where
  applyModifier (ReduceCostOf traits m) n
    | notNull (setFromList traits `intersection` toTraits ecDef) = max 0 (n - m)
  applyModifier _ n = n

getIsPlayable
  :: ( MonadReader env m
     , HasModifiersFor env ()
     , HasSet InvestigatorId env LocationId
     , HasCount ClueCount env LocationId
     , HasActions env ActionType
     , MonadIO m
     )
  => InvestigatorAttrs
  -> [WindowType]
  -> Card
  -> m Bool
getIsPlayable _ _ (EncounterCard _) = pure False -- TODO: there might be some playable ones?
getIsPlayable attrs@InvestigatorAttrs {..} windows c@(PlayerCard MkPlayerCard {..})
  = do
    modifiers <-
      map modifierType <$> getModifiersFor (toSource attrs) (toTarget attrs) ()
    modifiedCardCost <- getModifiedCardCost attrs c
    passesRestrictions <- allM passesRestriction (cdPlayRestrictions pcDef)
    pure
      $ (cdCardType pcDef /= SkillType)
      && (modifiedCardCost <= investigatorResources)
      && none prevents modifiers
      && (not (cdFast pcDef) || (cdFast pcDef && cardInWindows windows c attrs))
      && (cdAction pcDef /= Just Action.Evade || not
           (null investigatorEngagedEnemies)
         )
      && passesRestrictions
 where
  prevents (CannotPlay typePairs) = any
    (\(cType, traits) ->
      cdCardType pcDef
        == cType
        && (null traits || notNull (intersection (toTraits pcDef) traits))
    )
    typePairs
  prevents _ = False
  passesRestriction = \case
    ClueOnLocation -> (> 0) . unClueCount <$> getCount investigatorLocation
    AnotherInvestigatorInSameLocation ->
      notNull <$> getSet @InvestigatorId investigatorLocation
    ScenarioCardHasResignAbility -> do
      actions' <- concat . concat <$> sequence
        [ traverse
            (getActions investigatorId window)
            ([minBound .. maxBound] :: [ActionType])
        | window <- windows
        ]
      pure $ flip
        any
        actions'
        \case
          UseAbility _ ability -> case abilityType ability of
            ActionAbility (Just Action.Resign) _ -> True
            _ -> False
          _ -> False

drawOpeningHand
  :: InvestigatorAttrs -> Int -> ([PlayerCard], [Card], [PlayerCard])
drawOpeningHand a n = go n (a ^. discardL, a ^. handL, coerce (a ^. deckL))
 where
  go 0 (d, h, cs) = (d, h, cs)
  go _ (_, _, []) =
    error "this should never happen, it means the deck was empty during drawing"
  go m (d, h, c : cs) = if cdWeakness (pcDef c)
    then go m (c : d, h, cs)
    else go (m - 1) (d, PlayerCard c : h, cs)

cardInWindows :: [WindowType] -> Card -> InvestigatorAttrs -> Bool
cardInWindows windows c _ = case c of
  PlayerCard pc ->
    notNull $ cdWindows (pcDef pc) `intersect` setFromList windows
  _ -> False

getPlayableCards
  :: ( MonadReader env m
     , HasModifiersFor env ()
     , HasSet InvestigatorId env LocationId
     , HasCount ClueCount env LocationId
     , HasActions env ActionType
     , MonadIO m
     )
  => InvestigatorAttrs
  -> [WindowType]
  -> m [Card]
getPlayableCards a@InvestigatorAttrs {..} windows = do
  playableDiscards <- getPlayableDiscards a windows
  playableHandCards <- filterM (getFastIsPlayable a windows) investigatorHand
  pure $ playableHandCards <> playableDiscards

getPlayableDiscards
  :: ( MonadReader env m
     , HasModifiersFor env ()
     , HasSet InvestigatorId env LocationId
     , HasCount ClueCount env LocationId
     , HasActions env ActionType
     , MonadIO m
     )
  => InvestigatorAttrs
  -> [WindowType]
  -> m [Card]
getPlayableDiscards attrs@InvestigatorAttrs {..} windows = do
  modifiers <-
    map modifierType <$> getModifiersFor (toSource attrs) (toTarget attrs) ()
  filterM (getFastIsPlayable attrs windows) (possibleCards modifiers)
 where
  possibleCards modifiers = map (PlayerCard . snd) $ filter
    (canPlayFromDiscard modifiers)
    (zip @_ @Int [0 ..] investigatorDiscard)
  canPlayFromDiscard modifiers (n, card) =
    any (allowsPlayFromDiscard n card) modifiers
  allowsPlayFromDiscard 0 MkPlayerCard {..} (CanPlayTopOfDiscard (mcardType, traits))
    = maybe True (== cdCardType pcDef) mcardType
      && (null traits
         || (setFromList traits `HashSet.isSubsetOf` toTraits pcDef)
         )
  allowsPlayFromDiscard _ _ _ = False


getPossibleSkillTypeChoices
  :: (MonadReader env m, HasModifiersFor env ())
  => SkillType
  -> InvestigatorAttrs
  -> m [SkillType]
getPossibleSkillTypeChoices skillType attrs = do
  modifiers <-
    map modifierType <$> getModifiersFor (toSource attrs) (toTarget attrs) ()
  pure $ foldr applyModifier [skillType] modifiers
 where
  applyModifier (UseSkillInPlaceOf toReplace toUse) skills
    | toReplace == skillType = toUse : skills
  applyModifier _ skills = skills

instance HasModifiersFor env InvestigatorAttrs where
  getModifiersFor = noModifiersFor

instance HasActions env InvestigatorAttrs where
-- instance (ActionRunner env, HasSkillTest env) => HasActions env InvestigatorAttrs where
  -- getActions iid window attrs | iid == investigatorId attrs = concat <$> for
  --   (attrs ^.. handL . traverse . _PlayerCard)
  --   (getActions iid (InHandWindow iid window) . toCardInstance iid . PlayerCard)
  getActions _ _ _ = pure []

instance HasTokenValue env InvestigatorAttrs where
  getTokenValue _ _ _ = error "should not be asking this here"

instance InvestigatorRunner env => RunMessage env InvestigatorAttrs where
  runMessage msg i | doNotMask msg = do
    traverseOf_
      (handL . traverse . _PlayerCard)
      (runMessage msg . toCardInstance (toId i) . PlayerCard)
      i
    traverseOf_
      (discardL . traverse)
      (runMessage msg . toCardInstance (toId i) . PlayerCard)
      i
    runInvestigatorMessage msg i
  runMessage msg i = do
    traverseOf_
      (handL . traverse . _PlayerCard)
      (runMessage (InHand (toId i) msg) . toCardInstance (toId i) . PlayerCard)
      i
    traverseOf_
      (discardL . traverse)
      (runMessage (InDiscard (toId i) msg)
      . toCardInstance (toId i)
      . PlayerCard
      )
      i
    runInvestigatorMessage msg i

hasModifier
  :: (MonadReader env m, HasModifiersFor env ())
  => InvestigatorAttrs
  -> ModifierType
  -> m Bool
hasModifier InvestigatorAttrs { investigatorId } m =
  elem m
    . map modifierType
    <$> getModifiersFor
          (InvestigatorSource investigatorId)
          (InvestigatorTarget investigatorId)
          ()

isForced :: Message -> Bool
isForced (UseAbility _ Ability { abilityType }) = abilityType == ForcedAbility
isForced _ = False

runInvestigatorMessage
  :: (InvestigatorRunner env, MonadReader env m, MonadRandom m, MonadIO m)
  => Message
  -> InvestigatorAttrs
  -> m InvestigatorAttrs
runInvestigatorMessage msg a@InvestigatorAttrs {..} = case msg of
  ResetGame -> pure $ (baseAttrs
                        investigatorId
                        investigatorName
                        investigatorClass
                        (getAttrStats a)
                        (setToList investigatorTraits)
                      )
    { investigatorXp = investigatorXp
    , investigatorPhysicalTrauma = investigatorPhysicalTrauma
    , investigatorMentalTrauma = investigatorMentalTrauma
    , investigatorSanityDamage = investigatorMentalTrauma
    , investigatorHealthDamage = investigatorPhysicalTrauma
    }
  SetupInvestigators -> do
    let
      (permanentCards, deck') =
        partition (cdPermanent . pcDef) (unDeck investigatorDeck)
      (discard, hand, deck) = drawOpeningHand (a & deckL .~ Deck deck') 5
    pushAll
      $ [ PutCardIntoPlay investigatorId (PlayerCard card) Nothing
        | card <- permanentCards
        ]
      <> [TakeStartingResources investigatorId]
    pure $ a & (discardL .~ discard) & (handL .~ hand) & (deckL .~ Deck deck)
  TakeStartingResources iid | iid == investigatorId -> do
    modifiers' <-
      map modifierType <$> getModifiersFor (toSource a) (toTarget a) ()
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
  AllRandomDiscard | not (a ^. defeatedL || a ^. resignedL) ->
    a <$ push (RandomDiscard investigatorId)
  RandomDiscard iid | iid == investigatorId -> do
    n <- getRandomR (0, length investigatorHand - 1)
    case investigatorHand !!? n of
      Nothing -> pure a
      Just c -> a <$ push (DiscardCard investigatorId (toCardId c))
  FinishedWithMulligan iid | iid == investigatorId -> do
    let (discard, hand, deck) = drawOpeningHand a (5 - length investigatorHand)
    push (ShuffleDiscardBackIn iid)
    pure
      $ a
      & (resourcesL .~ 5)
      & (discardL .~ discard)
      & (handL .~ hand)
      & (deckL .~ Deck deck)
  ShuffleDiscardBackIn iid | iid == investigatorId ->
    if notNull investigatorDiscard
      then do
        deck <- shuffleM (investigatorDiscard <> coerce investigatorDeck)
        pure $ a & discardL .~ [] & deckL .~ Deck deck
      else pure a
  Resign iid | iid == investigatorId -> do
    pushAll $ resolve $ InvestigatorResigned iid
    pure $ a & resignedL .~ True
  InvestigatorDefeated iid | iid == investigatorId ->
    a <$ push (InvestigatorWhenEliminated (toSource a) iid)
  InvestigatorResigned iid | iid == investigatorId ->
    a <$ push (InvestigatorWhenEliminated (toSource a) iid)
  -- InvestigatorWhenEliminated is handled by the scenario
  InvestigatorEliminated iid | iid == investigatorId -> do
    push (PlaceClues (LocationTarget investigatorLocation) investigatorClues)
    pure $ a & cluesL .~ 0 & resourcesL .~ 0
  EnemyMove eid _ lid | lid /= investigatorLocation ->
    pure $ a & engagedEnemiesL %~ deleteSet eid
  EnemyEngageInvestigator eid iid | iid == investigatorId ->
    pure $ a & engagedEnemiesL %~ insertSet eid
  RemoveEnemy eid -> pure $ a & engagedEnemiesL %~ deleteSet eid
  TakeControlOfAsset iid aid | iid == investigatorId ->
    pure $ a & assetsL %~ insertSet aid
  TakeControlOfAsset iid aid | iid /= investigatorId ->
    pure $ a & assetsL %~ deleteSet aid
  ChooseAndDiscardAsset iid | iid == investigatorId -> do
    discardableAssetIds <- map unDiscardableAssetId <$> getSetList iid
    a <$ push (chooseOne iid $ map (Discard . AssetTarget) discardableAssetIds)
  AttachAsset aid _ | aid `member` investigatorAssets ->
    pure $ a & assetsL %~ deleteSet aid
  AttachTreachery tid (InvestigatorTarget iid) | iid == investigatorId ->
    pure $ a & treacheriesL %~ insertSet tid
  AllCheckHandSize | not (a ^. defeatedL || a ^. resignedL) -> do
    handSize <- getHandSize a
    when (length investigatorHand > handSize)
      $ push (CheckHandSize investigatorId)
    pure a
  CheckHandSize iid | iid == investigatorId -> do
    handSize <- getHandSize a
    when (length investigatorHand > handSize) $ push
      (chooseOne
        iid
        [ Run [DiscardCard iid (toCardId card), CheckHandSize iid]
        | card <- filter (not . cdWeakness . pcDef)
          $ mapMaybe (preview _PlayerCard) investigatorHand
        ]
      )
    pure a
  AddToDiscard iid pc | iid == investigatorId -> pure $ a & discardL %~ (pc :)
  ChooseAndDiscardCard iid | iid == investigatorId -> a <$ push
    (chooseOne iid
    $ [ DiscardCard iid (toCardId card) | card <- discardableCards a ]
    )
  DiscardCard iid cardId | iid == investigatorId -> do
    let
      card = fromJustNote "must be in hand"
        $ find ((== cardId) . toCardId) investigatorHand
    case card of
      PlayerCard pc ->
        pure $ a & handL %~ filter ((/= cardId) . toCardId) & discardL %~ (pc :)
      EncounterCard _ -> pure $ a & handL %~ filter ((/= cardId) . toCardId) -- TODO: This should discard to the encounter discard
  RemoveCardFromHand iid cardCode | iid == investigatorId ->
    pure $ a & handL %~ filter ((/= cardCode) . toCardCode)
  ShuffleIntoDeck iid (TreacheryTarget tid) | iid == investigatorId ->
    pure $ a & treacheriesL %~ deleteSet tid
  ShuffleIntoDeck iid (AssetTarget aid) | iid == investigatorId -> do
    card <- fromJustNote "missing card" <$> getPlayerCard aid
    deck' <- shuffleM (card : unDeck investigatorDeck)
    push $ After msg
    pure
      $ a
      & assetsL
      %~ deleteSet aid
      & deckL
      .~ Deck deck'
      & slotsL
      %~ removeFromSlots aid
  Discard (TreacheryTarget tid) -> pure $ a & treacheriesL %~ deleteSet tid
  Discard (EnemyTarget eid) -> pure $ a & engagedEnemiesL %~ deleteSet eid
  Discarded (AssetTarget aid) (PlayerCard card)
    | aid `elem` investigatorAssets
    -> pure
      $ a
      & (assetsL %~ deleteSet aid)
      & (discardL %~ (card :))
      & (slotsL %~ removeFromSlots aid)
  Discarded (AssetTarget aid) (EncounterCard _)
    | aid `elem` investigatorAssets -> error "Not handled"
  Exiled (AssetTarget aid) _ | aid `elem` investigatorAssets ->
    pure $ a & (assetsL %~ deleteSet aid) & (slotsL %~ removeFromSlots aid)
  RemoveFromGame (AssetTarget aid) -> pure $ a & assetsL %~ deleteSet aid
  ChooseFightEnemy iid source skillType traits isAction
    | iid == investigatorId -> do
      enemyIds <- map unFightableEnemyId <$> getSetList (iid, source)
      if null traits
        then do
          a <$ push
            (chooseOne
              iid
              [ FightEnemy iid eid source skillType isAction | eid <- enemyIds ]
            )
        else do
          validEnemies <- filterM
            (fmap (notNull . intersection traits) . getSet @Trait)
            enemyIds
          a <$ push
            (chooseOne
              iid
              [ FightEnemy iid eid source skillType isAction
              | eid <- validEnemies
              ]
            )
  ChooseFightEnemyNotEngagedWithInvestigator iid source skillType isAction
    | iid == investigatorId -> do
      enemyIds <- getSet investigatorLocation
      aloofEnemyIds <- mapSet unAloofEnemyId <$> getSet investigatorLocation
      let
        fightableEnemyIds =
          enemyIds
            `difference` (investigatorEngagedEnemies `union` aloofEnemyIds)
      a <$ push
        (chooseOne
          iid
          [ FightEnemy iid eid source skillType isAction
          | eid <- setToList fightableEnemyIds
          ]
        )
  EngageEnemy iid eid True | iid == investigatorId -> a <$ pushAll
    [ TakeAction iid (Just Action.Engage) (ActionCost 1)
    , EngageEnemy iid eid False
    ]
  EngageEnemy iid eid False | iid == investigatorId ->
    pure $ a & engagedEnemiesL %~ insertSet eid
  EngageEnemy iid eid False | iid /= investigatorId ->
    pure $ a & engagedEnemiesL %~ deleteSet eid
  FightEnemy iid eid source skillType True | iid == investigatorId ->
    a <$ pushAll
      [ TakeAction iid (Just Action.Fight) (ActionCost 1)
      , FightEnemy iid eid source skillType False
      ]
  FightEnemy iid eid source skillType False | iid == investigatorId -> do
    pushAll
      [ WhenAttackEnemy iid eid
      , AttackEnemy iid eid source skillType
      , AfterAttackEnemy iid eid
      ]
    pure a
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
  InvestigatorDamageEnemy iid eid | iid == investigatorId -> do
    damage <- damageValueFor 1 a
    a <$ push (EnemyDamage eid iid (InvestigatorSource iid) damage)
  EnemyEvaded iid eid | iid == investigatorId -> do
    push
      (CheckWindow
        iid
        [ Window (Just $ toSource a) (Just $ EnemyTarget eid)
            $ AfterEnemyEvaded You eid
        ]
      )
    pure $ a & engagedEnemiesL %~ deleteSet eid
  AddToVictory (EnemyTarget eid) -> pure $ a & engagedEnemiesL %~ deleteSet eid
  ChooseEvadeEnemy iid source skillType isAction | iid == investigatorId ->
    a <$ push
      (chooseOne
        iid
        [ EvadeEnemy iid eid source skillType isAction
        | eid <- setToList investigatorEngagedEnemies
        ]
      )
  EvadeEnemy iid eid source skillType True | iid == investigatorId ->
    a <$ pushAll
      [ TakeAction iid (Just Action.Evade) (ActionCost 1)
      , EvadeEnemy iid eid source skillType False
      ]
  EvadeEnemy iid eid source skillType False | iid == investigatorId ->
    a <$ pushAll
      [ WhenEvadeEnemy iid eid
      , TryEvadeEnemy iid eid source skillType
      , AfterEvadeEnemy iid eid
      ]
  MoveAction iid lid cost True | iid == investigatorId -> a <$ pushAll
    [ TakeAction iid (Just Action.Move) cost
    , CheckAttackOfOpportunity iid False
    , MoveAction iid lid cost False
    ]
  MoveAction iid lid _cost False | iid == investigatorId ->
    a <$ pushAll (resolve $ Move iid investigatorLocation lid)
  Move iid fromLocationId toLocationId | iid == investigatorId -> a <$ pushAll
    [ Will (MoveFrom iid fromLocationId)
    , Will (MoveTo iid toLocationId)
    , MoveFrom iid fromLocationId
    , MoveTo iid toLocationId
    ]
  Will (FailedSkillTest iid _ _ (InvestigatorTarget iid') _ _)
    | iid == iid' && iid == investigatorId -> a <$ push
      (CheckWindow
        investigatorId
        [Window Nothing (Just $ toTarget a) $ WhenWouldFailSkillTest You]
      )
  InvestigatorDirectDamage iid source damage horror
    | iid == investigatorId && not
      (investigatorDefeated || investigatorResigned)
    -> a <$ pushAll
      ([ CheckWindow
           iid
           [ Window (Just source) (Just $ toTarget a)
               $ WhenWouldTakeDamage source (toTarget a)
           ]
       | damage > 0
       ]
      <> [ CheckWindow
             iid
             [ Window (Just source) (Just $ toTarget a)
                 $ WhenWouldTakeHorror source (toTarget a)
             ]
         | horror > 0
         ]
      <> [InvestigatorDamage iid source damage horror, CheckDefeated source]
      <> [After (InvestigatorTakeDamage iid source damage horror)]
      <> [ CheckWindow
             iid
             [ Window (Just source) (Just $ toTarget a)
                 $ WhenDealtHorror source (toTarget a)
             ]
         | horror > 0
         ]
      )
  InvestigatorAssignDamage iid source strategy damage horror
    | iid == investigatorId && not
      (investigatorDefeated || investigatorResigned)
    -> do
      modifiers <-
        map modifierType <$> getModifiersFor (toSource a) (toTarget a) ()
      if TreatAllDamageAsDirect `elem` modifiers
        then a <$ push (InvestigatorDirectDamage iid source damage horror)
        else a <$ pushAll
          ([ CheckWindow
               iid
               [ Window (Just source) (Just $ toTarget a)
                   $ WhenWouldTakeDamage source (toTarget a)
               ]
           | damage > 0
           ]
          <> [ CheckWindow
                 iid
                 [ Window (Just source) (Just $ toTarget a)
                     $ WhenWouldTakeHorror source (toTarget a)
                 ]
             | horror > 0
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
  InvestigatorDoAssignDamage iid source _ 0 0 damageTargets horrorTargets
    | iid == investigatorId -> a <$ push
      (CheckWindow iid
      $ [ Window (Just source) (Just target) $ WhenDealtDamage source target
        | target <- nub damageTargets
        ]
      <> [ Window (Just source) (Just target) $ WhenDealtHorror source target
         | target <- nub horrorTargets
         ]
      )
  InvestigatorDoAssignDamage iid source strategy health sanity damageTargets horrorTargets
    | iid == investigatorId
    -> do
      healthDamageMessages <- if health > 0
        then do
          healthDamageableAssets <- map unHealthDamageableAssetId
            <$> getSetList iid
          let
            assignRestOfHealthDamage = InvestigatorDoAssignDamage
              investigatorId
              source
              strategy
              (health - 1)
              sanity
            mustAssignDamageToAssets =
              strategy == DamageAssetsFirst && notNull healthDamageableAssets
          pure
            $ [ Run
                  [ InvestigatorDamage investigatorId source 1 0
                  , assignRestOfHealthDamage
                    (InvestigatorTarget investigatorId : damageTargets)
                    horrorTargets
                  ]
              | not mustAssignDamageToAssets
              ]
            <> [ Run
                   [ AssetDamage aid source 1 0
                   , assignRestOfHealthDamage
                     (AssetTarget aid : damageTargets)
                     horrorTargets
                   ]
               | aid <- healthDamageableAssets
               ]
        else pure []
      sanityDamageMessages <- if sanity > 0
        then do
          sanityDamageableAssets <- map unSanityDamageableAssetId
            <$> getSetList iid
          let
            assignRestOfSanityDamage = InvestigatorDoAssignDamage
              investigatorId
              source
              strategy
              health
              (sanity - 1)
            mustAssignDamageToAssets =
              strategy == DamageAssetsFirst && notNull sanityDamageableAssets
          pure
            $ [ Run
                  [ InvestigatorDamage investigatorId source 0 1
                  , assignRestOfSanityDamage
                    damageTargets
                    (InvestigatorTarget investigatorId : horrorTargets)
                  ]
              | not mustAssignDamageToAssets
              ]
            <> [ Run
                   [ AssetDamage aid source 0 1
                   , assignRestOfSanityDamage
                     damageTargets
                     (AssetTarget aid : horrorTargets)
                   ]
               | aid <- sanityDamageableAssets
               ]
        else pure []
      a <$ push (chooseOne iid $ healthDamageMessages <> sanityDamageMessages)
  Investigate iid lid source skillType True | iid == investigatorId -> do
    modifiers <-
      map modifierType <$> getModifiersFor (toSource a) (LocationTarget lid) ()
    let
      investigateCost = foldr applyModifier 1 modifiers
      applyModifier (ActionCostOf (IsAction Action.Investigate) m) n =
        max 0 (n + m)
      applyModifier _ n = n
    a <$ pushAll
      [ TakeAction iid (Just Action.Investigate) (ActionCost investigateCost)
      , CheckAttackOfOpportunity iid False
      , Investigate iid lid source skillType False
      ]
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
    push (After (GainClues iid n))
    pure $ a & cluesL +~ n
  DiscoverClues iid lid n _ | iid == investigatorId ->
    a <$ push (AfterDiscoverClues iid lid n)
  AfterDiscoverClues iid _ n | iid == investigatorId -> do
    push (After (GainClues iid n))
    pure $ a & cluesL +~ n
  InvestigatorDiscardAllClues iid | iid == investigatorId ->
    pure $ a & cluesL .~ 0
  MoveAllCluesTo target | not (isTarget a target) -> do
    when (investigatorClues > 0) (push $ PlaceClues target investigatorClues)
    pure $ a & cluesL .~ 0
  PayCardCost iid cardId | iid == investigatorId -> do
    let
      card =
        fromJustNote "not in hand" $ find ((== cardId) . toCardId) (a ^. handL)
      cost = getCost card
    pure $ a & resourcesL -~ cost
  PayDynamicCardCost iid cardId n beforePlayMessages | iid == investigatorId ->
    do
      let
        resolveMessages =
          beforePlayMessages <> [PayedForDynamicCard iid cardId n False]
      if investigatorResources > n
        then a <$ push
          (chooseOne
            iid
            [ Label
              "Increase spent resources"
              [PayDynamicCardCost iid cardId (n + 1) beforePlayMessages]
            , Label ("Resolve with cost of " <> tshow n) resolveMessages
            ]
          )
        else a <$ pushAll resolveMessages
  PayedForDynamicCard iid cardId n False | iid == investigatorId -> do
    push (PlayDynamicCard iid cardId n Nothing False)
    pure $ a & resourcesL -~ n
  InitiatePlayDynamicCard iid cardId n mtarget asAction
    | iid == investigatorId -> a <$ pushAll
      [ CheckWindow
        iid
        [ Window (Just $ toSource a) (Just $ CardIdTarget cardId)
            $ WhenPlayCard You cardId
        ]
      , PlayDynamicCard iid cardId n mtarget asAction
      ]
  PlayDynamicCard iid cardId _n _mtarget True | iid == investigatorId -> do
    let
      card = fromJustNote "not in hand"
        $ find ((== cardId) . toCardId) investigatorHand
      isFast = case card of
        PlayerCard pc -> cdFast (pcDef pc)
        _ -> False
      maction = case card of
        PlayerCard pc -> cdAction (pcDef pc)
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
            `notElem` cdAttackOfOpportunityModifiers (pcDef pc)
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
  PlayCard iid cardId mtarget windows True | iid == investigatorId -> do
    let
      card = fromJustNote "not in hand"
        $ find ((== cardId) . toCardId) investigatorHand
      isFast = case card of
        PlayerCard pc -> cdFast (pcDef pc)
        _ -> False
      maction = case card of
        PlayerCard pc -> cdAction (pcDef pc)
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
            `notElem` cdAttackOfOpportunityModifiers (pcDef pc)
        _ -> actionProvokesAttackOfOpportunities
      aooMessage =
        [ CheckAttackOfOpportunity iid isFast | provokesAttackOfOpportunities ]
    actionCost <- if isFast
      then pure 0
      else maybe (pure 1) (getActionCost a) maction
    if investigatorRemainingActions
        >= actionCost
        && investigatorResources
        >= getCost card
      then a <$ pushAll
        ([ TakeAction iid (Just Action.Play) (ActionCost actionCost)
         , PayCardCost iid cardId
         ]
        <> aooMessage
        <> [PlayCard iid cardId mtarget window False]
        )
      else pure a
  PlayedCard iid cardId _ _ | iid == investigatorId ->
    pure $ a & handL %~ filter ((/= cardId) . toCardId)
  InvestigatorPlayAsset iid aid slotTypes traits | iid == investigatorId -> do
    let assetsUpdate = assetsL %~ insertSet aid
    if fitsAvailableSlots slotTypes traits a
      then pure $ foldl'
        (\a' slotType ->
          a' & slotsL . ix slotType %~ placeInAvailableSlot aid traits
        )
        (a & assetsUpdate)
        slotTypes
      else do
        let
          missingSlotTypes = slotTypes \\ concatMap
            (\slotType -> availableSlotTypesFor slotType traits a)
            (nub slotTypes)
          assetsThatCanProvideSlots =
            nub $ concatMap (`discardableAssets` a) missingSlotTypes
        a <$ push
          (chooseOne
            iid
            [ Run
                [ Discard (AssetTarget aid')
                , InvestigatorPlayAsset iid aid slotTypes traits
                ]
            | aid' <- assetsThatCanProvideSlots
            ]
          )
  ChangeCardToFast iid cardId | iid == investigatorId -> do
    let
      updateCard card = if toCardId card == cardId
        then case card of
          PlayerCard pc ->
            PlayerCard $ pc { pcDef = (pcDef pc) { cdFast = True } }
          EncounterCard ec -> EncounterCard ec
        else card
    pure $ a & handL %~ map updateCard
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
  InvestigatorWhenDefeated _source iid | iid == investigatorId -> do
    push (InvestigatorDefeated iid)
    pure $ a & defeatedL .~ True & endedTurnL .~ True
  InvestigatorKilled iid | iid == investigatorId -> do
    push (InvestigatorDefeated iid)
    pure $ a & defeatedL .~ True & endedTurnL .~ True
  MoveAllTo lid | not (a ^. defeatedL || a ^. resignedL) ->
    a <$ push (MoveTo investigatorId lid)
  MoveTo iid lid | iid == investigatorId -> do
    connectedLocations <- mapSet unConnectedLocationId <$> getSet lid
    pushAll [WhenEnterLocation iid lid, AfterEnterLocation iid lid]
    pure $ a & locationL .~ lid & connectedLocationsL .~ connectedLocations
  AddedConnection lid1 lid2 | lid1 == investigatorLocation ->
    pure $ a & (connectedLocationsL %~ insertSet lid2)
  AddedConnection lid1 lid2 | lid2 == investigatorLocation ->
    pure $ a & (connectedLocationsL %~ insertSet lid1)
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
  ChooseEndTurn iid | iid == investigatorId -> do
    push
      (CheckWindow iid [Window Nothing (Just $ toTarget a) $ AfterEndTurn You])
    pure $ a & endedTurnL .~ True
  BeginRound -> do
    actionsForTurn <- getActionsForTurn a
    pure
      $ a
      & (endedTurnL .~ False)
      & (remainingActionsL .~ actionsForTurn)
      & (actionsTakenL .~ mempty)
  DiscardTopOfDeck iid n mTarget | iid == investigatorId -> do
    let (cs, deck') = splitAt n (unDeck investigatorDeck)
    pushAll
      $ [ DeckHasNoCards investigatorId mTarget | null deck' ]
      <> [ DiscardedTopOfDeck iid cs target | target <- maybeToList mTarget ]
    pure $ a & deckL .~ Deck deck' & discardL %~ (reverse cs <>)
  DrawCards iid n True | iid == investigatorId -> a <$ pushAll
    [ TakeAction iid (Just Action.Draw) (ActionCost 1)
    , CheckAttackOfOpportunity iid False
    , DrawCards iid n False
    ]
  DrawCards iid 0 False | iid == investigatorId -> pure a
  DrawCards iid n False | iid == investigatorId ->
    if null (unDeck investigatorDeck)
      then if null investigatorDiscard
        then pure a
        else a <$ pushAll [EmptyDeck iid, DrawCards iid n False]
      else do
        let
          (mcard, deck) = drawCard (coerce investigatorDeck)
          handUpdate = maybe id ((:) . PlayerCard) mcard
        case mcard of
          Just card@MkPlayerCard {..} -> do
            when (cdCardType pcDef == PlayerTreacheryType)
              $ push (DrewTreachery iid $ PlayerCard card)
            when (cdCardType pcDef == PlayerEnemyType)
              $ push (DrewPlayerEnemy iid $ PlayerCard card)
            when (cdCardType pcDef /= PlayerTreacheryType && cdWeakness pcDef)
              $ push (Revelation iid (PlayerCardSource $ toCardId card))
          Nothing -> pure ()
        pushAll
          $ [ DeckHasNoCards iid Nothing | null deck ]
          <> [ InvestigatorDrewPlayerCard iid card | card <- maybeToList mcard ]
          <> [DrawCards iid (n - 1) False]
        pure $ a & handL %~ handUpdate & deckL .~ Deck deck
  InvestigatorSpendClues iid n | iid == investigatorId -> pure $ a & cluesL -~ n
  SpendResources iid n | iid == investigatorId ->
    pure $ a & resourcesL %~ max 0 . subtract n
  LoseResources iid n | iid == investigatorId ->
    pure $ a & resourcesL %~ max 0 . subtract n
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
  EmptyDeck iid | iid == investigatorId -> a <$ pushAll
    [ShuffleDiscardBackIn iid, InvestigatorDamage iid EmptyDeckSource 0 1]
  When (EnemySpawn _ lid eid) | lid == investigatorLocation -> do
    traits <- getSetList eid
    a <$ push
      (CheckWindow
        investigatorId
        [ Window Nothing (Just $ EnemyTarget eid)
            $ WhenEnemySpawns YourLocation traits
        ]
      )
  UseAbility iid ability@Ability {..} | iid == investigatorId -> a <$ push
    (CreatePayAbilityCostEffect (Just ability) abilitySource (toTarget a))
  AllDrawCardAndResource | not (a ^. defeatedL || a ^. resignedL) -> do
    unlessM (hasModifier a CannotDrawCards)
      $ push (DrawCards investigatorId 1 False)
    mayChooseNotToTakeResources <-
      elem MayChooseNotToTakeUpkeepResources
      . map modifierType
      <$> getModifiersFor (toSource a) (InvestigatorTarget investigatorId) ()
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
      if cdWeakness (pcDef card) then card { pcBearer = Just iid } else card
    pure $ a & deckL .~ Deck shuffled
  InvestigatorCommittedCard iid cardId | iid == investigatorId ->
    pure $ a & handL %~ filter ((/= cardId) . toCardId)
  BeforeSkillTest iid skillType skillDifficulty | iid == investigatorId -> do
    committedCardIds <- map unCommittedCardId <$> getSetList iid
    committedCardCodes <- mapSet unCommittedCardCode <$> getSet ()
    actions <- getActions iid (WhenSkillTest skillType) ()
    isScenarioAbility <- getIsScenarioAbility
    source <- fromJustNote "damage outside skill test" <$> getSkillTestSource
    cannotCommitCards <-
      elem CannotCommitCards
      . map modifierType
      <$> getModifiersFor source (InvestigatorTarget investigatorId) ()
    let
      triggerMessage = StartSkillTest investigatorId
      beginMessage = BeforeSkillTest iid skillType skillDifficulty
      committableCards = if cannotCommitCards
        then []
        else flip filter investigatorHand $ \case
          PlayerCard MkPlayerCard {..} ->
            let
              passesRestrictions = flip
                all
                (cdCommitRestrictions pcDef)
                \case
                  MaxOnePerTest ->
                    cdCardCode pcDef `notElem` committedCardCodes
                  OnlyYourTest -> True
                  ScenarioAbility -> isScenarioAbility
                  MinSkillTestValueDifference n ->
                    (skillDifficulty - baseSkillValueFor skillType Nothing [] a)
                      >= n
            in
              pcId
              `notElem` committedCardIds
              && (SkillWild
                 `elem` cdSkills pcDef
                 || skillType
                 `elem` cdSkills pcDef
                 )
              && passesRestrictions
          _ -> False
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
          <> map (\action -> Run [action, beginMessage]) actions
          <> [triggerMessage]
          )
        )
      else push (SkillTestAsk $ chooseOne iid [triggerMessage])
    pure a
  BeforeSkillTest iid skillType skillDifficulty | iid /= investigatorId -> do
    locationId <- getId iid
    isScenarioAbility <- getIsScenarioAbility
    when (locationId == investigatorLocation) $ do
      committedCardIds <- map unCommittedCardId <$> getSetList investigatorId
      committedCardCodes <- mapSet unCommittedCardCode <$> getSet ()
      let
        beginMessage = BeforeSkillTest iid skillType skillDifficulty
        committableCards = if notNull committedCardIds
          then []
          else flip
            filter
            investigatorHand
            \case
              PlayerCard MkPlayerCard {..} ->
                let
                  passesRestrictions = flip
                    all
                    (cdCommitRestrictions pcDef)
                    \case
                      MaxOnePerTest ->
                        cdCardCode pcDef `notElem` committedCardCodes
                      OnlyYourTest -> False
                      ScenarioAbility -> isScenarioAbility
                      MinSkillTestValueDifference n ->
                        (skillDifficulty
                          - baseSkillValueFor skillType Nothing [] a
                          )
                          >= n
                in
                  pcId
                  `notElem` committedCardIds
                  && (SkillWild
                     `elem` cdSkills pcDef
                     || skillType
                     `elem` cdSkills pcDef
                     )
                  && passesRestrictions
              _ -> False
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
  CheckWindow iid windows | iid == investigatorId -> do
    actions <- fmap concat <$> for windows $ \window ->
      getActions iid (windowType window) ()
    playableCards <- getPlayableCards a (map windowType windows)
    if notNull playableCards || notNull actions
      then if any isForced actions
        then
          a <$ push
            (chooseOne iid $ map (Run . (: [CheckWindow iid windows])) actions)
        else a <$ push
          (chooseOne iid
          $ [ Run
                [ PayCardCost iid (toCardId c)
                , PlayCard iid (toCardId c) Nothing windows False
                , CheckWindow iid windows
                ]
            | c <- playableCards
            ]
          <> map (Run . (: [CheckWindow iid windows])) actions
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
    ([ CreatePayAbilityCostEffect Nothing (toSource a) (toTarget a)
     , PayAbilityCost (toSource a) iid mAction cost
     , PayAbilityCostFinished (toSource a) iid
     ]
    <> [ TakenAction iid action | action <- maybeToList mAction ]
    )
  TakenAction iid action | iid == investigatorId ->
    pure $ a & actionsTakenL %~ (<> [action])
  PutOnTopOfDeck iid card | iid == investigatorId ->
    pure $ a & deckL %~ Deck . (card :) . unDeck
  AddToHand iid card | iid == investigatorId -> do
    case card of
      PlayerCard card' ->
        when (cdRevelation (pcDef card'))
          $ if toCardType card' == PlayerTreacheryType
              then push (DrewTreachery iid card)
              else push (Revelation iid (PlayerCardSource $ toCardId card'))
      _ -> pure ()
    pure $ a & handL %~ (card :)
  ShuffleCardsIntoDeck iid cards | iid == investigatorId -> do
    deck <- shuffleM (cards <> unDeck investigatorDeck)
    pure $ a & deckL .~ Deck deck
  AddToHandFromDeck iid cardId | iid == investigatorId -> do
    let
      card = fromJustNote "card did not exist"
        $ find ((== cardId) . toCardId) (unDeck investigatorDeck)
    deck <- shuffleM $ filter ((/= cardId) . toCardId) (unDeck investigatorDeck)
    case card of
      MkPlayerCard {..} -> do
        when (cdCardType pcDef == PlayerTreacheryType)
          $ push (DrewTreachery iid $ PlayerCard card)
        when (cdCardType pcDef == PlayerEnemyType)
          $ push (DrewPlayerEnemy iid $ PlayerCard card)
    pure $ a & deckL .~ Deck deck & handL %~ (PlayerCard card :)
  DisengageEnemy iid eid | iid == investigatorId ->
    pure $ a & engagedEnemiesL %~ deleteSet eid
  SearchDeckForTraits iid (InvestigatorTarget iid') traits
    | iid' == investigatorId -> runMessage
      (SearchTopOfDeck
        iid
        (InvestigatorSource iid)
        (InvestigatorTarget iid')
        (length $ unDeck investigatorDeck)
        traits
        (ShuffleBackIn $ DrawFound iid)
      )
      a
  SearchTopOfDeck iid source (InvestigatorTarget iid') n traits strategy
    | iid' == investigatorId -> do
      let
        (cards, deck) = splitAt n $ unDeck investigatorDeck
        traits' = setFromList traits
      push $ EndSearch iid source
      case strategy of
        PutBackInAnyOrder -> push
          (chooseOneAtATime iid
          $ [ AddFocusedToTopOfDeck
                iid
                (InvestigatorTarget iid')
                (toCardId card)
            | card <- cards
            ]
          )
        ShuffleBackIn (DrawFound who) -> do
          let
            choices =
              [ Run
                  [ AddFocusedToHand
                    iid
                    (InvestigatorTarget who)
                    (toCardId card)
                  , ShuffleAllFocusedIntoDeck iid (InvestigatorTarget iid')
                  ]
              | card <- cards
              , null traits' || notNull (traits' `intersection` toTraits card)
              ]
          push
            (chooseOne iid $ if null choices
              then
                [ Label
                    "No cards found"
                    [ShuffleAllFocusedIntoDeck iid (InvestigatorTarget iid')]
                ]
              else choices
            )
        ShuffleBackIn (NotifyTargetOfFound target) -> do
          let
            choices =
              [ Run
                  [ SearchTopOfDeckFound iid target (PlayerCard card)
                  , ShuffleAllFocusedIntoDeck iid (InvestigatorTarget iid')
                  ]
              | card <- cards
              , null traits' || notNull (traits' `intersection` toTraits card)
              ]
          push
            (chooseOne iid $ if null choices
              then
                [ Label
                    "No cards found"
                    [ SearchTopOfDeckNoneFound iid target
                    , ShuffleAllFocusedIntoDeck iid (InvestigatorTarget iid')
                    ]
                ]
              else choices
            )
      actions <- fmap concat <$> for cards $ \card' -> getActions
        iid
        (WhenAmongSearchedCards You)
        (toCardInstance iid $ PlayerCard card')
      -- TODO: This is for astounding revelation and only one research action is possible
      -- so we are able to short circuit here, but we may have additional cards in the
      -- future so we may want to make this more versatile
      unless (null actions) $ push
        (chooseOne iid
        $ actions
        <> [Continue "Skip playing fast cards or using reactions"]
        )
      push (FocusCards $ map PlayerCard cards)
      pure $ a & deckL .~ Deck deck
  SearchDiscard iid (InvestigatorTarget iid') traits | iid' == investigatorId ->
    do
      let traits' = setFromList traits
      push
        (chooseOne
          iid
          [ Run
              [ AddFocusedToHand iid (InvestigatorTarget iid') (toCardId card)
              , RemoveFromDiscard iid (toCardId card)
              , UnfocusCards
              ]
          | card <- investigatorDiscard
          , null traits' || traits' `intersection` toTraits card == traits'
          ]
        )
      a <$ push (FocusCards $ map PlayerCard investigatorDiscard)
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
  RemoveDiscardFromGame iid | iid == investigatorId -> pure $ a & discardL .~ []
  After (FailedSkillTest iid mAction source (InvestigatorTarget iid') _ n)
    | iid == iid' && iid == investigatorId -> do
      let
        windows = maybe
          []
          (\case
            Action.Investigate ->
              [ Window (Just source) (Just $ toTarget a)
                  $ AfterFailInvestigationSkillTest You n
              ]
            _ -> []
          )
          mAction
      a <$ push
        (CheckWindow
          iid
          (Window (Just source) (Just $ toTarget a) (AfterFailSkillTest You n)
          : windows
          )
        )
  After (PassedSkillTest iid mAction source (InvestigatorTarget iid') _ n)
    | iid == iid' && iid == investigatorId -> a <$ push
      (CheckWindow
        iid
        [ Window (Just source) (Just $ toTarget a)
            $ AfterPassSkillTest mAction source You n
        ]
      )
  PlayerWindow iid additionalActions | iid == investigatorId -> do
    actions <- getActions iid NonFast ()
    fastActions <- getActions iid (DuringTurn You) ()
    playerWindowActions <- getActions iid FastPlayerWindow ()
    modifiers <-
      map modifierType
        <$> getModifiersFor (InvestigatorSource iid) (InvestigatorTarget iid) ()
    canAffordTakeResources <- getCanAfford a Action.Resource
    canAffordDrawCards <- getCanAfford a Action.Draw
    canAffordPlayCard <- getCanAfford a Action.Play
    isPlayableMap :: HashMap Card Bool <- mapFromList <$> for
      investigatorHand
      (\c -> do
        isPlayable <- getIsPlayable a [DuringTurn You] c
        pure (c, isPlayable)
      )
    let isPlayable c = findWithDefault False c isPlayableMap
    fastIsPlayableMap :: HashMap Card Bool <- mapFromList <$> for
      investigatorHand
      (\c -> do
        fastIsPlayable <- getFastIsPlayable a [DuringTurn You] c
        pure (c, fastIsPlayable)
      )
    let fastIsPlayable c = findWithDefault False c fastIsPlayableMap
    a <$ push
      (chooseOne
        iid
        (additionalActions
        <> [ TakeResources iid 1 True
           | canAffordTakeResources && CannotGainResources `notElem` modifiers
           ]
        <> [ DrawCards iid 1 True
           | canAffordDrawCards
             && CannotTakeAction (IsAction Action.Draw)
             `notElem` modifiers
             && CannotDrawCards
             `notElem` modifiers
           ]
        <> [ InitiatePlayCard iid (toCardId c) Nothing True
           | c <- investigatorHand
           , canAffordPlayCard || fastIsPlayable c
           , isPlayable c && not (isDynamic c)
           ]
        <> [ InitiatePlayDynamicCard iid (toCardId c) 0 Nothing True
           | c <- investigatorHand
           , canAffordPlayCard || fastIsPlayable c
           , isPlayable c && isDynamic c
           ]
        <> [ChooseEndTurn iid]
        <> actions
        <> fastActions
        <> playerWindowActions
        )
      )
  _ -> pure a
