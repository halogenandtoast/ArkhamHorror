module Arkham.Types.Investigator.Attrs
  ( module Arkham.Types.Investigator.Attrs
  , module X
  ) where

import Arkham.Prelude

import Arkham.Types.ClassSymbol as X
import Arkham.Types.Classes as X hiding (discard)
import Arkham.Types.Investigator.Runner as X
import Arkham.Types.Name as X
import Arkham.Types.Stats as X
import Arkham.Types.Token as X
import Arkham.Types.Trait as X hiding (Cultist)

import qualified Data.Text as T
import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Action (Action)
import qualified Arkham.Types.Action as Action
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard
import Arkham.Types.CommitRestriction
import Arkham.Types.Cost
import Arkham.Types.Deck
import Arkham.Types.EntityInstance
import Arkham.Types.Game.Helpers hiding (windows)
import qualified Arkham.Types.Game.Helpers as Helpers
import Arkham.Types.Helpers
import Arkham.Types.Id
import Arkham.Types.DamageEffect
import Arkham.Types.Matcher
  ( AssetMatcher(..)
  , CardMatcher(..)
  , EnemyMatcher(..)
  , InvestigatorMatcher(..)
  , LocationMatcher(..)
  , assetIs
  , pattern AloofEnemy
  )
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillTest
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window (Window(..))
import qualified Arkham.Types.Window as Window
import qualified Data.HashSet as HashSet
import Data.UUID (nil)

class IsInvestigator a

type InvestigatorRunner env
  = (InnerInvestigatorRunner env, EntityInstanceRunner env)

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
  -- investigator-specific fields
  , investigatorTomeActions :: Maybe Int
  }
  deriving stock (Show, Generic)

startsWithL :: Lens' InvestigatorAttrs [CardDef]
startsWithL =
  lens investigatorStartsWith $ \m x -> m { investigatorStartsWith = x }

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

inHandTreacheriesL :: Lens' InvestigatorAttrs (HashSet TreacheryId)
inHandTreacheriesL =
  lens investigatorInHandTreacheries $ \m x -> m { investigatorInHandTreacheries = x }

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

cardsUnderneathL :: Lens' InvestigatorAttrs [Card]
cardsUnderneathL = lens investigatorCardsUnderneath
  $ \m x -> m { investigatorCardsUnderneath = x }

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

instance ToGameLoggerFormat InvestigatorAttrs where
  format attrs = "{investigator:\"" <> T.replace "\"" "\\\"" (display $ toName attrs) <> "\":" <> tshow (toId attrs) <> "}"

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
  applyModifier (ActionSkillModifier action skillType m) n | canBeIncreased || m < 0 =
    if skillType == skill && Just action == maction then max 0 (n + m) else n
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
  modifiers <- getModifiers source (InvestigatorTarget $ investigatorId attrs)
  pure $ foldr applyModifier 8 modifiers
 where
  applyModifier (HandSize m) n = max 0 (n + m)
  applyModifier _ n = n

getInHandCount :: (MonadReader env m, HasModifiersFor env ()) => InvestigatorAttrs -> m Int
getInHandCount attrs = do
  let cards = investigatorHand attrs
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
  , investigatorTraits = setFromList traits
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

getFastIsPlayable
  :: (HasCallStack, MonadReader env m, CanCheckPlayable env)
  => InvestigatorAttrs
  -> [Window]
  -> Card
  -> m Bool
getFastIsPlayable _ _ (EncounterCard _) = pure False -- TODO: there might be some playable ones?
getFastIsPlayable attrs windows c@(PlayerCard _) = do
  modifiers <- getModifiers (toSource attrs) (toTarget attrs)
  isPlayable <- getIsPlayable (toId attrs) (toSource attrs) windows c
  pure $ (canBecomeFast modifiers || isJust (cdFastWindow pcDef)) && isPlayable
 where
  pcDef = toCardDef c
  canBecomeFast modifiers = foldr applyModifier False modifiers
  applyModifier (CanBecomeFast (mcardType, traits)) _
    | maybe True (== cdCardType pcDef) mcardType
      && notNull (setFromList traits `intersect` toTraits pcDef)
    = True
  applyModifier _ val = val

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
  -> [Window]
  -> m [Card]
getPlayableCards a@InvestigatorAttrs {..} windows = do
  playableDiscards <- getPlayableDiscards a windows
  playableHandCards <- filterM (getFastIsPlayable a windows) investigatorHand
  pure $ playableHandCards <> playableDiscards

getPlayableDiscards
  :: (MonadReader env m, CanCheckPlayable env)
  => InvestigatorAttrs
  -> [Window]
  -> m [Card]
getPlayableDiscards attrs@InvestigatorAttrs {..} windows = do
  modifiers <- getModifiers (toSource attrs) (toTarget attrs)
  filterM
    (getIsPlayable (toId attrs) (toSource attrs) windows)
    (possibleCards modifiers)
 where
  possibleCards modifiers = map (PlayerCard . snd) $ filter
    (canPlayFromDiscard modifiers)
    (zip @_ @Int [0 ..] investigatorDiscard)
  canPlayFromDiscard modifiers (n, card) =
    any (allowsPlayFromDiscard n card) modifiers
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

instance (EntityInstanceRunner env, InvestigatorRunner env) => RunMessage env InvestigatorAttrs where
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
hasModifier InvestigatorAttrs { investigatorId } m = elem m <$> getModifiers
  (InvestigatorSource investigatorId)
  (InvestigatorTarget investigatorId)

findCard :: CardId -> InvestigatorAttrs -> Card
findCard cardId a =
  fromJustNote "not in hand or discard" $ findMatch $ (a ^. handL) <> map
    PlayerCard
    (a ^. discardL)
  where findMatch = find ((== cardId) . toCardId)

getAsIfInHandCards
  :: (MonadReader env m, HasModifiersFor env ())
  => InvestigatorAttrs
  -> m [Card]
getAsIfInHandCards attrs = do
  modifiers <- getModifiers (toSource attrs) (toTarget attrs)
  let
    modifiersPermitPlay c = any (modifierPermitsPlay c) modifiers
    modifierPermitsPlay (c, depth) = \case
      CanPlayTopOfDiscard (mType, traits) | depth == 0 ->
        maybe True (== toCardType c) mType
          && (null traits || notNull
               (setFromList traits `intersection` toTraits c)
             )
      _ -> False
  pure $ map (PlayerCard . fst) $ filter
    modifiersPermitPlay
    (zip (investigatorDiscard attrs) [0 :: Int ..])

runInvestigatorMessage
  :: (InvestigatorRunner env, MonadReader env m, MonadRandom m, MonadIO m, HasGameLogger env)
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
    windows <- checkWindows
      [Window Timing.After (Window.DrawingStartingHand iid)]
    pushAll $ ShuffleDiscardBackIn iid : windows
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
    windowMsgs <- checkWindows
      ((`Window` Window.InvestigatorDefeated source iid)
      <$> [Timing.When, Timing.After]
      )
    a <$ pushAll (windowMsgs <> [InvestigatorWhenEliminated (toSource a) iid])
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
  TakeControlOfAsset iid aid | iid == investigatorId -> do
    slots <- getList aid
    traits <- getSetList aid
    a <$ push (InvestigatorPlayAsset iid aid slots traits)
  TakeControlOfAsset iid aid | iid /= investigatorId ->
    pure $ a & (assetsL %~ deleteSet aid) & (slotsL %~ removeFromSlots aid)
  ChooseAndDiscardAsset iid assetMatcher | iid == investigatorId -> do
    discardableAssetIds <- selectList
      (assetMatcher <> DiscardableAsset <> AssetOwnedBy You)
    a <$ push (chooseOne iid $ map (Discard . AssetTarget) discardableAssetIds)
  AttachAsset aid _ | aid `member` investigatorAssets ->
    pure $ a & (assetsL %~ deleteSet aid) & (slotsL %~ removeFromSlots aid)
  AttachTreachery tid (InvestigatorTarget iid) | iid == investigatorId ->
    pure $ a & treacheriesL %~ insertSet tid
  AllCheckHandSize | not (a ^. defeatedL || a ^. resignedL) -> do
    handSize <- getHandSize a
    inHandCount <- getInHandCount a
    when (inHandCount > handSize)
      $ push (CheckHandSize investigatorId)
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
  AddTreacheryToHand iid tid | iid == investigatorId ->
    pure $ a & inHandTreacheriesL %~ insertSet tid
  Discard (TreacheryTarget tid) -> pure $ a & treacheriesL %~ deleteSet tid & inHandTreacheriesL %~ deleteSet tid
  Discarded (EnemyTarget eid) _ -> pure $ a & engagedEnemiesL %~ deleteSet eid
  PlaceEnemyInVoid eid -> pure $ a & engagedEnemiesL %~ deleteSet eid
  Discarded (AssetTarget aid) (PlayerCard card)
    | aid `elem` investigatorAssets
    -> pure
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
  ChooseFightEnemy iid source mTarget skillType traits isAction
    | iid == investigatorId -> do
      enemyIds <- map unFightableEnemyId <$> getSetList (iid, source)
      if null traits
        then do
          a <$ push
            (chooseOne
              iid
              [ FightEnemy iid eid source mTarget skillType isAction | eid <- enemyIds ]
            )
        else do
          validEnemies <- filterM
            (fmap (notNull . intersection traits) . getSet @Trait)
            enemyIds
          a <$ push
            (chooseOne
              iid
              [ FightEnemy iid eid source mTarget skillType isAction
              | eid <- validEnemies
              ]
            )
  ChooseFightEnemyNotEngagedWithInvestigator iid source mTarget skillType isAction
    | iid == investigatorId -> do
      enemyIds <- select $ EnemyAt $ LocationWithId investigatorLocation
      aloofEnemyIds <- select $ AloofEnemy <> EnemyAt (LocationWithId investigatorLocation)
      let
        fightableEnemyIds =
          enemyIds
            `difference` (investigatorEngagedEnemies `union` aloofEnemyIds)
      a <$ push
        (chooseOne
          iid
          [ FightEnemy iid eid source mTarget skillType isAction
          | eid <- setToList fightableEnemyIds
          ]
        )
  EngageEnemy iid eid True | iid == investigatorId -> do
    modifiers' <- getModifiers (toSource a) (toTarget a)
    a <$ pushAll
      ([ TakeAction iid (Just Action.Engage) (ActionCost 1) ]
      <> [ CheckAttackOfOpportunity iid False | ActionDoesNotCauseAttacksOfOpportunity Action.Engage `notElem` modifiers']
      <> [EngageEnemy iid eid False
      ])
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
  FightEnemy iid eid source mTarget skillType False | iid == investigatorId -> do
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
    modifiers' <- getModifiers (InvestigatorSource iid) (EnemyTarget eid)
    pushAll =<< checkWindows [Window Timing.After (Window.EnemyEvaded iid eid)]
    pure $ if AlternateSuccessfullEvasion `elem` modifiers'
      then a
      else a & engagedEnemiesL %~ deleteSet eid
  AddToVictory (EnemyTarget eid) -> pure $ a & engagedEnemiesL %~ deleteSet eid
  -- TODO: WARNING: HERE BE DRAGONS
  ChooseEvadeEnemy iid source skillType isAction | iid == investigatorId ->
    a <$ push
      (chooseOne
        iid
        [ EvadeLabel
            eid
            [ ChosenEvadeEnemy source eid
            , EvadeEnemy iid eid source skillType isAction
            ]
        | eid <- setToList investigatorEngagedEnemies
        ]
      )
  EvadeEnemy iid eid source skillType True | iid == investigatorId -> do
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
      , EvadeEnemy iid eid source skillType False
      ]
  EvadeEnemy iid eid source skillType False | iid == investigatorId ->
    a <$ pushAll
      [TryEvadeEnemy iid eid source skillType, AfterEvadeEnemy iid eid]
  MoveAction iid lid cost True | iid == investigatorId -> a <$ pushAll
    [TakeAction iid (Just Action.Move) cost, MoveAction iid lid cost False]
  MoveAction iid lid _cost False | iid == investigatorId -> do
    afterWindowMsgs <- Helpers.checkWindows [Window Timing.After $ Window.MoveAction iid investigatorLocation lid]
    a <$ pushAll (resolve (Move (toSource a) iid investigatorLocation lid) <> afterWindowMsgs)
  Move source iid fromLocationId destinationLocationId | iid == investigatorId -> do
    windowMsgs <- Helpers.windows [Window.Moves iid fromLocationId destinationLocationId]
    a <$ pushAll
      ([ Will (MoveFrom source iid fromLocationId)
      , Will (MoveTo source iid destinationLocationId)
      , MoveFrom source iid fromLocationId
      , MoveTo source iid destinationLocationId
      ] <> windowMsgs)
  Will (FailedSkillTest iid _ _ (InvestigatorTarget iid') _ _)
    | iid == iid' && iid == investigatorId -> do
      windows <- checkWindows
        [Window Timing.When (Window.WouldFailSkillTest iid)]
      a <$ pushAll windows
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
           iid
           [Window Timing.When (Window.WouldTakeDamage source (toTarget a))]
       | damage > 0
       ]
      <> [ CheckWindow
             iid
             [Window Timing.When (Window.WouldTakeHorror source (toTarget a))]
         | horror > 0
         ]
      <> [ CheckWindow
             iid
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
             iid
             [Window Timing.When (Window.DealtHorror source (toTarget a))]
         | horror > 0
         ]
      <> [ CheckWindow
             iid
             [Window Timing.When (Window.DealtDamage source NonAttackDamageEffect (toTarget a))]
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
               iid
               [Window Timing.When (Window.WouldTakeDamage source (toTarget a))]
           | damage > 0
           ]
          <> [ CheckWindow
                 iid
                 [ Window
                     Timing.When
                     (Window.WouldTakeHorror source (toTarget a))
                 ]
             | horror > 0
             ]
          <> [ CheckWindow
                 iid
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
      pushAll =<< checkWindows
        (concatMap (\t -> map (Window t) windows) [Timing.When, Timing.After])
      pure a
  InvestigatorDoAssignDamage iid source _ 0 0 damageTargets horrorTargets
    | iid == investigatorId -> a <$ push
      (CheckWindow iid
      $ [ Window Timing.When (Window.DealtDamage source NonAttackDamageEffect target)
        | target <- nub damageTargets
        ]
      <> [ Window Timing.When (Window.DealtHorror source target)
         | target <- nub horrorTargets
         ]
      )
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
                <$> select (AssetOwnedBy You <> assetIs def)
              pure
                $ [ damageInvestigator | null validAssets ]
                <> map damageAsset validAssets
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
              mustBeDamagedFirstBeforeInvestigator <- selectList (AssetCanBeAssignedHorrorBy iid <> AssetWithModifier NonDirectHorrorMustBeAssignToThisFirst)
              pure $ [damageInvestigator | null mustBeDamagedFirstBeforeInvestigator] <> map damageAsset sanityDamageableAssets
            DamageFirst def -> do
              validAssets <-
                setToList
                . intersection (setFromList sanityDamageableAssets)
                <$> select (AssetOwnedBy You <> assetIs def)
              pure
                $ [ damageInvestigator | null validAssets ]
                <> map damageAsset validAssets
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
        ([ TakeAction iid (Just Action.Investigate) (ActionCost investigateCost)]
        <> [ CheckAttackOfOpportunity iid False | ActionDoesNotCauseAttacksOfOpportunity Action.Investigate `notElem` modifiers']
        <> [ Investigate iid lid source mTarget skillType False
        ])
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
    windows <- checkWindows
      ((`Window` Window.GainsClues iid n) <$> [Timing.When, Timing.After])
    a <$ pushAll (windows <> [PlaceClues (InvestigatorTarget iid) n, After (GainClues iid n)])
  PlaceClues (InvestigatorTarget iid) n | iid == investigatorId -> do
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
    let card = findCard cardId a
    cost <- getModifiedCardCost iid card
    iids <- filter (/= iid) <$> getInvestigatorIds
    iidsWithModifiers <- for iids $ \iid' -> do
      modifiers <- getModifiers (InvestigatorSource iid') (InvestigatorTarget iid')
      pure (iid', modifiers)
    canHelpPay <- flip filterM iidsWithModifiers $ \(_, modifiers) -> do
      flip anyM modifiers $ \case
        CanSpendResourcesOnCardFromInvestigator iMatcher cMatcher ->
          liftA2 (&&) (member iid <$> select iMatcher) (pure $ cardMatch card cMatcher)
        _ -> pure False
    if null canHelpPay
       then pure $ a & resourcesL -~ cost
       else do
         iidsWithResources <- traverse (traverseToSnd (fmap unResourceCount . getCount)) (iid : map fst canHelpPay)
         a <$ push (Ask iid $ ChoosePaymentAmounts ("Pay " <> tshow cost <> " resources") (Just cost) $ map (\(iid', resources) -> (iid', (0, resources), SpendResources iid' 1)) iidsWithResources)
  PayDynamicCardCost iid cardId _n beforePlayMessages | iid == investigatorId ->
    a <$ push (Ask iid $ ChooseDynamicCardAmounts iid cardId (0, investigatorResources) False beforePlayMessages)
  PayedForDynamicCard iid cardId n False | iid == investigatorId -> do
    push (PlayDynamicCard iid cardId n Nothing False)
    pure $ a & resourcesL -~ n
  InitiatePlayDynamicCard iid cardId n mtarget asAction
    | iid == investigatorId -> do
      let card = findCard cardId a
      a <$ pushAll
        [ CheckWindow iid [Window Timing.When (Window.PlayCard iid card)]
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
      [ CheckWindow iid [Window Timing.When (Window.PlayCard iid card)]
      , if isFastEvent card
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
        && (investigatorResources + additionalResources
        >= getCost card)
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
    pushAll =<< checkWindows [Window Timing.After (Window.PlayCard iid card)]
    pure $ a & handL %~ filter (/= card) & discardL %~ filter
      ((/= card) . PlayerCard)
  InvestigatorPlayAsset iid aid slotTypes traits | iid == investigatorId -> do
    a <$ if fitsAvailableSlots slotTypes traits a
      then push (InvestigatorPlayedAsset iid aid slotTypes traits)
      else do
        let
          missingSlotTypes = slotTypes \\ concatMap
            (\slotType -> availableSlotTypesFor slotType traits a)
            (nub slotTypes)
          assetsThatCanProvideSlots =
            nub $ concatMap (`discardableAssets` a) missingSlotTypes
        push
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
    pushAll
      $ movedByWindows
      <> [ WhenWillEnterLocation iid lid
         , WhenEnterLocation iid lid
         , AfterEnterLocation iid lid
         ]
    pure $ a & locationL .~ lid
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
  BeginInvestigation -> do
    actionsForTurn <- getAbilitiesForTurn a
    pure
      $ a
      & (endedTurnL .~ False)
      & (remainingActionsL .~ actionsForTurn)
      & (actionsTakenL .~ mempty)
  DiscardTopOfDeck iid n mTarget | iid == investigatorId -> do
    let (cs, deck') = splitAt n (unDeck investigatorDeck)
    windowMsgs <- if null deck'
      then checkWindows
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
                (toCardType card /= PlayerTreacheryType && isJust (cdCardSubType
                  $toCardDef card)
                )
              $ push (Revelation iid (PlayerCardSource $ toCardId card))
          Nothing -> pure ()

        windowMsgs <- if null deck
          then checkWindows
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
    windowMsgs <- checkWindows
      [ Window
          Timing.After
          (Window.DrawCard iid (PlayerCard card) $ InvestigatorDeck iid)
      ]
    a <$ pushAll windowMsgs
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
    shuffled <-
      shuffleM
      $ flip map (unDeck deck)
      $ \card -> if isJust (cdCardSubType $ toCardDef card)
          then card { pcBearer = Just iid }
          else card
    pure $ a & deckL .~ Deck shuffled
  InvestigatorCommittedCard iid card | iid == investigatorId -> do
    commitedCardWindows <- Helpers.windows [Window.CommittedCard iid card]
    pushAll $ FocusCards [card] : commitedCardWindows <> [UnfocusCards]
    pure $ a & handL %~ filter (/= card)
  ReturnToHand iid (AssetTarget aid) | iid == investigatorId ->
    pure $ a & assetsL %~ deleteSet aid & slotsL %~ removeFromSlots aid
  PlaceUnderneath target cards | isTarget a target ->
    pure $ a & cardsUnderneathL <>~ cards
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
                MaxOnePerTest -> pure $ toCardCode card `notElem` committedCardCodes
                OnlyYourTest -> pure True
                OnlyIfYourLocationHasClues -> pure $ clueCount > 0
                ScenarioAbility -> pure isScenarioAbility
                SelfCanCommitWhen matcher -> notNull <$> select (You <> matcher)
                MinSkillTestValueDifference n ->
                  pure $ (skillDifficulty - baseSkillValueFor skillType Nothing [] a)
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
          passesCommitRestrictions <-
              allM
              passesCommitRestriction
              (cdCommitRestrictions $ toCardDef card)
          pure $ toCardId card
            `notElem` committedCardIds
            && (SkillWild
               `elem` cdSkills (toCardDef card)
               || skillType
               `elem` cdSkills (toCardDef card)
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
      let
        beginMessage = BeforeSkillTest iid skillType skillDifficulty
      committableCards <- if notNull committedCardIds
          then pure []
          else flip
            filterM
            investigatorHand
            \case
              PlayerCard card -> do
                let
                  passesCommitRestriction = \case
                      MaxOnePerTest -> pure $ toCardCode card `notElem` committedCardCodes
                      OnlyYourTest -> pure False
                      OnlyIfYourLocationHasClues -> pure $ clueCount > 0
                      ScenarioAbility -> pure isScenarioAbility
                      SelfCanCommitWhen matcher -> notNull <$> select (You <> matcher)
                      MinSkillTestValueDifference n ->
                        pure $ (skillDifficulty - baseSkillValueFor skillType Nothing [] a)
                          >= n
                  prevented = flip
                    any
                    modifiers'
                    \case
                      CanOnlyUseCardsInRole role ->
                        cdClassSymbol (toCardDef card)
                          `notElem` [Just Neutral, Just role, Nothing]
                      _ -> False
                passesCriterias <-
                  allM
                  passesCommitRestriction
                  (cdCommitRestrictions $ toCardDef card)
                pure $ toCardId card
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
  CheckWindow iid windows | iid == investigatorId -> do
    a <$ push (RunWindow iid windows)
  RunWindow iid windows | iid == investigatorId -> do
    actions <- nub . concat <$> traverse (getActions iid) windows
    playableCards <- getPlayableCards a windows
    if notNull playableCards || notNull actions
      then if any isForcedAbility actions
        then do
          -- Silent forced abilities should trigger automatically
          let (silent, normal) = partition isSilentForcedAbility actions
              toUseAbilities = map (($ windows) . UseAbility iid)
          a <$ pushAll (toUseAbilities silent <> [
            chooseOne iid $ map
              (Run . (: [RunWindow iid windows])) (toUseAbilities normal) | notNull normal]
            )
        else do
          actionsWithMatchingWindows <- for actions $ \ability ->
            (ability, )
              <$> filterM
                    (\w -> windowMatches
                      iid
                      (abilitySource ability)
                      w
                      (abilityWindow ability)
                    )
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
    pure $ a & deckL %~ Deck . (card :) . unDeck
  AddToHand iid card | iid == investigatorId -> do
    case card of
      PlayerCard card' ->
        when (cdRevelation (toCardDef card'))
          $ if toCardType card' == PlayerTreacheryType
              then push (DrewTreachery iid card)
              else push (Revelation iid (PlayerCardSource $ toCardId card'))
      _ -> pure ()
    pure
      $ a
      & (handL %~ (card :))
      & (cardsUnderneathL %~ filter (/= card))
      & (discardL %~ filter ((/= card) . PlayerCard))
  ShuffleCardsIntoDeck iid cards | iid == investigatorId -> do
    deck <- shuffleM (cards <> unDeck investigatorDeck)
    pure $ a & deckL .~ Deck deck
  PlaceOnBottomOfDeck iid card | iid == investigatorId ->
    pure $ a & deckL %~ Deck . (<> [card]) . unDeck
  AddToHandFromDeck iid cardId | iid == investigatorId -> do
    let
      card = fromJustNote "card did not exist"
        $ find ((== cardId) . toCardId) (unDeck investigatorDeck)
    deck <- shuffleM $ filter ((/= cardId) . toCardId) (unDeck investigatorDeck)
    when (toCardType card == PlayerTreacheryType)
      $ push (DrewTreachery iid $ PlayerCard card)
    when (toCardType card == PlayerEnemyType)
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
        DeferAllSearchedToTarget target -> push
          ( SearchTopOfDeckAll
              iid
              target
              (InvestigatorDeck iid)
              (map PlayerCard cards))
        DeferSearchedToTarget target -> pushAll
          [ SearchTopOfDeckFound
              iid
              target
              (InvestigatorDeck iid)
              (PlayerCard card)
          | card <- cards
          ]
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
                  [ SearchTopOfDeckFound
                    iid
                    target
                    (InvestigatorDeck iid)
                    (PlayerCard card)
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
      let window = Window Timing.When (Window.AmongSearchedCards iid)
      actions <- fmap concat <$> for cards $ \card' -> filterM
        (windowMatches iid source window . abilityWindow)
        (getAbilities (toCardInstance iid $ PlayerCard card'))
      -- TODO: This is for astounding revelation and only one research action is possible
      -- so we are able to short circuit here, but we may have additional cards in the
      -- future so we may want to make this more versatile
      unless (null actions) $ push
        (chooseOne iid
        $ map (($ [window]) . UseAbility iid) actions
        <> [Continue "Skip playing fast cards or using reactions!!!"]
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
              _ -> error "expected location source for investigate"
            _ -> []
          )
          mAction
      windowMsgs <- checkWindows
        (Window Timing.After (Window.FailSkillTest iid n) : windows)
      a <$ pushAll windowMsgs
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
      msgs <- checkWindows
        (Window Timing.After (Window.PassSkillTest mAction source iid n)
        : windows
        )
      a <$ pushAll msgs
  PlayerWindow iid additionalActions isAdditional | iid == investigatorId -> do
    let
      windows =
        [ Window Timing.When (Window.DuringTurn iid)
        , Window Timing.When Window.FastPlayerWindow
        ]
    actions <- getActions iid (Window Timing.When Window.NonFast)
    if any isForcedAbility actions
      then a
        <$ push (chooseOne iid $ map (($ windows) . UseAbility iid) actions)
      else do
        fastActions <- getActions
          iid
          (Window Timing.When (Window.DuringTurn iid))
        playerWindowActions <- getActions
          iid
          (Window Timing.When Window.FastPlayerWindow)
        modifiers <- getModifiers
          (InvestigatorSource iid)
          (InvestigatorTarget iid)
        canAffordTakeResources <- getCanAfford a Action.Resource
        canAffordDrawCards <- getCanAfford a Action.Draw
        canAffordPlayCard <- getCanAfford a Action.Play
        asIfInHandCards <- getAsIfInHandCards a
        let handCards = investigatorHand <> asIfInHandCards
        isPlayableMap :: HashMap Card Bool <- mapFromList <$> for
          handCards
          (\c -> do
            isPlayable <- getIsPlayable (toId a) (toSource a) windows c
            pure (c, isPlayable)
          )
        let isPlayable c = findWithDefault False c isPlayableMap
        fastIsPlayableMap :: HashMap Card Bool <- mapFromList <$> for
          handCards
          (\c -> do
            fastIsPlayable <- getFastIsPlayable a windows c
            pure (c, fastIsPlayable)
          )
        let
          fastIsPlayable c = findWithDefault False c fastIsPlayableMap
          usesAction = not isAdditional
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
               ]
            <> [ InitiatePlayCard iid (toCardId c) Nothing usesAction
               | c <- handCards
               , canAffordPlayCard || fastIsPlayable c
               , isPlayable c && not (isDynamic c)
               ]
            <> [ InitiatePlayDynamicCard iid (toCardId c) 0 Nothing usesAction
               | c <- handCards
               , canAffordPlayCard || fastIsPlayable c
               , isPlayable c && isDynamic c
               ]
            <> [ChooseEndTurn iid]
            <> map
                 (($ windows) . UseAbility iid)
                 (nub $ actions <> fastActions <> playerWindowActions)
            )
          )
  PlayerWindow iid additionalActions isAdditional | iid /= investigatorId -> do
    actions <- getActions iid (Window Timing.When Window.NonFast)
    if any isForcedAbility actions
      then pure a -- handled by active player
      else do
        fastActions <- getActions
          investigatorId
          (Window Timing.When (Window.DuringTurn iid))
        playerWindowActions <- getActions
          investigatorId
          (Window Timing.When Window.FastPlayerWindow)
        asIfInHandCards <- getAsIfInHandCards a
        let
          handCards = investigatorHand <> asIfInHandCards
          windows =
            [ Window Timing.When (Window.DuringTurn iid)
            , Window Timing.When Window.FastPlayerWindow
            ]
        fastIsPlayableMap :: HashMap Card Bool <- mapFromList <$> for
          handCards
          (\c -> do
            fastIsPlayable <- getFastIsPlayable a windows c
            pure (c, fastIsPlayable)
          )
        let
          fastIsPlayable c = findWithDefault False c fastIsPlayableMap
          usesAction = not isAdditional
          choices =
            additionalActions
              <> [ InitiatePlayCard
                     investigatorId
                     (toCardId c)
                     Nothing
                     usesAction
                 | c <- handCards
                 , fastIsPlayable c && not (isDynamic c)
                 ]
              <> [ InitiatePlayDynamicCard
                     investigatorId
                     (toCardId c)
                     0
                     Nothing
                     usesAction
                 | c <- handCards
                 , fastIsPlayable c && isDynamic c
                 ]
              <> map
                   (($ windows) . UseAbility investigatorId)
                   (nub $ fastActions <> playerWindowActions)
        a <$ unless
          (null choices)
          (push $ AskPlayer $ chooseOne investigatorId choices)
  Blanked msg' -> runMessage msg' a
  _ -> pure a
