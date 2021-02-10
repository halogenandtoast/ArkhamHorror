{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Arkham.Types.Investigator.Attrs where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.EnemyId
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.TreacheryId
import Arkham.Types.Window
import Arkham.Types.Action (Action)
import qualified Arkham.Types.Action as Action
import Arkham.Types.CommitRestriction
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait
import Arkham.Types.EntityInstance
import Control.Monad.Fail
import qualified Data.HashSet as HashSet

data InvestigatorAttrs = InvestigatorAttrs
  { investigatorName :: Text
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

makeLensesWith suffixedFields ''InvestigatorAttrs

instance ToJSON InvestigatorAttrs where
  toJSON = genericToJSON $ aesonOptions $ Just "investigator"
  toEncoding = genericToEncoding $ aesonOptions $ Just "investigator"

instance FromJSON InvestigatorAttrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "investigator"

instance IsCard InvestigatorAttrs where
  getCardId = error "Investigators are not treated as cards yet"
  getCardCode = unInvestigatorId . investigatorId
  getTraits = investigatorTraits
  getKeywords = mempty

instance HasCount ActionRemainingCount env InvestigatorAttrs where
  getCount = pure . ActionRemainingCount . investigatorRemainingActions

instance HasList Action.TakenAction env InvestigatorAttrs where
  getList = pure . map Action.TakenAction . investigatorActionsTaken

instance Entity InvestigatorAttrs where
  type EntityId InvestigatorAttrs = InvestigatorId
  type EntityAttrs InvestigatorAttrs = InvestigatorAttrs
  toId = investigatorId
  toAttrs = id

instance NamedEntity InvestigatorAttrs where
  toName = mkName . investigatorName

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
  :: (MonadReader env m, HasModifiersFor env (), HasSource ForSkillTest env)
  => Int
  -> InvestigatorAttrs
  -> m Int
damageValueFor baseValue attrs = do
  source <-
    asks $ fromJustNote "damage outside skill test" . getSource ForSkillTest
  modifiers <-
    map modifierType
      <$> getModifiersFor source (InvestigatorTarget $ investigatorId attrs) ()
  pure $ foldr applyModifier baseValue modifiers
 where
  applyModifier (DamageDealt m) n = max 0 (n + m)
  applyModifier _ n = n

getIsScenarioAbility
  :: ( HasSource ForSkillTest env
     , MonadReader env m
     , CanBeWeakness env TreacheryId
     )
  => m Bool
getIsScenarioAbility = do
  source <-
    asks $ fromJustNote "damage outside skill test" . getSource ForSkillTest
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
  :: (MonadReader env m, HasModifiersFor env (), HasSource ForSkillTest env)
  => InvestigatorAttrs
  -> m Int
getHandSize attrs = do
  source <- asks $ fromMaybe (toSource attrs) . getSource ForSkillTest
  modifiers <-
    map modifierType
      <$> getModifiersFor source (InvestigatorTarget $ investigatorId attrs) ()
  pure $ foldr applyModifier 8 modifiers
 where
  applyModifier (HandSize m) n = max 0 (n + m)
  applyModifier _ n = n

getActionsForTurn
  :: (MonadReader env m, HasModifiersFor env ()) => InvestigatorAttrs -> m Int
getActionsForTurn attrs@InvestigatorAttrs {..} = do
  modifiers <-
    map modifierType <$> getModifiersFor (toSource attrs) (toTarget attrs) ()
  pure $ foldr applyModifier 3 modifiers
 where
  applyModifier (AdditionalActions m) n = max 0 (n + m)
  applyModifier _ n = n

getCanDiscoverClues
  :: (MonadReader env m, HasModifiersFor env ()) => InvestigatorAttrs -> m Bool
getCanDiscoverClues attrs@InvestigatorAttrs {..} = do
  modifiers <-
    map modifierType <$> getModifiersFor (toSource attrs) (toTarget attrs) ()
  pure $ not (any match modifiers)
 where
  match CannotDiscoverClues{} = True
  match _ = False

getCanSpendClues
  :: (MonadReader env m, HasModifiersFor env ()) => InvestigatorAttrs -> m Bool
getCanSpendClues attrs@InvestigatorAttrs {..} = do
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
  -> Text
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
  , investigatorLocation = "00000"
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
  :: (MonadReader env m, HasModifiersFor env (), HasSource ForSkillTest env)
  => InvestigatorAttrs
  -> Int
  -> m Int
cluesToDiscover attrs startValue = do
  source <-
    asks $ fromJustNote "damage outside skill test" . getSource ForSkillTest
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
  :: (MonadReader env m, HasModifiersFor env ())
  => InvestigatorAttrs
  -> [Window]
  -> Card
  -> m Bool
getFastIsPlayable _ _ (EncounterCard _) = pure False -- TODO: there might be some playable ones?
getFastIsPlayable attrs windows c@(PlayerCard MkPlayerCard {..}) = do
  modifiers <-
    map modifierType <$> getModifiersFor (toSource attrs) (toTarget attrs) ()
  isPlayable <- getIsPlayable attrs windows c
  pure $ (pcFast || canBecomeFast modifiers) && isPlayable
 where
  canBecomeFast modifiers = foldr applyModifier False modifiers
  applyModifier (CanBecomeFast (mcardType, traits)) _
    | maybe True (== pcCardType) mcardType
      && not (null (setFromList traits `intersect` pcTraits))
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
  startingCost = case pcCost of
    StaticCost n -> n
    DynamicCost -> 0
  applyModifier (ReduceCostOf traits m) n
    | not (null (setFromList traits `intersection` pcTraits)) = max 0 (n - m)
  applyModifier (ReduceCostOfCardType cardType m) n | cardType == pcCardType =
    max 0 (n - m)
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
    | not (null (setFromList traits `intersection` ecTraits)) = max 0 (n - m)
  applyModifier _ n = n

getIsPlayable
  :: (MonadReader env m, HasModifiersFor env ())
  => InvestigatorAttrs
  -> [Window]
  -> Card
  -> m Bool
getIsPlayable _ _ (EncounterCard _) = pure False -- TODO: there might be some playable ones?
getIsPlayable attrs@InvestigatorAttrs {..} windows c@(PlayerCard MkPlayerCard {..})
  = do
    modifiers <-
      map modifierType <$> getModifiersFor (toSource attrs) (toTarget attrs) ()
    modifiedCardCost <- getModifiedCardCost attrs c
    pure
      $ (pcCardType /= SkillType)
      && (modifiedCardCost <= investigatorResources)
      && none prevents modifiers
      && (not pcFast || (pcFast && cardInWindows windows c attrs))
      && (pcAction /= Just Action.Evade || not (null investigatorEngagedEnemies)
         )
 where
  none f = not . any f
  prevents (CannotPlay typePairs) = any
    (\(cType, traits) ->
      pcCardType
        == cType
        && (null traits || not (null (intersection pcTraits traits)))
    )
    typePairs
  prevents _ = False

drawOpeningHand
  :: InvestigatorAttrs -> Int -> ([PlayerCard], [Card], [PlayerCard])
drawOpeningHand a n = go n (a ^. discardL, a ^. handL, coerce (a ^. deckL))
 where
  go 0 (d, h, cs) = (d, h, cs)
  go _ (_, _, []) =
    error "this should never happen, it means the deck was empty during drawing"
  go m (d, h, c : cs) = if pcWeakness c
    then go m (c : d, h, cs)
    else go (m - 1) (d, PlayerCard c : h, cs)

cardInWindows :: [Window] -> Card -> InvestigatorAttrs -> Bool
cardInWindows windows c _ = case c of
  PlayerCard pc -> not . null $ pcWindows pc `intersect` setFromList windows
  _ -> False

getPlayableCards
  :: (MonadReader env m, HasModifiersFor env ())
  => InvestigatorAttrs
  -> [Window]
  -> m [Card]
getPlayableCards a@InvestigatorAttrs {..} windows = do
  playableDiscards <- getPlayableDiscards a windows
  playableHandCards <- filterM (getFastIsPlayable a windows) investigatorHand
  pure $ playableHandCards <> playableDiscards

getPlayableDiscards
  :: (MonadReader env m, HasModifiersFor env ())
  => InvestigatorAttrs
  -> [Window]
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
    = maybe True (== pcCardType) mcardType
      && (null traits || (setFromList traits `HashSet.isSubsetOf` pcTraits))
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

instance ActionRunner env => HasActions env InvestigatorAttrs where
  getActions iid window attrs | iid == investigatorId attrs = concat <$> for
    (attrs ^.. handL . traverse . _PlayerCard)
    (getActions iid (InHandWindow iid window) . toCardInstance iid . PlayerCard)
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
isForced (ActivateCardAbilityAction _ Ability { abilityType }) =
  abilityType == ForcedAbility
isForced _ = False

runInvestigatorMessage
  :: ( InvestigatorRunner env
     , MonadReader env m
     , MonadRandom m
     , MonadIO m
     , MonadFail m
     )
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
      (permanentCards, deck') = partition pcPermanent (unDeck investigatorDeck)
      (discard, hand, deck) = drawOpeningHand (a & deckL .~ Deck deck') 5
    unshiftMessages
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
  InvestigatorMulligan iid | iid == investigatorId -> if null investigatorHand
    then a <$ unshiftMessage (FinishedWithMulligan investigatorId)
    else a <$ unshiftMessage
      (chooseOne iid
      $ Run
          [Continue "Done With Mulligan", FinishedWithMulligan investigatorId]
      : [ Run [DiscardCard iid (getCardId card), InvestigatorMulligan iid]
        | card <- investigatorHand
        ]
      )
  BeginTrade iid (AssetTarget aid) iids | iid == investigatorId ->
    a <$ unshiftMessage
      (chooseOne
        iid
        [ TargetLabel (InvestigatorTarget iid') [TakeControlOfAsset iid' aid]
        | iid' <- iids
        ]
      )
  BeginTrade iid ResourceTarget iids | iid == investigatorId ->
    a <$ unshiftMessage
      (chooseOne
        iid
        [ TargetLabel
            (InvestigatorTarget iid')
            [TakeResources iid' 1 False, SpendResources iid 1]
        | iid' <- iids
        ]
      )
  AllRandomDiscard | not (a ^. defeatedL || a ^. resignedL) ->
    a <$ unshiftMessage (RandomDiscard investigatorId)
  RandomDiscard iid | iid == investigatorId -> do
    n <- getRandomR (0, length investigatorHand - 1)
    case investigatorHand !!? n of
      Nothing -> pure a
      Just c -> a <$ unshiftMessage (DiscardCard investigatorId (getCardId c))
  FinishedWithMulligan iid | iid == investigatorId -> do
    let (discard, hand, deck) = drawOpeningHand a (5 - length investigatorHand)
    unshiftMessage (ShuffleDiscardBackIn iid)
    pure
      $ a
      & (resourcesL .~ 5)
      & (discardL .~ discard)
      & (handL .~ hand)
      & (deckL .~ Deck deck)
  ShuffleDiscardBackIn iid | iid == investigatorId ->
    if not (null investigatorDiscard)
      then do
        deck <- shuffleM (investigatorDiscard <> coerce investigatorDeck)
        pure $ a & discardL .~ [] & deckL .~ Deck deck
      else pure a
  Resign iid | iid == investigatorId -> do
    unshiftMessages $ resolve $ InvestigatorResigned iid
    pure $ a & resignedL .~ True
  InvestigatorDefeated iid | iid == investigatorId ->
    a <$ unshiftMessage (InvestigatorWhenEliminated iid)
  InvestigatorResigned iid | iid == investigatorId ->
    a <$ unshiftMessage (InvestigatorWhenEliminated iid)
  -- InvestigatorWhenEliminated is handled by the scenario
  InvestigatorEliminated iid | iid == investigatorId -> do
    unshiftMessage
      (PlaceClues (LocationTarget investigatorLocation) investigatorClues)
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
    a <$ unshiftMessage
      (chooseOne iid $ map (Discard . AssetTarget) discardableAssetIds)
  AttachAsset aid _ | aid `member` investigatorAssets ->
    pure $ a & assetsL %~ deleteSet aid
  AttachTreachery tid (InvestigatorTarget iid) | iid == investigatorId ->
    pure $ a & treacheriesL %~ insertSet tid
  AllCheckHandSize | not (a ^. defeatedL || a ^. resignedL) -> do
    handSize <- getHandSize a
    when (length investigatorHand > handSize)
      $ unshiftMessage (CheckHandSize investigatorId)
    pure a
  CheckHandSize iid | iid == investigatorId -> do
    handSize <- getHandSize a
    when (length investigatorHand > handSize) $ unshiftMessage
      (chooseOne
        iid
        [ Run [DiscardCard iid (getCardId card), CheckHandSize iid]
        | card <- filter (not . pcWeakness)
          $ mapMaybe (preview _PlayerCard) investigatorHand
        ]
      )
    pure a
  AddToDiscard iid pc | iid == investigatorId -> pure $ a & discardL %~ (pc :)
  ChooseAndDiscardCard iid | iid == investigatorId -> a <$ unshiftMessage
    (chooseOne iid
    $ [ DiscardCard iid (getCardId card) | card <- discardableCards a ]
    )
  DiscardCard iid cardId | iid == investigatorId -> do
    let
      card = fromJustNote "must be in hand"
        $ find ((== cardId) . getCardId) investigatorHand
    case card of
      PlayerCard pc ->
        pure
          $ a
          & handL
          %~ filter ((/= cardId) . getCardId)
          & discardL
          %~ (pc :)
      EncounterCard _ -> pure $ a & handL %~ filter ((/= cardId) . getCardId) -- TODO: This should discard to the encounter discard
  RemoveCardFromHand iid cardCode | iid == investigatorId ->
    pure $ a & handL %~ filter ((/= cardCode) . getCardCode)
  ShuffleIntoDeck iid (TreacheryTarget tid) | iid == investigatorId ->
    pure $ a & treacheriesL %~ deleteSet tid
  ShuffleIntoDeck iid (AssetTarget aid) | iid == investigatorId -> do
    card <- fromJustNote "missing card" <$> getPlayerCard aid
    deck' <- shuffleM (card : unDeck investigatorDeck)
    unshiftMessage $ After msg
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
  Discarded (AssetTarget aid) card | aid `elem` investigatorAssets ->
    pure
      $ a
      & (assetsL %~ deleteSet aid)
      & (discardL %~ (lookupPlayerCard (getCardCode card) (unAssetId aid) :))
      & (slotsL %~ removeFromSlots aid)
  RemoveFromGame (AssetTarget aid) -> pure $ a & assetsL %~ deleteSet aid
  ChooseFightEnemy iid source skillType isAction | iid == investigatorId -> do
    enemyIds <- getSet investigatorLocation
    aloofEnemyIds <- mapSet unAloofEnemyId <$> getSet investigatorLocation
    let
      fightableEnemyIds =
        investigatorEngagedEnemies `union` (enemyIds `difference` aloofEnemyIds)
    a <$ unshiftMessage
      (chooseOne
        iid
        [ FightEnemy iid eid source skillType isAction
        | eid <- setToList fightableEnemyIds
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
      a <$ unshiftMessage
        (chooseOne
          iid
          [ FightEnemy iid eid source skillType isAction
          | eid <- setToList fightableEnemyIds
          ]
        )
  EngageEnemy iid eid True | iid == investigatorId -> a <$ unshiftMessages
    [ TakeAction iid (Just Action.Engage) (ActionCost 1)
    , EngageEnemy iid eid False
    ]
  EngageEnemy iid eid False | iid == investigatorId ->
    pure $ a & engagedEnemiesL %~ insertSet eid
  EngageEnemy iid eid False | iid /= investigatorId ->
    pure $ a & engagedEnemiesL %~ deleteSet eid
  FightEnemy iid eid source skillType True | iid == investigatorId ->
    a <$ unshiftMessages
      [ TakeAction iid (Just Action.Fight) (ActionCost 1)
      , FightEnemy iid eid source skillType False
      ]
  FightEnemy iid eid source skillType False | iid == investigatorId -> do
    unshiftMessages
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
        [x] | x /= iid -> unshiftMessage (InvestigatorDamageInvestigator iid x)
        _ -> pure ()
    pure a
  InvestigatorDamageInvestigator iid xid | iid == investigatorId -> do
    damage <- damageValueFor 1 a
    a <$ unshiftMessage
      (InvestigatorAssignDamage xid (InvestigatorSource iid) DamageAny damage 0)
  InvestigatorDamageEnemy iid eid | iid == investigatorId -> do
    damage <- damageValueFor 1 a
    a <$ unshiftMessage (EnemyDamage eid iid (InvestigatorSource iid) damage)
  EnemyEvaded iid eid | iid == investigatorId -> do
    unshiftMessage (CheckWindow iid [AfterEnemyEvaded You eid])
    pure $ a & engagedEnemiesL %~ deleteSet eid
  AddToVictory (EnemyTarget eid) -> pure $ a & engagedEnemiesL %~ deleteSet eid
  ChooseEvadeEnemy iid source skillType isAction | iid == investigatorId ->
    a <$ unshiftMessage
      (chooseOne
        iid
        [ EvadeEnemy iid eid source skillType isAction
        | eid <- setToList investigatorEngagedEnemies
        ]
      )
  EvadeEnemy iid eid source skillType True | iid == investigatorId ->
    a <$ unshiftMessages
      [ TakeAction iid (Just Action.Evade) (ActionCost 1)
      , EvadeEnemy iid eid source skillType False
      ]
  EvadeEnemy iid eid source skillType False | iid == investigatorId ->
    a <$ unshiftMessages
      [ WhenEvadeEnemy iid eid
      , TryEvadeEnemy iid eid source skillType
      , AfterEvadeEnemy iid eid
      ]
  MoveAction iid lid True | iid == investigatorId -> a <$ unshiftMessages
    [ TakeAction iid (Just Action.Move) (ActionCost 1)
    , CheckAttackOfOpportunity iid False
    , MoveAction iid lid False
    ]
  MoveAction iid lid False | iid == investigatorId ->
    a <$ unshiftMessages (resolve $ Move iid investigatorLocation lid)
  Move iid fromLocationId toLocationId | iid == investigatorId ->
    a <$ unshiftMessages
      [ Will (MoveTo iid toLocationId)
      , MoveFrom iid fromLocationId
      , MoveTo iid toLocationId
      ]
  Will (FailedSkillTest iid _ _ (InvestigatorTarget iid') _ _)
    | iid == iid' && iid == investigatorId
    -> a <$ unshiftMessage
      (CheckWindow investigatorId [WhenWouldFailSkillTest You])
  InvestigatorDirectDamage iid source damage horror
    | iid == investigatorId && not
      (investigatorDefeated || investigatorResigned)
    -> a <$ unshiftMessages
      ([ CheckWindow iid [WhenWouldTakeDamage source (toTarget a)]
       | damage > 0
       ]
      <> [ CheckWindow iid [WhenWouldTakeHorror source (toTarget a)]
         | horror > 0
         ]
      <> [InvestigatorDamage iid source damage horror, CheckDefeated]
      <> [After (InvestigatorTakeDamage iid source damage horror)]
      <> [ CheckWindow iid [WhenDealtHorror source (toTarget a)]
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
        then
          a <$ unshiftMessage
            (InvestigatorDirectDamage iid source damage horror)
        else a <$ unshiftMessages
          ([ CheckWindow iid [WhenWouldTakeDamage source (toTarget a)]
           | damage > 0
           ]
          <> [ CheckWindow iid [WhenWouldTakeHorror source (toTarget a)]
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
             , CheckDefeated
             ]
          <> [After (InvestigatorTakeDamage iid source damage horror)]
          )
  InvestigatorDoAssignDamage iid source _ 0 0 damageTargets horrorTargets
    | iid == investigatorId -> a <$ unshiftMessage
      (CheckWindow iid
      $ [ WhenDealtDamage source target | target <- nub damageTargets ]
      <> [ WhenDealtHorror source target | target <- nub horrorTargets ]
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
              strategy == DamageAssetsFirst && not (null healthDamageableAssets)
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
              strategy == DamageAssetsFirst && not (null sanityDamageableAssets)
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
      a <$ unshiftMessage
        (chooseOne iid $ healthDamageMessages <> sanityDamageMessages)
  Investigate iid lid source skillType True | iid == investigatorId -> do
    modifiers <-
      map modifierType <$> getModifiersFor (toSource a) (LocationTarget lid) ()
    let
      investigateCost = foldr applyModifier 1 modifiers
      applyModifier (ActionCostOf (IsAction Action.Investigate) m) n =
        max 0 (n + m)
      applyModifier _ n = n
    a <$ unshiftMessages
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
        a <$ unshiftMessage
          (DiscoverCluesAtLocation iid lid modifiedCluesToDiscover maction)
      else pure a
  GainClues iid n | iid == investigatorId -> do
    unshiftMessage (After (GainClues iid n))
    pure $ a & cluesL +~ n
  DiscoverClues iid lid n _ | iid == investigatorId ->
    a <$ unshiftMessage (AfterDiscoverClues iid lid n)
  AfterDiscoverClues iid _ n | iid == investigatorId -> do
    unshiftMessage (After (GainClues iid n))
    pure $ a & cluesL +~ n
  InvestigatorDiscardAllClues iid | iid == investigatorId ->
    pure $ a & cluesL .~ 0
  MoveAllCluesTo target | not (isTarget a target) -> do
    when
      (investigatorClues > 0)
      (unshiftMessage $ PlaceClues target investigatorClues)
    pure $ a & cluesL .~ 0
  PayCardCost iid cardId | iid == investigatorId -> do
    let
      card =
        fromJustNote "not in hand" $ find ((== cardId) . getCardId) (a ^. handL)
      cost = getCost card
    pure $ a & resourcesL -~ cost
  PayDynamicCardCost iid cardId n beforePlayMessages | iid == investigatorId ->
    do
      let
        resolveMessages =
          beforePlayMessages <> [PayedForDynamicCard iid cardId n False]
      if investigatorResources > n
        then a <$ unshiftMessage
          (chooseOne
            iid
            [ Label
              "Increase spent resources"
              [PayDynamicCardCost iid cardId (n + 1) beforePlayMessages]
            , Label ("Resolve with cost of " <> tshow n) resolveMessages
            ]
          )
        else a <$ unshiftMessages resolveMessages
  PayedForDynamicCard iid cardId n False | iid == investigatorId -> do
    unshiftMessage (PlayDynamicCard iid cardId n Nothing False)
    pure $ a & resourcesL -~ n
  InitiatePlayDynamicCard iid cardId n mtarget asAction
    | iid == investigatorId -> a <$ unshiftMessages
      [ CheckWindow iid [WhenPlayCard You cardId]
      , PlayDynamicCard iid cardId n mtarget asAction
      ]
  PlayDynamicCard iid cardId _n _mtarget True | iid == investigatorId -> do
    let
      card = fromJustNote "not in hand"
        $ find ((== cardId) . getCardId) investigatorHand
      isFast = case card of
        PlayerCard pc -> pcFast pc
        _ -> False
      maction = case card of
        PlayerCard pc -> pcAction pc
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
            `notElem` pcAttackOfOpportunityModifiers pc
        _ -> actionProvokesAttackOfOpportunities
      aooMessage = if provokesAttackOfOpportunities
        then [CheckAttackOfOpportunity iid isFast]
        else []
    actionCost <- if isFast
      then pure 0
      else maybe (pure 1) (getActionCost a) maction
    a <$ unshiftMessages
      [ TakeAction iid (Just Action.Play) (ActionCost actionCost)
      , PayDynamicCardCost iid cardId 0 aooMessage
      ]
  InitiatePlayCard iid cardId mtarget asAction | iid == investigatorId ->
    a <$ unshiftMessages
      [ CheckWindow iid [WhenPlayCard You cardId]
      , PlayCard iid cardId mtarget asAction
      ]
  PlayCard iid cardId mtarget True | iid == investigatorId -> do
    let
      card = fromJustNote "not in hand"
        $ find ((== cardId) . getCardId) investigatorHand
      isFast = case card of
        PlayerCard pc -> pcFast pc
        _ -> False
      maction = case card of
        PlayerCard pc -> pcAction pc
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
            `notElem` pcAttackOfOpportunityModifiers pc
        _ -> actionProvokesAttackOfOpportunities
      aooMessage = if provokesAttackOfOpportunities
        then [CheckAttackOfOpportunity iid isFast]
        else []
    actionCost <- if isFast
      then pure 0
      else maybe (pure 1) (getActionCost a) maction
    if investigatorRemainingActions
        >= actionCost
        && investigatorResources
        >= getCost card
      then a <$ unshiftMessages
        ([ TakeAction iid (Just Action.Play) (ActionCost actionCost)
         , PayCardCost iid cardId
         ]
        <> aooMessage
        <> [PlayCard iid cardId mtarget False]
        )
      else pure a
  PlayedCard iid cardId | iid == investigatorId ->
    pure $ a & handL %~ filter ((/= cardId) . getCardId)
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
        a <$ unshiftMessage
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
      updateCard card = if getCardId card == cardId
        then case card of
          PlayerCard pc -> PlayerCard $ pc { pcFast = True }
          EncounterCard ec -> EncounterCard ec
        else card
    pure $ a & handL %~ map updateCard
  RemoveAllCopiesOfCardFromGame iid cardCode | iid == investigatorId -> do
    for_ investigatorAssets $ \assetId -> do
      cardCode' <- getId @CardCode assetId
      when
        (cardCode == cardCode')
        (unshiftMessage $ RemoveFromGame (AssetTarget assetId))
    pure
      $ a
      & (deckL %~ Deck . filter ((/= cardCode) . getCardCode) . unDeck)
      & (discardL %~ filter ((/= cardCode) . getCardCode))
      & (handL %~ filter ((/= cardCode) . getCardCode))
  InvestigatorDamage iid _ health sanity | iid == investigatorId ->
    pure $ a & healthDamageL +~ health & sanityDamageL +~ sanity
  CheckDefeated -> do
    facingDefeat <- getFacingDefeat a
    if facingDefeat
      then do
        modifiedHealth <- getModifiedHealth a
        modifiedSanity <- getModifiedSanity a
        unshiftMessage (InvestigatorWhenDefeated investigatorId)
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
  InvestigatorWhenDefeated iid | iid == investigatorId -> do
    unshiftMessage (InvestigatorDefeated iid)
    pure $ a & defeatedL .~ True & endedTurnL .~ True
  InvestigatorKilled iid | iid == investigatorId -> do
    unshiftMessage (InvestigatorDefeated iid)
    pure $ a & defeatedL .~ True & endedTurnL .~ True
  MoveAllTo lid | not (a ^. defeatedL || a ^. resignedL) ->
    a <$ unshiftMessage (MoveTo investigatorId lid)
  MoveTo iid lid | iid == investigatorId -> do
    connectedLocations <- asks $ mapSet unConnectedLocationId . getSet lid
    unshiftMessages [WhenEnterLocation iid lid, AfterEnterLocation iid lid]
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
    unshiftMessage (RefillSlots iid slotType assetIds)
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
        unshiftMessage
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
    unshiftMessage (CheckWindow iid [AfterEndTurn You])
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
    unshiftMessages
      $ [ DeckHasNoCards investigatorId | null deck' ]
      <> [ DiscardedTopOfDeck iid cs target | target <- maybeToList mTarget ]
    pure $ a & deckL .~ Deck deck' & discardL %~ (reverse cs <>)
  DrawCards iid n True | iid == investigatorId -> a <$ unshiftMessages
    [ TakeAction iid (Just Action.Draw) (ActionCost 1)
    , CheckAttackOfOpportunity iid False
    , DrawCards iid n False
    ]
  DrawCards iid 0 False | iid == investigatorId -> pure a
  DrawCards iid n False | iid == investigatorId ->
    if null (unDeck investigatorDeck)
      then if null investigatorDiscard
        then pure a
        else a <$ unshiftMessages [EmptyDeck iid, DrawCards iid n False]
      else do
        let
          (mcard, deck) = drawCard (coerce investigatorDeck)
          handUpdate = maybe id ((:) . PlayerCard) mcard
        case mcard of
          Just card@MkPlayerCard {..} -> do
            when (pcCardType == PlayerTreacheryType)
              $ unshiftMessage (DrewTreachery iid $ PlayerCard card)
            when (pcCardType == PlayerEnemyType)
              $ unshiftMessage (DrewPlayerEnemy iid $ PlayerCard card)
            when (pcCardType /= PlayerTreacheryType && pcWeakness)
              $ unshiftMessage
                  (Revelation iid (PlayerCardSource $ getCardId card))
          Nothing -> pure ()
        unshiftMessages
          $ [ DeckHasNoCards iid | null deck ]
          <> [ InvestigatorDrewPlayerCard iid card | card <- maybeToList mcard ]
          <> [DrawCards iid (n - 1) False]
        pure $ a & handL %~ handUpdate & deckL .~ Deck deck
  InvestigatorSpendClues iid n | iid == investigatorId -> pure $ a & cluesL -~ n
  SpendResources iid n | iid == investigatorId ->
    pure $ a & resourcesL %~ max 0 . subtract n
  TakeResources iid n True | iid == investigatorId -> do
    unlessM (hasModifier a CannotGainResources) $ unshiftMessages
      [ TakeAction iid (Just Action.Resource) (ActionCost 1)
      , CheckAttackOfOpportunity iid False
      , TakeResources iid n False
      ]
    pure a
  TakeResources iid n False | iid == investigatorId -> do
    cannotGainResources <- hasModifier a CannotGainResources
    pure $ if cannotGainResources then a else a & resourcesL +~ n
  EmptyDeck iid | iid == investigatorId -> a <$ unshiftMessages
    [ShuffleDiscardBackIn iid, InvestigatorDamage iid EmptyDeckSource 0 1]
  When (EnemySpawn _ lid eid) | lid == investigatorLocation -> do
    traits <- getSetList eid
    a <$ unshiftMessage
      (CheckWindow investigatorId [WhenEnemySpawns YourLocation traits])
  ActivateCardAbilityAction iid ability@Ability {..} | iid == investigatorId ->
    a <$ unshiftMessage
      (CreatePayAbilityCostEffect (Just ability) abilitySource (toTarget a))
  AllDrawCardAndResource | not (a ^. defeatedL || a ^. resignedL) -> do
    unlessM (hasModifier a CannotDrawCards)
      $ unshiftMessage (DrawCards investigatorId 1 False)
    pure $ a & resourcesL +~ 1
  LoadDeck iid deck | iid == investigatorId -> do
    shuffled <- shuffleM $ flip map deck $ \card -> if pcWeakness card
      then card { pcBearer = Just $ BearerId $ unInvestigatorId iid }
      else card
    pure $ a & deckL .~ Deck shuffled
  InvestigatorCommittedCard iid cardId | iid == investigatorId ->
    pure $ a & handL %~ filter ((/= cardId) . getCardId)
  BeforeSkillTest iid skillType | iid == investigatorId -> do
    committedCardIds <- map unCommittedCardId <$> getSetList iid
    committedCardCodes <- mapSet unCommittedCardCode <$> getSet ()
    actions <- getActions iid (WhenSkillTest skillType) ()
    isScenarioAbility <- getIsScenarioAbility
    source <-
      asks $ fromJustNote "damage outside skill test" . getSource ForSkillTest
    cannotCommitCards <-
      elem CannotCommitCards
      . map modifierType
      <$> getModifiersFor source (InvestigatorTarget investigatorId) ()
    let
      triggerMessage = StartSkillTest investigatorId
      beginMessage = BeforeSkillTest iid skillType
      committableCards = if cannotCommitCards
        then []
        else flip filter investigatorHand $ \case
          PlayerCard MkPlayerCard {..} ->
            pcId
              `notElem` committedCardIds
              && (SkillWild `elem` pcSkills || skillType `elem` pcSkills)
              && (MaxOnePerTest
                 `notElem` pcCommitRestrictions
                 || pcCardCode
                 `notElem` committedCardCodes
                 )
              && (ScenarioAbility
                 `notElem` pcCommitRestrictions
                 || isScenarioAbility
                 )
          _ -> False
    if not (null committableCards) || not (null committedCardIds) || not
      (null actions)
    then
      unshiftMessage
        (SkillTestAsk $ chooseOne
          iid
          (map
              (\card ->
                Run [SkillTestCommitCard iid (getCardId card), beginMessage]
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
    else
      unshiftMessage (SkillTestAsk $ chooseOne iid [triggerMessage])
    pure a
  BeforeSkillTest iid skillType | iid /= investigatorId -> do
    locationId <- getId iid
    isScenarioAbility <- getIsScenarioAbility
    when (locationId == investigatorLocation) $ do
      committedCardIds <- map unCommittedCardId <$> getSetList investigatorId
      committedCardCodes <- mapSet unCommittedCardCode <$> getSet ()
      let
        beginMessage = BeforeSkillTest iid skillType
        committableCards = if not (null committedCardIds)
          then []
          else flip filter investigatorHand $ \case
            PlayerCard MkPlayerCard {..} ->
              pcId
                `notElem` committedCardIds
                && (SkillWild `elem` pcSkills || skillType `elem` pcSkills)
                && (OnlyYourTest `notElem` pcCommitRestrictions)
                && (MaxOnePerTest
                   `notElem` pcCommitRestrictions
                   || pcCardCode
                   `notElem` committedCardCodes
                   )
                && (ScenarioAbility
                   `notElem` pcCommitRestrictions
                   || isScenarioAbility
                   )
            _ -> False
      when (not (null committableCards) || not (null committedCardIds))
        $ unshiftMessage
            (SkillTestAsk $ chooseOne
              investigatorId
              (map
                  (\card ->
                    Run
                      [ SkillTestCommitCard investigatorId (getCardId card)
                      , beginMessage
                      ]
                  )
                  committableCards
              <> map
                   (\cardId ->
                     Run
                       [ SkillTestUncommitCard investigatorId cardId
                       , beginMessage
                       ]
                   )
                   committedCardIds
              )
            )
    pure a
  CheckWindow iid windows | iid == investigatorId -> do
    actions <- fmap concat <$> for windows $ \window -> getActions iid window ()
    playableCards <- getPlayableCards a windows
    if not (null playableCards) || not (null actions)
      then if any isForced actions
        then a <$ unshiftMessage
          (chooseOne iid $ map (Run . (: [CheckWindow iid windows])) actions)
        else a <$ unshiftMessage
          (chooseOne iid
          $ [ Run
                [ PayCardCost iid (getCardId c)
                , PlayCard iid (getCardId c) Nothing False
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
  TakeAction iid mAction cost | iid == investigatorId -> a <$ unshiftMessages
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
      PlayerCard card' -> when (pcRevelation card')
        $ unshiftMessage (Revelation iid (PlayerCardSource $ getCardId card'))
      _ -> pure ()
    pure $ a & handL %~ (card :)
  ShuffleCardsIntoDeck iid cards | iid == investigatorId -> do
    deck <- shuffleM (cards <> unDeck investigatorDeck)
    pure $ a & deckL .~ Deck deck
  AddToHandFromDeck iid cardId | iid == investigatorId -> do
    let
      card = fromJustNote "card did not exist"
        $ find ((== cardId) . getCardId) (unDeck investigatorDeck)
    deck <- shuffleM
      $ filter ((/= cardId) . getCardId) (unDeck investigatorDeck)
    case card of
      MkPlayerCard {..} -> do
        when (pcCardType == PlayerTreacheryType)
          $ unshiftMessage (DrewTreachery iid $ PlayerCard card)
        when (pcCardType == PlayerEnemyType)
          $ unshiftMessage (DrewPlayerEnemy iid $ PlayerCard card)
    pure $ a & deckL .~ Deck deck & handL %~ (PlayerCard card :)
  DisengageEnemy iid eid | iid == investigatorId ->
    pure $ a & engagedEnemiesL %~ deleteSet eid
  SearchDeckForTraits iid (InvestigatorTarget iid') traits
    | iid' == investigatorId -> runMessage
      (SearchTopOfDeck
        iid
        (InvestigatorTarget iid')
        (length $ unDeck investigatorDeck)
        traits
        ShuffleBackIn
      )
      a
  SearchTopOfDeck iid (InvestigatorTarget iid') n traits strategy
    | iid' == investigatorId -> do
      let
        (cards, deck) = splitAt n $ unDeck investigatorDeck
        traits' = setFromList traits
      unshiftMessage $ EndSearch iid
      case strategy of
        PutBackInAnyOrder -> unshiftMessage
          (chooseOneAtATime iid
          $ [ AddFocusedToTopOfDeck
                iid
                (InvestigatorTarget iid')
                (getCardId card)
            | card <- cards
            ]
          )
        ShuffleBackIn -> do
          let
            choices =
              [ Run
                  [ AddFocusedToHand
                    iid
                    (InvestigatorTarget iid')
                    (getCardId card)
                  , ShuffleAllFocusedIntoDeck iid (InvestigatorTarget iid')
                  ]
              | card <- cards
              , null traits'
                || not (null $ traits' `intersection` getTraits card)
              ]
          unshiftMessage
            (chooseOne iid $ if null choices
              then
                [ Label
                    "No cards found"
                    [ShuffleAllFocusedIntoDeck iid (InvestigatorTarget iid')]
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
      unless (null actions) $ unshiftMessage
        (chooseOne iid
        $ actions
        <> [Continue "Skip playing fast cards or using reactions"]
        )
      unshiftMessage (FocusCards $ map PlayerCard cards)
      pure $ a & deckL .~ Deck deck
  SearchDiscard iid (InvestigatorTarget iid') traits | iid' == investigatorId ->
    do
      let traits' = setFromList traits
      unshiftMessage
        (chooseOne
          iid
          [ Run
              [ AddFocusedToHand iid (InvestigatorTarget iid') (getCardId card)
              , RemoveFromDiscard iid (getCardId card)
              , UnfocusCards
              ]
          | card <- investigatorDiscard
          , null traits' || traits' `intersection` getTraits card == traits'
          ]
        )
      a <$ unshiftMessage (FocusCards $ map PlayerCard investigatorDiscard)
  RemoveFromDiscard iid cardId | iid == investigatorId ->
    pure $ a & discardL %~ filter ((/= cardId) . getCardId)
  SufferTrauma iid physical mental | iid == investigatorId ->
    pure $ a & physicalTraumaL +~ physical & mentalTraumaL +~ mental
  GainXP iid amount | iid == investigatorId -> pure $ a & xpL +~ amount
  InvestigatorPlaceCluesOnLocation iid n | iid == investigatorId -> do
    let cluesToPlace = min n investigatorClues
    unshiftMessage
      (PlaceClues (LocationTarget investigatorLocation) cluesToPlace)
    pure $ a & cluesL -~ cluesToPlace
  InvestigatorPlaceAllCluesOnLocation iid | iid == investigatorId -> do
    unshiftMessage
      (PlaceClues (LocationTarget investigatorLocation) investigatorClues)
    pure $ a & cluesL .~ 0
  RemoveDiscardFromGame iid | iid == investigatorId -> pure $ a & discardL .~ []
  After (FailedSkillTest iid mAction _ (InvestigatorTarget iid') _ n)
    | iid == iid' && iid == investigatorId -> do
      let
        windows = maybe
          []
          (\case
            Action.Investigate -> [AfterFailInvestigationSkillTest You n]
            _ -> []
          )
          mAction
      a <$ unshiftMessage (CheckWindow iid (AfterFailSkillTest You n : windows))
  After (PassedSkillTest iid mAction source (InvestigatorTarget iid') _ n)
    | iid == iid' && iid == investigatorId -> a <$ unshiftMessage
      (CheckWindow iid [AfterPassSkillTest mAction source You n])
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
    a <$ unshiftMessage
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
        <> [ InitiatePlayCard iid (getCardId c) Nothing True
           | c <- investigatorHand
           , canAffordPlayCard || fastIsPlayable c
           , isPlayable c && not (isDynamic c)
           ]
        <> [ InitiatePlayDynamicCard iid (getCardId c) 0 Nothing True
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
