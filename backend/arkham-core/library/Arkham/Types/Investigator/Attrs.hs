{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Attrs where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Action (Action)
import qualified Arkham.Types.Action as Action
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard (playerCardAttrs)
import Arkham.Types.Card.PlayerCard.Attrs
  ( pcAction
  , pcAttackOfOpportunityModifiers
  , pcBearer
  , pcCardCode
  , pcCardType
  , pcFast
  , pcId
  , pcTraits
  , pcWeakness
  , pcWindows
  )
import qualified Arkham.Types.Card.PlayerCard.Attrs as PlayerCard
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.CommitRestriction
import Arkham.Types.EnemyId
import Arkham.Types.Helpers
import Arkham.Types.Investigator.Runner
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.TreacheryId
import Arkham.Types.Window
import ClassyPrelude hiding (unpack, (\\))
import Control.Monad.Fail
import Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.List ((\\))
import Lens.Micro
import Lens.Micro.Platform ()
import Safe (fromJustNote)
import System.Random
import System.Random.Shuffle

instance HasCardCode Attrs where
  getCardCode = unInvestigatorId . investigatorId

data Attrs = Attrs
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
  , investigatorModifiers :: HashMap Source [Modifier]
  , investigatorDefeated :: Bool
  , investigatorResigned :: Bool
  , investigatorSlots :: HashMap SlotType [Slot]
  , investigatorXP :: Int
  , investigatorPhysicalTrauma :: Int
  , investigatorMentalTrauma :: Int
  -- investigator-specific fields
  , investigatorTomeActions :: Maybe Int
  }
  deriving stock (Show, Generic)

instance ToJSON Attrs where
  toJSON = genericToJSON $ aesonOptions $ Just "investigator"
  toEncoding = genericToEncoding $ aesonOptions $ Just "investigator"

instance FromJSON Attrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "investigator"

locationId :: Lens' Attrs LocationId
locationId = lens investigatorLocation $ \m x -> m { investigatorLocation = x }

xp :: Lens' Attrs Int
xp = lens investigatorXP $ \m x -> m { investigatorXP = x }

physicalTrauma :: Lens' Attrs Int
physicalTrauma =
  lens investigatorPhysicalTrauma $ \m x -> m { investigatorPhysicalTrauma = x }

mentalTrauma :: Lens' Attrs Int
mentalTrauma =
  lens investigatorMentalTrauma $ \m x -> m { investigatorMentalTrauma = x }

modifiers :: Lens' Attrs (HashMap Source [Modifier])
modifiers =
  lens investigatorModifiers $ \m x -> m { investigatorModifiers = x }

connectedLocations :: Lens' Attrs (HashSet LocationId)
connectedLocations = lens investigatorConnectedLocations
  $ \m x -> m { investigatorConnectedLocations = x }

endedTurn :: Lens' Attrs Bool
endedTurn =
  lens investigatorEndedTurn $ \m x -> m { investigatorEndedTurn = x }

resigned :: Lens' Attrs Bool
resigned = lens investigatorResigned $ \m x -> m { investigatorResigned = x }

defeated :: Lens' Attrs Bool
defeated = lens investigatorDefeated $ \m x -> m { investigatorDefeated = x }

resources :: Lens' Attrs Int
resources =
  lens investigatorResources $ \m x -> m { investigatorResources = x }

clues :: Lens' Attrs Int
clues = lens investigatorClues $ \m x -> m { investigatorClues = x }

slots :: Lens' Attrs (HashMap SlotType [Slot])
slots = lens investigatorSlots $ \m x -> m { investigatorSlots = x }

remainingActions :: Lens' Attrs Int
remainingActions = lens investigatorRemainingActions
  $ \m x -> m { investigatorRemainingActions = x }

actionsTaken :: Lens' Attrs [Action]
actionsTaken =
  lens investigatorActionsTaken $ \m x -> m { investigatorActionsTaken = x }

engagedEnemies :: Lens' Attrs (HashSet EnemyId)
engagedEnemies =
  lens investigatorEngagedEnemies $ \m x -> m { investigatorEngagedEnemies = x }

assets :: Lens' Attrs (HashSet AssetId)
assets = lens investigatorAssets $ \m x -> m { investigatorAssets = x }

treacheries :: Lens' Attrs (HashSet TreacheryId)
treacheries =
  lens investigatorTreacheries $ \m x -> m { investigatorTreacheries = x }

healthDamage :: Lens' Attrs Int
healthDamage =
  lens investigatorHealthDamage $ \m x -> m { investigatorHealthDamage = x }

sanityDamage :: Lens' Attrs Int
sanityDamage =
  lens investigatorSanityDamage $ \m x -> m { investigatorSanityDamage = x }

discard :: Lens' Attrs [PlayerCard]
discard = lens investigatorDiscard $ \m x -> m { investigatorDiscard = x }

hand :: Lens' Attrs [Card]
hand = lens investigatorHand $ \m x -> m { investigatorHand = x }

deck :: Lens' Attrs (Deck PlayerCard)
deck = lens investigatorDeck $ \m x -> m { investigatorDeck = x }

facingDefeat :: Attrs -> Bool
facingDefeat a@Attrs {..} =
  investigatorHealthDamage
    >= modifiedHealth a
    || investigatorSanityDamage
    >= modifiedSanity a

skillValueFor :: SkillType -> Maybe Action -> [Modifier] -> Attrs -> Int
skillValueFor skill maction tempModifiers attrs = foldr
  applyModifier
  baseSkillValue
  (concat (HashMap.elems $ investigatorModifiers attrs) <> tempModifiers)
 where
  applyModifier (AnySkillValue m) n = max 0 (n + m)
  applyModifier (SkillModifier skillType m) n =
    if skillType == skill then max 0 (n + m) else n
  applyModifier (ActionSkillModifier action skillType m) n =
    if skillType == skill && Just action == maction then max 0 (n + m) else n
  applyModifier _ n = n
  baseSkillValue = case skill of
    SkillWillpower -> investigatorWillpower attrs
    SkillIntellect -> investigatorIntellect attrs
    SkillCombat -> investigatorCombat attrs
    SkillAgility -> investigatorAgility attrs
    SkillWild -> error "investigators do not have wild skills"

damageValueFor :: Int -> Attrs -> Int
damageValueFor baseValue attrs = foldr
  applyModifier
  baseValue
  (concat . HashMap.elems $ investigatorModifiers attrs)
 where
  applyModifier (DamageDealt m) n = max 0 (n + m)
  applyModifier _ n = n

getActionsForTurn :: Attrs -> Int
getActionsForTurn Attrs {..} = foldr
  applyModifier
  3
  (concat . HashMap.elems $ investigatorModifiers)
 where
  applyModifier (AdditionalActions m) n = max 0 (n + m)
  applyModifier _ n = n

canDiscoverClues :: Attrs -> Bool
canDiscoverClues Attrs {..} = not
  (any match (concat . HashMap.elems $ investigatorModifiers))
 where
  match CannotDiscoverClues{} = True
  match _ = False

canSpendClues :: Attrs -> Bool
canSpendClues Attrs {..} = not
  (any match (concat . HashMap.elems $ investigatorModifiers))
 where
  match CannotSpendClues{} = True
  match _ = False

modifiedHealth :: Attrs -> Int
modifiedHealth Attrs {..} = foldr
  applyModifier
  investigatorHealth
  (concat . HashMap.elems $ investigatorModifiers)
 where
  applyModifier (HealthModifier m) n = max 0 (n + m)
  applyModifier _ n = n

modifiedSanity :: Attrs -> Int
modifiedSanity Attrs {..} = foldr
  applyModifier
  investigatorSanity
  (concat . HashMap.elems $ investigatorModifiers)
 where
  applyModifier (SanityModifier m) n = max 0 (n + m)
  applyModifier _ n = n

removeFromSlots :: AssetId -> HashMap SlotType [Slot] -> HashMap SlotType [Slot]
removeFromSlots aid = HashMap.map (map (removeIfMatches aid))

fitsAvailableSlots :: [SlotType] -> [Trait] -> Attrs -> Bool
fitsAvailableSlots slotTypes traits a = null
  (slotTypes \\ concatMap
    (\slotType -> availableSlotTypesFor slotType traits a)
    (HashSet.toList (HashSet.fromList slotTypes))
  )

availableSlotTypesFor :: SlotType -> [Trait] -> Attrs -> [SlotType]
availableSlotTypesFor slotType traits a =
  case HashMap.lookup slotType (a ^. slots) of
    Nothing -> []
    Just slots' ->
      replicate (length (filter (canPutIntoSlot traits) slots')) slotType

placeInAvailableSlot :: AssetId -> [Trait] -> [Slot] -> [Slot]
placeInAvailableSlot _ _ [] = error "could not find empty slot"
placeInAvailableSlot aid traits (x : xs) = if canPutIntoSlot traits x
  then putIntoSlot aid x : xs
  else x : placeInAvailableSlot aid traits xs

discardableAssets :: SlotType -> Attrs -> [AssetId]
discardableAssets slotType a = case HashMap.lookup slotType (a ^. slots) of
  Nothing -> []
  Just slots' -> mapMaybe slotItem slots'

discardableCards :: Attrs -> [Card]
discardableCards Attrs {..} = if all cardIsWeakness investigatorHand
  then investigatorHand
  else filter (not . cardIsWeakness) investigatorHand

getAttrStats :: Attrs -> Stats
getAttrStats Attrs {..} = Stats
  { health = investigatorHealth
  , sanity = investigatorSanity
  , willpower = investigatorWillpower
  , intellect = investigatorIntellect
  , combat = investigatorCombat
  , agility = investigatorAgility
  }

baseAttrs :: InvestigatorId -> Text -> ClassSymbol -> Stats -> [Trait] -> Attrs
baseAttrs iid name classSymbol Stats {..} traits = Attrs
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
  , investigatorTraits = HashSet.fromList traits
  , investigatorTreacheries = mempty
  , investigatorModifiers = mempty
  , investigatorDefeated = False
  , investigatorResigned = False
  , investigatorSlots = HashMap.fromList
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
  , investigatorXP = 0
  , investigatorPhysicalTrauma = 0
  , investigatorMentalTrauma = 0
  , investigatorTomeActions = Nothing
  }

matchTarget :: Attrs -> ActionTarget -> Action -> Bool
matchTarget attrs (FirstOneOf as) action =
  action `elem` as && all (`notElem` investigatorActionsTaken attrs) as
matchTarget _ (IsAction a) action = action == a

actionCost :: Attrs -> Action -> Int
actionCost attrs a = foldr
  applyModifier
  1
  (concat . HashMap.elems $ investigatorModifiers attrs)
 where
  applyModifier (ActionCostOf match m) n =
    if matchTarget attrs match a then n + m else n
  applyModifier _ n = n

actionCostModifier :: Attrs -> Maybe Action -> Int
actionCostModifier _ Nothing = 0
actionCostModifier attrs (Just a) = foldr
  applyModifier
  0
  (concat . HashMap.elems $ investigatorModifiers attrs)
 where
  applyModifier (ActionCostOf match m) n =
    if matchTarget attrs match a then n + m else n
  applyModifier _ n = n

cluesToDiscover :: Attrs -> Int -> Int
cluesToDiscover attrs startValue = foldr
  applyModifier
  startValue
  (concat . HashMap.elems $ investigatorModifiers attrs)
 where
  applyModifier (DiscoveredClues m) n = n + m
  applyModifier _ n = n

canAfford :: Attrs -> Action -> Bool
canAfford a@Attrs {..} actionType =
  actionCost a actionType <= investigatorRemainingActions

fastIsPlayable :: Attrs -> [Window] -> Card -> Bool
fastIsPlayable _ _ (EncounterCard _) = False -- TODO: there might be some playable ones?
fastIsPlayable a windows c@(PlayerCard pc) =
  pcFast (playerCardAttrs pc) && isPlayable a windows c

modifiedCardCost :: Attrs -> Card -> Int
modifiedCardCost Attrs {..} (PlayerCard pc) = foldr
  applyModifier
  startingCost
  (concat . HashMap.elems $ investigatorModifiers)
 where
  PlayerCard.Attrs {..} = playerCardAttrs pc
  startingCost = case pcCost of
    StaticCost n -> n
    DynamicCost -> 0
  applyModifier (ReduceCostOf traits m) n
    | not
      (null (HashSet.fromList traits `intersection` HashSet.fromList pcTraits))
    = max 0 (n - m)
  applyModifier _ n = n
modifiedCardCost Attrs {..} (EncounterCard MkEncounterCard {..}) = foldr
  applyModifier
  (error "we need so specify ecCost for this to work")
  (concat . HashMap.elems $ investigatorModifiers)
 where
  applyModifier (ReduceCostOf traits m) n
    | not
      (null (HashSet.fromList traits `intersection` HashSet.fromList ecTraits))
    = max 0 (n - m)
  applyModifier _ n = n

isPlayable :: Attrs -> [Window] -> Card -> Bool
isPlayable _ _ (EncounterCard _) = False -- TODO: there might be some playable ones?
isPlayable a@Attrs {..} windows c@(PlayerCard pc) =
  (pcCardType /= SkillType)
    && (modifiedCardCost a c <= investigatorResources)
    && none prevents (concat . HashMap.elems $ investigatorModifiers)
    && (not pcFast || (pcFast && cardInWindows windows c a))
    && (pcAction /= Just Action.Evade || not (null investigatorEngagedEnemies))
 where
  PlayerCard.Attrs {..} = playerCardAttrs pc
  none f = not . any f
  prevents (CannotPlay types) = pcCardType `elem` types
  prevents _ = False

drawOpeningHand :: Attrs -> Int -> ([PlayerCard], [Card], [PlayerCard])
drawOpeningHand a n = go n (a ^. discard, a ^. hand, coerce (a ^. deck))
 where
  go 0 (d, h, cs) = (d, h, cs)
  go _ (_, _, []) =
    error "this should never happen, it means the deck was empty during drawing"
  go m (d, h, c : cs) = if pcWeakness (playerCardAttrs c)
    then go m (c : d, h, cs)
    else go (m - 1) (d, PlayerCard c : h, cs)

cardInWindows :: [Window] -> Card -> Attrs -> Bool
cardInWindows windows c _ = case c of
  PlayerCard pc ->
    not
      . null
      $ pcWindows (playerCardAttrs pc)
      `intersect` HashSet.fromList windows
  _ -> False

playableCards :: Attrs -> [Window] -> [Card]
playableCards a@Attrs {..} windows =
  filter (fastIsPlayable a windows) investigatorHand
    <> playableDiscards a windows

playableDiscards :: Attrs -> [Window] -> [Card]
playableDiscards a@Attrs {..} windows = filter
  (fastIsPlayable a windows)
  possibleCards
 where
  possibleCards = map (PlayerCard . snd)
    $ filter canPlayFromDiscard (zip @_ @Int [0 ..] investigatorDiscard)
  canPlayFromDiscard (n, card) = any
    (allowsPlayFromDiscard n card)
    (concat . HashMap.elems $ investigatorModifiers)
  allowsPlayFromDiscard 0 pc (CanPlayTopOfDiscard (mcardType, traits)) =
    let PlayerCard.Attrs {..} = playerCardAttrs pc
    in
      maybe True (== pcCardType) mcardType
        && (null traits
           || (HashSet.fromList traits
              `HashSet.isSubsetOf` HashSet.fromList pcTraits
              )
           )
  allowsPlayFromDiscard _ _ _ = False


possibleSkillTypeChoices :: SkillType -> Attrs -> [SkillType]
possibleSkillTypeChoices skillType attrs = foldr
  applyModifier
  [skillType]
  (concat . HashMap.elems $ investigatorModifiers attrs)
 where
  applyModifier (UseSkillInPlaceOf toReplace toUse) skills
    | toReplace == skillType = toUse : skills
  applyModifier _ skills = skills

instance IsInvestigator Attrs where
  locationOf Attrs {..} = investigatorLocation
  discardOf Attrs {..} = investigatorDiscard
  handOf Attrs {..} = investigatorHand
  deckOf Attrs {..} = unDeck investigatorDeck
  resourceCount Attrs {..} = investigatorResources
  canDo action a@Attrs {..} = canAfford a action
  clueCount Attrs {..} = investigatorClues
  spendableClueCount a@Attrs {..} =
    if canSpendClues a then investigatorClues else 0
  cardCount Attrs {..} = length investigatorHand
  discardableCardCount a = length $ discardableCards a
  canInvestigate location a@Attrs {..} =
    canDo Action.Investigate a && getId () location == investigatorLocation
  canMoveTo location a@Attrs {..} =
    canDo Action.Move a
      && getId () location
      `elem` investigatorConnectedLocations
  canEvade enemy a@Attrs {..} =
    canDo Action.Evade a && getId () enemy `elem` investigatorEngagedEnemies
  canFight _ a@Attrs {..} = canDo Action.Fight a
  canEngage enemy a@Attrs {..} =
    canDo Action.Engage a && getId () enemy `notElem` investigatorEngagedEnemies
  hasActionsRemaining Attrs {..} _actionType traits =
    let
      hasTomeActionsRemaining =
        Tome `member` traits && maybe False (> 0) investigatorTomeActions
    in investigatorRemainingActions > 0 || hasTomeActionsRemaining
  canTakeDirectDamage a = not (facingDefeat a)
  remainingHealth a = modifiedHealth a - investigatorHealthDamage a
  remainingSanity a = modifiedSanity a - investigatorSanityDamage a

instance HasId InvestigatorId () Attrs where
  getId _ Attrs {..} = investigatorId

instance HasActions env investigator Attrs where
  getActions _ _ _ = pure []

runInvestigatorMessage
  :: (InvestigatorRunner Attrs env, MonadReader env m, MonadIO m, MonadFail m)
  => Message
  -> Attrs
  -> m Attrs
runInvestigatorMessage msg a@Attrs {..} = case msg of
  ResetGame -> pure $ (baseAttrs
                        investigatorId
                        investigatorName
                        investigatorClass
                        (getAttrStats a)
                        (HashSet.toList investigatorTraits)
                      )
    { investigatorXP = investigatorXP
    , investigatorPhysicalTrauma = investigatorPhysicalTrauma
    , investigatorMentalTrauma = investigatorMentalTrauma
    , investigatorSanityDamage = investigatorMentalTrauma
    , investigatorHealthDamage = investigatorPhysicalTrauma
    }
  SetupInvestigators -> do
    let (discard', hand', deck') = drawOpeningHand a 5
    pure
      $ a
      & (resources .~ 5)
      & (discard .~ discard')
      & (hand .~ hand')
      & (deck .~ Deck deck')
  InvestigatorMulligan iid | iid == investigatorId -> if null investigatorHand
    then a <$ unshiftMessage (FinishedWithMulligan investigatorId)
    else a <$ unshiftMessage
      (Ask iid
      $ ChooseOne
      $ Run
          [Continue "Done With Mulligan", FinishedWithMulligan investigatorId]
      : [ Run [DiscardCard iid (getCardId card), InvestigatorMulligan iid]
        | card <- investigatorHand
        ]
      )
  AllRandomDiscard | not (a ^. defeated || a ^. resigned) ->
    a <$ unshiftMessage (RandomDiscard investigatorId)
  RandomDiscard iid | iid == investigatorId -> do
    n <- liftIO $ randomRIO (0, length investigatorHand - 1)
    case investigatorHand !!? n of
      Nothing -> pure a
      Just c -> a <$ unshiftMessage (DiscardCard investigatorId (getCardId c))
  FinishedWithMulligan iid | iid == investigatorId -> do
    let
      (discard', hand', deck') =
        drawOpeningHand a (5 - length investigatorHand)
    unshiftMessage (ShuffleDiscardBackIn iid)
    pure
      $ a
      & (resources .~ 5)
      & (discard .~ discard')
      & (hand .~ hand')
      & (deck .~ Deck deck')
  ShuffleDiscardBackIn iid | iid == investigatorId ->
    if not (null investigatorDiscard)
      then do
        deck' <- liftIO
          $ shuffleM (investigatorDiscard <> coerce investigatorDeck)
        pure $ a & discard .~ [] & deck .~ Deck deck'
      else pure a
  Resign iid | iid == investigatorId -> do
    unshiftMessage (InvestigatorResigned iid)
    pure $ a & resigned .~ True
  InvestigatorDefeated iid | iid == investigatorId ->
    a <$ unshiftMessage (InvestigatorWhenEliminated iid)
  InvestigatorResigned iid | iid == investigatorId ->
    a <$ unshiftMessage (InvestigatorWhenEliminated iid)
  -- InvestigatorWhenEliminated is handled by the scenario
  InvestigatorEliminated iid | iid == investigatorId -> do
    unshiftMessage
      (PlaceClues (LocationTarget investigatorLocation) investigatorClues)
    pure $ a & clues .~ 0 & resources .~ 0
  EnemyMove eid _ lid | lid == investigatorLocation -> do
    aloofEnemyIds <- HashSet.map unAloofEnemyId
      <$> asks (getSet investigatorLocation)
    when (eid `notElem` aloofEnemyIds)
      $ unshiftMessage (EnemyEngageInvestigator eid investigatorId)
    pure a
  EnemyEngageInvestigator eid iid | iid == investigatorId ->
    pure $ a & engagedEnemies %~ HashSet.insert eid
  EnemyDefeated eid _ _ _ -> do
    unshiftMessage
      (RemoveAllModifiersOnTargetFrom
        (InvestigatorTarget investigatorId)
        (EnemySource eid)
      )
    pure $ a & engagedEnemies %~ HashSet.delete eid
  RemoveEnemy eid -> pure $ a & engagedEnemies %~ HashSet.delete eid
  TakeControlOfAsset iid aid | iid == investigatorId ->
    pure $ a & assets %~ HashSet.insert aid
  ChooseAndDiscardAsset iid | iid == investigatorId -> a <$ unshiftMessage
    (Ask iid $ ChooseOne $ map
      (Discard . AssetTarget)
      (HashSet.toList $ a ^. assets)
    )
  AttachTreachery tid (InvestigatorTarget iid) | iid == investigatorId ->
    pure $ a & treacheries %~ HashSet.insert tid
  AllCheckHandSize | not (a ^. defeated || a ^. resigned) -> do
    when (length investigatorHand > 8)
      $ unshiftMessage (CheckHandSize investigatorId)
    pure a
  CheckHandSize iid | iid == investigatorId -> do
    when (length investigatorHand > 8) $ unshiftMessage
      (Ask iid $ ChooseOne
        [ Run [DiscardCard iid (getCardId card), CheckHandSize iid]
        | card <- investigatorHand
        ]
      )
    pure a
  AddToDiscard iid pc | iid == investigatorId -> pure $ a & discard %~ (pc :)
  ChooseAndDiscardCard iid | iid == investigatorId -> do
    a <$ unshiftMessage
      (Ask iid
      $ ChooseOne
      $ [ DiscardCard iid (getCardId card) | card <- discardableCards a ]
      )
  DiscardCard iid cardId | iid == investigatorId -> do
    let
      card = fromJustNote "must be in hand"
        $ find ((== cardId) . getCardId) investigatorHand
    case card of
      PlayerCard pc ->
        pure $ a & hand %~ filter ((/= cardId) . getCardId) & discard %~ (pc :)
      EncounterCard _ -> pure $ a & hand %~ filter ((/= cardId) . getCardId) -- TODO: This should discard to the encounter discard
  RemoveCardFromHand iid cardCode | iid == investigatorId ->
    pure $ a & hand %~ filter ((/= cardCode) . getCardCode)
  ShuffleIntoDeck iid (TreacheryTarget tid) | iid == investigatorId -> do
    unshiftMessage
      (RemoveAllModifiersOnTargetFrom
        (InvestigatorTarget investigatorId)
        (TreacherySource tid)
      )
    pure $ a & treacheries %~ HashSet.delete tid
  Discard (TreacheryTarget tid) -> do
    unshiftMessage
      (RemoveAllModifiersOnTargetFrom
        (InvestigatorTarget investigatorId)
        (TreacherySource tid)
      )
    pure $ a & treacheries %~ HashSet.delete tid
  Discard (EnemyTarget eid) -> do
    unshiftMessage
      (RemoveAllModifiersOnTargetFrom
        (InvestigatorTarget investigatorId)
        (EnemySource eid)
      )
    pure $ a & engagedEnemies %~ HashSet.delete eid
  Discarded (AssetTarget aid) cardCode | aid `elem` investigatorAssets -> do
    unshiftMessage
      (RemoveAllModifiersOnTargetFrom
        (InvestigatorTarget investigatorId)
        (AssetSource aid)
      )
    pure
      $ a
      & (assets %~ HashSet.delete aid)
      & (discard %~ (lookupPlayerCard cardCode (CardId $ unAssetId aid) :))
      & (slots %~ removeFromSlots aid)
  ChooseFightEnemy iid skillType tempModifiers tokenResponses isAction
    | iid == investigatorId -> do
      enemyIds <- asks (getSet investigatorLocation)
      aloofEnemyIds <- HashSet.map unAloofEnemyId
        <$> asks (getSet investigatorLocation)
      let
        fightableEnemyIds =
          investigatorEngagedEnemies
            `union` (enemyIds `difference` aloofEnemyIds)
      a <$ unshiftMessage
        (Ask iid $ ChooseOne
          [ FightEnemy iid eid skillType tempModifiers tokenResponses isAction
          | eid <- HashSet.toList fightableEnemyIds
          ]
        )
  EngageEnemy iid eid True | iid == investigatorId -> a <$ unshiftMessages
    [TakeAction iid 1 (Just Action.Engage), EngageEnemy iid eid False]
  EngageEnemy iid eid False | iid == investigatorId ->
    pure $ a & engagedEnemies %~ HashSet.insert eid
  EngageEnemy iid eid False | iid /= investigatorId ->
    pure $ a & engagedEnemies %~ HashSet.delete eid
  FightEnemy iid eid skillType tempModifiers tokenResponses True
    | iid == investigatorId -> a <$ unshiftMessages
      [ TakeAction iid 1 (Just Action.Fight)
      , FightEnemy iid eid skillType tempModifiers tokenResponses False
      ]
  FightEnemy iid eid skillType tempModifiers tokenResponses False
    | iid == investigatorId -> do
      unshiftMessages
        [ WhenAttackEnemy iid eid
        , AttackEnemy iid eid skillType tempModifiers tokenResponses
        , AfterAttackEnemy iid eid
        ]
      pure a
  FailedAttackEnemy iid eid | iid == investigatorId -> do
    investigatorIds <- HashSet.toList <$> asks (getSet eid)
    case investigatorIds of
      [x] | x /= iid -> unshiftMessage (InvestigatorDamageInvestigator iid x)
      _ -> pure ()
    pure a
  InvestigatorDamageInvestigator iid xid | iid == investigatorId -> do
    let damage = damageValueFor 1 a
    a <$ unshiftMessage
      (InvestigatorAssignDamage xid (InvestigatorSource iid) damage 0)
  InvestigatorDamageEnemy iid eid | iid == investigatorId -> do
    let damage = damageValueFor 1 a
    a <$ unshiftMessage (EnemyDamage eid iid (InvestigatorSource iid) damage)
  EnemyEvaded iid eid | iid == investigatorId ->
    pure $ a & engagedEnemies %~ HashSet.delete eid
  AddToVictory (EnemyTarget eid) ->
    pure $ a & engagedEnemies %~ HashSet.delete eid
  ChooseEvadeEnemy iid skillType onSuccess onFailure tokenResponses isAction
    | iid == investigatorId -> a <$ unshiftMessage
      (Ask iid $ ChooseOne $ map
        (\eid -> EvadeEnemy
          iid
          eid
          skillType
          onSuccess
          onFailure
          tokenResponses
          isAction
        )
        (HashSet.toList investigatorEngagedEnemies)
      )
  EvadeEnemy iid eid skillType onSuccess onFailure tokenResponses True
    | iid == investigatorId -> a <$ unshiftMessages
      [ TakeAction iid 1 (Just Action.Evade)
      , EvadeEnemy iid eid skillType onSuccess onFailure tokenResponses False
      ]
  EvadeEnemy iid eid skillType onSuccess onFailure tokenResponses False
    | iid == investigatorId -> a <$ unshiftMessages
      [ WhenEvadeEnemy iid eid
      , TryEvadeEnemy iid eid skillType onSuccess onFailure [] tokenResponses
      , AfterEvadeEnemy iid eid
      ]
  MoveAction iid lid True | iid == investigatorId -> a <$ unshiftMessages
    [ TakeAction iid 1 (Just Action.Move)
    , CheckAttackOfOpportunity iid False
    , MoveAction iid lid False
    ]
  MoveAction iid lid False | iid == investigatorId -> a <$ unshiftMessages
    [Will (MoveTo iid lid), MoveFrom iid investigatorLocation, MoveTo iid lid]
  InvestigatorDirectDamage iid source damage horror
    | iid == investigatorId && not
      (investigatorDefeated || investigatorResigned)
    -> a <$ unshiftMessages
      ([InvestigatorDamage iid source damage horror, CheckDefeated]
      <> [After (InvestigatorTakeDamage iid source damage horror)]
      )
  InvestigatorAssignDamage iid source damage horror
    | iid == investigatorId && not
      (investigatorDefeated || investigatorResigned)
    -> a <$ unshiftMessages
      ([ InvestigatorDoAssignDamage iid source damage horror [] []
       , CheckDefeated
       ]
      <> [After (InvestigatorTakeDamage iid source damage horror)]
      )
  InvestigatorDoAssignDamage iid source 0 0 damageTargets horrorTargets
    | iid == investigatorId -> a <$ unshiftMessages
      ([ DidReceiveDamage target source
       | target <- HashSet.toList (HashSet.fromList damageTargets)
       ]
      <> [ DidReceiveHorror target source
         | target <- HashSet.toList (HashSet.fromList horrorTargets)
         ]
      )
  DidReceiveHorror (InvestigatorTarget iid) _ | iid == investigatorId ->
    a <$ unshiftMessage (CheckWindow iid [AfterAssignedHorror You])
  InvestigatorDoAssignDamage iid source health sanity damageTargets horrorTargets
    | iid == investigatorId
    -> do
      healthDamageMessages <- if health > 0
        then do
          let
            assignRestOfHealthDamage = InvestigatorDoAssignDamage
              investigatorId
              source
              (health - 1)
              sanity
          healthDamageableAssets <-
            map unHealthDamageableAssetId . HashSet.toList <$> asks (getSet iid)
          pure
            $ Run
                [ InvestigatorDamage investigatorId source 1 0
                , assignRestOfHealthDamage
                  (InvestigatorTarget investigatorId : damageTargets)
                  horrorTargets
                ]
            : [ Run
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
          let
            assignRestOfSanityDamage = InvestigatorDoAssignDamage
              investigatorId
              source
              health
              (sanity - 1)
          sanityDamageableAssets <-
            map unSanityDamageableAssetId . HashSet.toList <$> asks (getSet iid)
          pure
            $ Run
                [ InvestigatorDamage investigatorId source 0 1
                , assignRestOfSanityDamage
                  damageTargets
                  (InvestigatorTarget investigatorId : horrorTargets)
                ]
            : [ Run
                  [ AssetDamage aid source 0 1
                  , assignRestOfSanityDamage
                    damageTargets
                    (AssetTarget aid : horrorTargets)
                  ]
              | aid <- sanityDamageableAssets
              ]
        else pure []
      a <$ unshiftMessage
        (Ask iid $ ChooseOne $ healthDamageMessages <> sanityDamageMessages)
  Investigate iid lid skillType modifiers' tokenResponses overrides True
    | iid == investigatorId -> a <$ unshiftMessages
      [ TakeAction iid 1 (Just Action.Investigate)
      , CheckAttackOfOpportunity iid False
      , Investigate iid lid skillType modifiers' tokenResponses overrides False
      ]
  InvestigatorDiscoverCluesAtTheirLocation iid n | iid == investigatorId ->
    runMessage (InvestigatorDiscoverClues iid investigatorLocation n) a
  InvestigatorDiscoverClues iid lid n | iid == investigatorId ->
    if canDiscoverClues a
      then
        a <$ unshiftMessage
          (DiscoverCluesAtLocation iid lid (cluesToDiscover a n))
      else pure a
  GainClues iid n | iid == investigatorId -> do
    pure $ a & clues +~ n
  DiscoverClues iid lid n | iid == investigatorId -> do
    a <$ unshiftMessage (AfterDiscoverClues iid lid n)
  AfterDiscoverClues iid _ n | iid == investigatorId -> pure $ a & clues +~ n
  PayCardCost iid cardId | iid == investigatorId -> do
    let
      card =
        fromJustNote "not in hand" $ find ((== cardId) . getCardId) (a ^. hand)
      cost = getCost card
    pure $ a & resources -~ cost
  PayDynamicCardCost iid cardId n beforePlayMessages | iid == investigatorId ->
    do
      let
        resolve =
          beforePlayMessages <> [PayedForDynamicCard iid cardId n False]
      if investigatorResources > n
        then a <$ unshiftMessage
          (Ask
            iid
            (ChooseOne
              [ Label
                "Increase spent resources"
                [PayDynamicCardCost iid cardId (n + 1) beforePlayMessages]
              , Label ("Resolve with cost of " <> tshow n) resolve
              ]
            )
          )
        else a <$ unshiftMessages resolve
  PayedForDynamicCard iid cardId n False | iid == investigatorId -> do
    unshiftMessage (PlayDynamicCard iid cardId n False)
    pure $ a & resources -~ n
  PlayDynamicCard iid cardId _n True | iid == investigatorId -> do
    let
      card = fromJustNote "not in hand"
        $ find ((== cardId) . getCardId) investigatorHand
      isFast = case card of
        PlayerCard pc -> pcFast (playerCardAttrs pc)
        _ -> False
      maction = case card of
        PlayerCard pc -> pcAction (playerCardAttrs pc)
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
            `notElem` pcAttackOfOpportunityModifiers (playerCardAttrs pc)
        _ -> actionProvokesAttackOfOpportunities
      actionCost' = if isFast then 0 else maybe 1 (actionCost a) maction
      aooMessage = if provokesAttackOfOpportunities
        then [CheckAttackOfOpportunity iid isFast]
        else []
    a <$ unshiftMessages
      [ TakeAction iid actionCost' (Just Action.Play)
      , PayDynamicCardCost iid cardId 0 aooMessage
      ]
  PlayCard iid cardId True | iid == investigatorId -> do
    let
      card = fromJustNote "not in hand"
        $ find ((== cardId) . getCardId) investigatorHand
      isFast = case card of
        PlayerCard pc -> pcFast (playerCardAttrs pc)
        _ -> False
      maction = case card of
        PlayerCard pc -> pcAction (playerCardAttrs pc)
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
            `notElem` pcAttackOfOpportunityModifiers (playerCardAttrs pc)
        _ -> actionProvokesAttackOfOpportunities
      actionCost' = if isFast then 0 else maybe 1 (actionCost a) maction
      aooMessage = if provokesAttackOfOpportunities
        then [CheckAttackOfOpportunity iid isFast]
        else []
    a <$ unshiftMessages
      ([TakeAction iid actionCost' (Just Action.Play), PayCardCost iid cardId]
      <> aooMessage
      <> [PlayCard iid cardId False]
      )
  PlayedCard iid cardId discarded | iid == investigatorId -> do
    if discarded
      then
        a <$ unshiftMessages
          [Will (DiscardCard iid cardId), DiscardCard iid cardId]
      else pure $ a & hand %~ filter ((/= cardId) . getCardId)
  InvestigatorPlayAsset iid aid slotTypes traits | iid == investigatorId -> do
    let assetsUpdate = assets %~ HashSet.insert aid
    if fitsAvailableSlots slotTypes traits a
      then pure $ foldl'
        (\a' slotType ->
          a' & slots . ix slotType %~ placeInAvailableSlot aid traits
        )
        (a & assetsUpdate)
        slotTypes
      else do
        let
          missingSlotTypes = slotTypes \\ concatMap
            (\slotType -> availableSlotTypesFor slotType traits a)
            (HashSet.toList (HashSet.fromList slotTypes))
          assetsThatCanProvideSlots =
            HashSet.toList . HashSet.fromList $ concatMap
              (`discardableAssets` a)
              missingSlotTypes
        a <$ unshiftMessage
          (Ask iid $ ChooseOne
            [ Run
                [ Discard (AssetTarget aid')
                , InvestigatorPlayAsset iid aid slotTypes traits
                ]
            | aid' <- assetsThatCanProvideSlots
            ]
          )
  InvestigatorDamage iid _ health sanity | iid == investigatorId ->
    pure $ a & healthDamage +~ health & sanityDamage +~ sanity
  CheckDefeated -> if facingDefeat a
    then do
      unshiftMessage (InvestigatorWhenDefeated investigatorId)
      if investigatorHealthDamage >= modifiedHealth a
        then pure $ a & physicalTrauma +~ 1
        else pure $ a & mentalTrauma +~ 1
    else pure a
  HealDamage (InvestigatorTarget iid) amount | iid == investigatorId ->
    pure $ a & healthDamage %~ max 0 . subtract amount
  HealHorror (InvestigatorTarget iid) amount | iid == investigatorId ->
    pure $ a & sanityDamage %~ max 0 . subtract amount
  InvestigatorWhenDefeated iid | iid == investigatorId -> do
    unshiftMessage (InvestigatorDefeated iid)
    pure $ a & defeated .~ True & endedTurn .~ True
  MoveAllTo lid | not (a ^. defeated || a ^. resigned) ->
    a <$ unshiftMessage (MoveTo investigatorId lid)
  MoveTo iid lid | iid == investigatorId -> do
    connectedLocations' <- HashSet.map unConnectedLocationId
      <$> asks (getSet lid)
    unshiftMessages [WhenEnterLocation iid lid, AfterEnterLocation iid lid]
    pure $ a & locationId .~ lid & connectedLocations .~ connectedLocations'
  AddedConnection lid1 lid2 | lid1 == investigatorLocation ->
    pure $ a & (connectedLocations %~ HashSet.insert lid2)
  AddedConnection lid1 lid2 | lid2 == investigatorLocation ->
    pure $ a & (connectedLocations %~ HashSet.insert lid1)
  AddModifiers (InvestigatorTarget iid) source modifiers'
    | iid == investigatorId
    -> pure $ a & modifiers %~ HashMap.insertWith (<>) source modifiers'
  AddSlot iid slotType slot | iid == investigatorId -> do
    let
      slots' = HashMap.findWithDefault [] slotType investigatorSlots
      assetIds = mapMaybe slotItem slots'
      emptiedSlots = sort $ slot : map emptySlot slots'
    unshiftMessage (RefillSlots iid slotType assetIds)
    pure $ a & slots %~ HashMap.insert slotType emptiedSlots
  RefillSlots iid slotType assetIds | iid == investigatorId -> do
    let
      slots' = HashMap.findWithDefault [] slotType investigatorSlots
      emptiedSlots = sort $ map emptySlot slots'
    assetsWithTraits <- for assetIds $ \assetId -> do
      traits <- HashSet.toList <$> asks (getSet assetId)
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
      then pure $ a & slots %~ HashMap.insert slotType updatedSlots
      else do
        unshiftMessage
          (Ask iid $ ChooseOne
            [ Run
                [ Discard (AssetTarget aid')
                , RefillSlots iid slotType (filter (/= aid') assetIds)
                ]
            | aid' <- assetIds
            ]
          )
        pure a
  RemoveAllModifiersOnTargetFrom (InvestigatorTarget iid) source
    | iid == investigatorId -> do
      when (any (any ((source ==) . sourceOfSlot)) investigatorSlots)
        $ unshiftMessages
            [ RefillSlots iid slotType (mapMaybe slotItem slots')
            | (slotType, slots') <- HashMap.toList investigatorSlots
            ]
      pure
        $ a
        & (modifiers %~ HashMap.delete source)
        & (slots %~ HashMap.map (filter ((source /=) . sourceOfSlot)))
  RemoveAllModifiersFrom source -> do
    when (any (any ((source ==) . sourceOfSlot)) investigatorSlots)
      $ unshiftMessages
          [ RefillSlots investigatorId slotType (mapMaybe slotItem slots')
          | (slotType, slots') <- HashMap.toList investigatorSlots
          ]
    pure
      $ a
      & (modifiers %~ HashMap.delete source)
      & (slots %~ HashMap.map (filter ((source /=) . sourceOfSlot)))
  ChooseEndTurn iid | iid == investigatorId -> pure $ a & endedTurn .~ True
  BeginRound ->
    pure
      $ a
      & (endedTurn .~ False)
      & (remainingActions .~ getActionsForTurn a)
      & (actionsTaken .~ mempty)
  EndRound -> do
    lingeringEventIds <- asks (getSet ())
    pure $ a & modifiers %~ HashMap.filterWithKey
      (\k _ -> case k of
        EventSource eid -> eid `member` lingeringEventIds
        _ -> True
      )
  DrawCards iid n True | iid == investigatorId -> a <$ unshiftMessages
    [ TakeAction iid 1 (Just Action.Draw)
    , CheckAttackOfOpportunity iid False
    , DrawCards iid n False
    ]
  DrawCards iid 0 False | iid == investigatorId -> pure a
  DrawCards iid n False | iid == investigatorId ->
    if null (unDeck investigatorDeck)
      then if null investigatorDiscard
        then pure a
        else a
          <$ unshiftMessages [ShuffleDiscardBackIn iid, DrawCards iid n False]
      else do
        let
          (mcard, deck') = drawCard (coerce investigatorDeck)
          handUpdate = maybe id ((:) . PlayerCard) mcard
        case mcard of
          Just pc -> do
            let PlayerCard.Attrs {..} = playerCardAttrs pc
            when (pcCardType == PlayerTreacheryType)
              $ unshiftMessage (DrewPlayerTreachery iid pcCardCode pcId)
            when (pcCardType == PlayerEnemyType)
              $ unshiftMessage (DrewPlayerEnemy iid pcCardCode pcId)
          Nothing -> pure ()
        unshiftMessage (DrawCards iid (n - 1) False)
        pure $ a & hand %~ handUpdate & deck .~ Deck deck'
  InvestigatorSpendClues iid n | iid == investigatorId -> pure $ a & clues -~ n
  SpendResources iid n | iid == investigatorId ->
    pure $ a & resources %~ max 0 . subtract n
  TakeResources iid n True | iid == investigatorId -> a <$ unshiftMessages
    [ TakeAction iid 1 (Just Action.Resource)
    , CheckAttackOfOpportunity iid False
    , TakeResources iid n False
    ]
  TakeResources iid n False | iid == investigatorId -> pure $ a & resources +~ n
  EmptyDeck iid | iid == investigatorId -> a <$ unshiftMessages
    [ShuffleDiscardBackIn iid, InvestigatorDamage iid EmptyDeckSource 0 1]
  AllDrawEncounterCard | not (a ^. defeated || a ^. resigned) ->
    a <$ unshiftMessage
      (Ask investigatorId
      $ ChooseOne [InvestigatorDrawEncounterCard investigatorId]
      )
  When (EnemySpawn lid eid) | lid == investigatorLocation -> do
    traits <- HashSet.toList <$> asks (getSet eid)
    a <$ unshiftMessage
      (CheckWindow investigatorId [WhenEnemySpawns YourLocation traits])
  ActivateCardAbilityAction iid Ability {..} | iid == investigatorId -> do
    unshiftMessage
      (UseCardAbility
        iid
        abilitySource
        abilityProvider
        abilityMetadata
        abilityIndex
      ) -- We should check action type when added for aoo
    case abilityType of
      ForcedAbility -> pure ()
      FastAbility _ -> pure ()
      ReactionAbility _ -> pure ()
      ActionAbility n actionType ->
        if actionType
            `notElem` [ Just Action.Fight
                      , Just Action.Evade
                      , Just Action.Resign
                      , Just Action.Parley
                      ]
          then unshiftMessages
            [TakeAction iid n actionType, CheckAttackOfOpportunity iid False]
          else unshiftMessage (TakeAction iid n actionType)
    pure a
  AllDrawCardAndResource | not (a ^. defeated || a ^. resigned) -> do
    unshiftMessage (DrawCards investigatorId 1 False)
    pure $ a & resources +~ 1
  LoadDeck iid deck' | iid == investigatorId -> do
    for_ deck' $ \card -> do
      when (pcWeakness $ playerCardAttrs card) $ unshiftMessage
        (SetBearer (getCardId card) (BearerId $ getCardCode a))
    shuffled <- liftIO $ shuffleM deck'
    pure $ a & deck .~ Deck shuffled
  InvestigatorCommittedCard iid cardId | iid == investigatorId ->
    pure $ a & hand %~ filter ((/= cardId) . getCardId)
  BeforeSkillTest iid skillType | iid == investigatorId -> do
    committedCardIds <- map unCommittedCardId . HashSet.toList <$> asks
      (getSet iid)
    committedCardCodes <- HashSet.map unCommittedCardCode <$> asks (getSet ())
    actions <- join $ asks (getActions a (WhenSkillTest skillType))
    let
      triggerMessage = StartSkillTest investigatorId
      beginMessage = BeforeSkillTest iid skillType
      committableCards = flip filter investigatorHand $ \case
        PlayerCard pc ->
          let PlayerCard.Attrs {..} = playerCardAttrs pc
          in
            pcId
            `notElem` committedCardIds
            && (SkillWild `elem` pcSkills || skillType `elem` pcSkills)
            && (MaxOnePerTest
               `notElem` pcCommitRestrictions
               || pcCardCode
               `notElem` committedCardCodes
               )
        _ -> False
    if not (null committableCards) || not (null committedCardIds) || not
      (null actions)
    then
      unshiftMessage
        (SkillTestAsk $ Ask iid $ ChooseOne
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
      unshiftMessage (SkillTestAsk $ Ask iid $ ChooseOne [triggerMessage])
    pure a
  BeforeSkillTest iid skillType | iid /= investigatorId -> do
    locationId' <- asks (getId iid)
    when (locationId' == investigatorLocation) $ do
      committedCardIds <- map unCommittedCardId . HashSet.toList <$> asks
        (getSet investigatorId)
      committedCardCodes <- HashSet.map unCommittedCardCode <$> asks (getSet ())
      let
        beginMessage = BeforeSkillTest iid skillType
        committableCards = if not (null committedCardIds)
          then []
          else flip filter investigatorHand $ \case
            PlayerCard pc ->
              let PlayerCard.Attrs {..} = playerCardAttrs pc
              in
                pcId
                `notElem` committedCardIds
                && (SkillWild `elem` pcSkills || skillType `elem` pcSkills)
                && (OnlyYourTest `notElem` pcCommitRestrictions)
                && (MaxOnePerTest
                   `notElem` pcCommitRestrictions
                   || pcCardCode
                   `notElem` committedCardCodes
                   )
            _ -> False
      when (not (null committableCards) || not (null committedCardIds))
        $ unshiftMessage
            (SkillTestAsk $ Ask investigatorId $ ChooseOne
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
    actions <- fmap concat <$> for windows $ \window -> do
      join (asks (getActions a window))
    if not (null $ playableCards a windows) || not (null actions)
      then a <$ unshiftMessage
        (Ask iid
        $ ChooseOne
        $ map
            (\c -> Run
              [ PayCardCost iid (getCardId c)
              , PlayCard iid (getCardId c) False
              , CheckWindow iid windows
              ]
            )
            (playableCards a windows)
        <> map (Run . (: [CheckWindow iid windows])) actions
        <> [Continue "Skip playing fast cards or using reactions"]
        )
      else pure a
  LoseActions iid _ n | iid == investigatorId ->
    pure $ a & remainingActions %~ max 0 . subtract n
  GainActions iid _ n | iid == investigatorId ->
    pure $ a & remainingActions +~ n
  TakeAction iid actionCost' maction | iid == investigatorId -> do
    let
      costModifier = actionCostModifier a maction
      modifiedActionCost = max 0 (actionCost' + costModifier)
      actionsTakenUpdate = case maction of
        Nothing -> id
        Just action -> (<> [action])
    pure
      $ a
      & (remainingActions %~ max 0 . subtract modifiedActionCost)
      & (actionsTaken %~ actionsTakenUpdate)
  PutOnTopOfDeck iid card | iid == investigatorId ->
    pure $ a & deck %~ Deck . (card :) . unDeck
  AddToHand iid card | iid == investigatorId -> pure $ a & hand %~ (card :)
  ShuffleCardsIntoDeck iid cards | iid == investigatorId -> do
    deck' <- liftIO $ shuffleM (cards <> unDeck investigatorDeck)
    pure $ a & deck .~ Deck deck'
  AddToHandFromDeck iid cardId | iid == investigatorId -> do
    let
      card = fromJustNote "card did not exist"
        $ find ((== cardId) . getCardId) (unDeck investigatorDeck)
      PlayerCard.Attrs {..} = playerCardAttrs card
    deck' <- liftIO $ shuffleM $ filter
      ((/= cardId) . getCardId)
      (unDeck investigatorDeck)
    when (pcCardType == PlayerTreacheryType)
      $ unshiftMessage (DrewPlayerTreachery iid pcCardCode pcId)
    when (pcCardType == PlayerEnemyType)
      $ unshiftMessage (DrewPlayerEnemy iid pcCardCode pcId)
    pure $ a & deck .~ Deck deck' & hand %~ (PlayerCard card :)
  DisengageEnemy iid eid | iid == investigatorId -> do
    pure $ a & engagedEnemies %~ HashSet.delete eid
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
        (cards, deck') = splitAt n $ unDeck investigatorDeck
        traits' = HashSet.fromList traits
      case strategy of
        PutBackInAnyOrder -> unshiftMessage
          (Ask iid $ ChooseOneAtATime
            [ AddFocusedToTopOfDeck
                iid
                (InvestigatorTarget iid')
                (getCardId card)
            | card <- cards
            ]
          )
        ShuffleBackIn -> unshiftMessage
          (Ask iid $ ChooseOne
            [ Run
                [ AddFocusedToHand
                  iid
                  (InvestigatorTarget iid')
                  (getCardId card)
                , ShuffleAllFocusedIntoDeck iid (InvestigatorTarget iid')
                ]
            | card <- cards
            , null traits'
              || traits'
              `intersection` HashSet.fromList (pcTraits $ playerCardAttrs card)
              == traits'
            ]
          )
      unshiftMessage (FocusCards $ map PlayerCard cards)
      pure $ a & deck .~ Deck deck'
  SearchDiscard iid (InvestigatorTarget iid') traits | iid' == investigatorId ->
    do
      let traits' = HashSet.fromList traits
      unshiftMessage
        (Ask iid $ ChooseOne
          [ Run
              [ AddFocusedToHand iid (InvestigatorTarget iid') (getCardId card)
              , RemoveFromDiscard iid (getCardId card)
              ]
          | card <- investigatorDiscard
          , null traits'
            || traits'
            `intersection` setFromList (pcTraits $ playerCardAttrs card)
            == traits'
          ]
        )
      a <$ unshiftMessage (FocusCards $ map PlayerCard investigatorDiscard)
  RemoveFromDiscard iid cardId | iid == investigatorId ->
    pure $ a & discard %~ filter ((/= cardId) . getCardId)
  SufferTrauma iid physical mental | iid == investigatorId ->
    pure $ a & physicalTrauma +~ physical & mentalTrauma +~ mental
  GainXP iid amount | iid == investigatorId -> pure $ a & xp +~ amount
  InvestigatorPlaceCluesOnLocation iid n | iid == investigatorId -> do
    let cluesToPlace = min n investigatorClues
    unshiftMessage
      (PlaceClues (LocationTarget investigatorLocation) cluesToPlace)
    pure $ a & clues -~ cluesToPlace
  InvestigatorPlaceAllCluesOnLocation iid | iid == investigatorId -> do
    unshiftMessage
      (PlaceClues (LocationTarget investigatorLocation) investigatorClues)
    pure $ a & clues .~ 0
  RemoveDiscardFromGame iid | iid == investigatorId -> pure $ a & discard .~ []
  After (FailedSkillTest iid _ _ (InvestigatorTarget iid') n)
    | iid == iid' && iid == investigatorId -> do
      a <$ unshiftMessage (CheckWindow iid [AfterFailSkillTest You n])
  After (PassedSkillTest iid _ _ (InvestigatorTarget iid') n)
    | iid == iid' && iid == investigatorId -> do
      a <$ unshiftMessage (CheckWindow iid [AfterPassSkillTest You n])
  PlayerWindow iid additionalActions | iid == investigatorId -> do
    actions <- join $ asks (getActions a NonFast)
    fastActions <- join $ asks (getActions a (DuringTurn You))
    playerWindowActions <- join $ asks (getActions a FastPlayerWindow)
    a <$ unshiftMessage
      (Ask iid $ ChooseOne
        (additionalActions
        <> [ TakeResources iid 1 True | canAfford a Action.Resource ]
        <> [ DrawCards iid 1 True | canAfford a Action.Draw ]
        <> [ PlayCard iid (getCardId c) True
           | c <- investigatorHand
           , canAfford a Action.Play || fastIsPlayable a [DuringTurn You] c
           , isPlayable a [DuringTurn You] c && not (isDynamic c)
           ]
        <> [ PlayDynamicCard iid (getCardId c) 0 True
           | c <- investigatorHand
           , canAfford a Action.Play || fastIsPlayable a [DuringTurn You] c
           , isPlayable a [DuringTurn You] c && isDynamic c
           ]
        <> [ChooseEndTurn iid]
        <> actions
        <> fastActions
        <> playerWindowActions
        )
      )
  _ -> pure a

_PlayerCard :: Traversal' Card PlayerCard
_PlayerCard f (PlayerCard pc) = PlayerCard <$> f pc
_PlayerCard _ (EncounterCard ec) = pure (EncounterCard ec)

instance (InvestigatorRunner Attrs env) => RunMessage env Attrs where
  runMessage msg a =
    traverseOf (hand . traverse . _PlayerCard) (runMessage msg) a
      >>= runInvestigatorMessage msg
