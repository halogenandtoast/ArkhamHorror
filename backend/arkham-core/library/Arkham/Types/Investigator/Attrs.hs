{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Attrs where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.Action (ActionType)
import qualified Arkham.Types.Action as Action
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.EnemyId
import Arkham.Types.FastWindow
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
import ClassyPrelude hiding (unpack)
import Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
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
  , investigatorActionsTaken :: [ActionType]
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
  , investigatorModifiers :: [Modifier]
  , investigatorAbilities :: [Ability]
  , investigatorDefeated :: Bool
  , investigatorResigned :: Bool
  , investigatorSlots :: HashMap SlotType [Slot]
  , investigatorXP :: Int
  , investigatorPhysicalTrauma :: Int
  , investigatorMentalTrauma :: Int
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

modifiers :: Lens' Attrs [Modifier]
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

actionsTaken :: Lens' Attrs [ActionType]
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
facingDefeat Attrs {..} =
  investigatorHealthDamage
    >= investigatorHealth
    || investigatorSanityDamage
    >= investigatorSanity

skillValueFor :: SkillType -> Maybe ActionType -> [Modifier] -> Attrs -> Int
skillValueFor skill maction tempModifiers attrs = foldr
  applyModifier
  baseSkillValue
  (investigatorModifiers attrs <> tempModifiers)
 where
  applyModifier (SkillModifier skillType m _) n =
    if skillType == skill then max 0 (n + m) else n
  applyModifier (ActionSkillModifier action skillType m _) n =
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
  (investigatorModifiers attrs)
 where
  applyModifier (DamageDealt m _) n = max 0 (n + m)
  applyModifier _ n = n

removeFromSlots :: AssetId -> HashMap SlotType [Slot] -> HashMap SlotType [Slot]
removeFromSlots aid = HashMap.map (map (removeIfMatches aid))

hasEmptySlot :: SlotType -> [Trait] -> Attrs -> Bool
hasEmptySlot slotType traits a = case HashMap.lookup slotType (a ^. slots) of
  Nothing -> False
  Just slots' -> any (canPutIntoSlot traits) slots'

placeInAvailableSlot :: AssetId -> [Trait] -> [Slot] -> [Slot]
placeInAvailableSlot _ _ [] = error "could not find empty slot"
placeInAvailableSlot aid traits (x : xs) = if canPutIntoSlot traits x
  then putIntoSlot aid x : xs
  else x : placeInAvailableSlot aid traits xs

discardableAssets :: SlotType -> Attrs -> [AssetId]
discardableAssets slotType a = case HashMap.lookup slotType (a ^. slots) of
  Nothing -> []
  Just slots' -> mapMaybe slotItem slots'

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
  , investigatorAbilities = mempty
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
  }

sourceIsInvestigator :: Source -> Attrs -> Bool
sourceIsInvestigator source Attrs {..} = case source of
  InvestigatorSource sourceId -> sourceId == investigatorId
  AssetSource sourceId -> sourceId `elem` investigatorAssets
  _ -> False

matchTarget :: Attrs -> ActionTarget -> ActionType -> Bool
matchTarget attrs (FirstOneOf as) action =
  action `elem` as && all (`notElem` investigatorActionsTaken attrs) as
matchTarget _ (IsAction a) action = action == a

actionCost :: Attrs -> ActionType -> Int
actionCost attrs a = foldr applyModifier 1 (investigatorModifiers attrs)
 where
  applyModifier (ActionCostOf match m _) n =
    if matchTarget attrs match a then n + m else n
  applyModifier _ n = n

cluesToDiscover :: Attrs -> Int -> Int
cluesToDiscover attrs startValue = foldr
  applyModifier
  startValue
  (investigatorModifiers attrs)
 where
  applyModifier (DiscoveredClues m _) n = n + m
  applyModifier _ n = n

canAfford :: Attrs -> ActionType -> Bool
canAfford a@Attrs {..} actionType =
  actionCost a actionType <= investigatorRemainingActions

canPayAbilityCost :: Maybe AbilityCost -> Attrs -> Bool
canPayAbilityCost Nothing _ = True
canPayAbilityCost (Just cost) Attrs {..} = case cost of
  ResourceCost n -> investigatorResources >= n
  ClueCost n -> investigatorClues >= n
  CardCost n -> length investigatorHand >= n

canPerform
  :: (MonadReader env m, InvestigatorRunner env) => Attrs -> ActionType -> m Bool
canPerform a@Attrs {..} Action.Move = do
  blockedLocationIds <- HashSet.map unBlockedLocationId <$> asks (getSet ())
  let
    accessibleLocations =
      investigatorConnectedLocations `difference` blockedLocationIds
  pure $ canAfford a Action.Move && not (null accessibleLocations)
canPerform a Action.Investigate = pure $ canAfford a Action.Investigate
canPerform a@Attrs {..} Action.Fight = do
  enemyIds <- asks (getSet investigatorLocation)
  aloofEnemyIds <- HashSet.map unAloofEnemyId
    <$> asks (getSet investigatorLocation)
  let
    unengagedEnemyIds = enemyIds `difference` investigatorEngagedEnemies
    fightableEnemyIds =
      investigatorEngagedEnemies
        `union` (unengagedEnemyIds `difference` aloofEnemyIds)
  pure $ canAfford a Action.Fight && not (null fightableEnemyIds)
canPerform a@Attrs {..} Action.Evade =
  pure $ canAfford a Action.Evade && not (null investigatorEngagedEnemies)
canPerform a@Attrs {..} Action.Engage = do
  enemyIds <- asks (getSet investigatorLocation)
  let unengagedEnemyIds = enemyIds `difference` investigatorEngagedEnemies
  pure $ canAfford a Action.Engage && not (null unengagedEnemyIds)
canPerform a Action.Draw = pure $ canAfford a Action.Draw
canPerform a Action.Resign = pure $ canAfford a Action.Resign
canPerform a Action.Resource = pure $ canAfford a Action.Resource
canPerform a Action.Parley = pure $ canAfford a Action.Parley
canPerform a@Attrs {..} Action.Play = do
  let playableCards = filter (isPlayable a [DuringTurn You]) investigatorHand
  pure $ canAfford a Action.Play && not (null playableCards)
canPerform a@Attrs {..} Action.Ability = do
  availableAbilities <- getAvailableAbilities a
  filteredAbilities <- flip filterM availableAbilities $ \Ability {..} ->
    case abilityType of
      ActionAbility _ (Just action) -> canPerform a action -- TODO: we need to calculate the total cost
      ActionAbility _ Nothing -> pure True -- e.g. Old Book of Lore
      FastAbility _ -> pure True -- must be filtered out later
      ReactionAbility _ -> pure False

  pure $ canAfford a Action.Ability && not (null filteredAbilities)

fastIsPlayable :: Attrs -> [FastWindow] -> Card -> Bool
fastIsPlayable _ _ (EncounterCard _) = False -- TODO: there might be some playable ones?
fastIsPlayable a windows c@(PlayerCard MkPlayerCard {..}) =
  pcFast && isPlayable a windows c

isPlayable :: Attrs -> [FastWindow] -> Card -> Bool
isPlayable _ _ (EncounterCard _) = False -- TODO: there might be some playable ones?
isPlayable a@Attrs {..} windows c@(PlayerCard MkPlayerCard {..}) =
  (pcCardType /= SkillType)
    && (pcCost <= investigatorResources)
    && none prevents investigatorModifiers
    && (not pcFast || (pcFast && cardInWindows windows c a))
    && (pcAction /= Just Action.Evade || not (null investigatorEngagedEnemies))
 where
  none f = not . any f
  prevents (CannotPlay types _) = pcCardType `elem` types
  prevents _ = False

takeAction :: ActionType -> Attrs -> Attrs
takeAction action a =
  a
    & (remainingActions %~ max 0 . subtract (actionCost a action))
    & (actionsTaken %~ (<> [action]))

getAvailableAbilities
  :: (InvestigatorRunner env, MonadReader env m) => Attrs -> m [Ability]
getAvailableAbilities a@Attrs {..} = do
  assetAbilities <- mconcat
    <$> traverse (asks . getList) (HashSet.toList investigatorAssets)
  treacheryAbilities <- mconcat
    <$> traverse (asks . getList) (HashSet.toList investigatorTreacheries)
  locationAbilities <- asks (getList investigatorLocation)
  locationEnemyIds <- asks (getSet @EnemyId investigatorLocation)
  locationEnemyAbilities <- mconcat
    <$> traverse (asks . getList) (HashSet.toList locationEnemyIds)
  locationAssets <- asks (getSet @AssetId investigatorLocation)
  locationAssetAbilities <- mconcat
    <$> traverse (asks . getList) (HashSet.toList locationAssets)
  locationTreacheries <- asks (getSet @TreacheryId investigatorLocation)
  locationTreacheryAbilities <- mconcat
    <$> traverse (asks . getList) (HashSet.toList locationTreacheries)
  actAndAgendaAbilities <- asks (getList ())
  pure $ filter canPerformAbility $ mconcat
    [ investigatorAbilities
    , assetAbilities
    , treacheryAbilities
    , locationAbilities
    , locationEnemyAbilities
    , locationAssetAbilities
    , locationTreacheryAbilities
    , actAndAgendaAbilities
    ]
 where
  canPerformAbility Ability {..} = case abilityType of
    ActionAbility _ (Just actionType) -> canAfford a actionType
    ActionAbility _ Nothing -> True -- e.g. Old Book of Lore
    FastAbility Any -> True
    FastAbility _ -> True -- must be filtered out later
    ReactionAbility _ -> True

drawOpeningHand :: Attrs -> Int -> ([PlayerCard], [Card], [PlayerCard])
drawOpeningHand a n = go n (a ^. discard, a ^. hand, coerce (a ^. deck))
 where
  go 0 (d, h, cs) = (d, h, cs)
  go _ (_, _, []) =
    error "this should never happen, it means the deck was empty during drawing"
  go m (d, h, c : cs) = if pcWeakness c
    then go m (c : d, h, cs)
    else go (m - 1) (d, PlayerCard c : h, cs)

cardInWindows :: [FastWindow] -> Card -> Attrs -> Bool
cardInWindows windows c _ = case c of
  PlayerCard pc ->
    not . null $ pcFastWindows pc `intersect` HashSet.fromList windows
  _ -> False

abilityInWindows
  :: (MonadReader env m, InvestigatorRunner env)
  => [FastWindow]
  -> Ability
  -> Attrs
  -> m Bool
abilityInWindows windows ability@Ability {..} _ =
  case (abilityType, abilityLimit) of
    (ReactionAbility window, OncePerRound) -> if window `elem` windows
      then do
        usedAbilities <- map unUsedAbility <$> asks (getList ())
        pure $ ability `notElem` usedAbilities
      else pure False
    (ReactionAbility window, _) -> pure $ window `elem` windows
    _ -> pure False

possibleSkillTypeChoices :: SkillType -> Attrs -> [SkillType]
possibleSkillTypeChoices skillType attrs = foldr
  applyModifier
  [skillType]
  (investigatorModifiers attrs)
 where
  applyModifier (UseSkillInPlaceOf toReplace toUse _) skills
    | toReplace == skillType = toUse : skills
  applyModifier _ skills = skills

instance (InvestigatorRunner env) => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    PlaceCluesOnLocation iid n | iid == investigatorId -> do
      let m = min n investigatorClues
      unshiftMessage (PlaceClues (LocationTarget investigatorLocation) m)
      pure $ a & clues -~ m
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
            [ Continue "Done With Mulligan"
            , FinishedWithMulligan investigatorId
            ]
        : [ Run [DiscardCard iid (getCardId card), InvestigatorMulligan iid]
          | card <- investigatorHand
          ]
        )
    AllRandomDiscard | not (a ^. defeated || a ^. resigned) -> do
      n <- liftIO $ randomRIO (0, length investigatorHand - 1)
      case investigatorHand !!? n of
        Nothing -> pure a
        Just c ->
          a <$ unshiftMessage (DiscardCard investigatorId (getCardId c))
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
    EnemyDefeated eid _ _ _ -> pure $ a & engagedEnemies %~ HashSet.delete eid
    RemoveEnemy eid -> pure $ a & engagedEnemies %~ HashSet.delete eid
    TakeControlOfAsset iid aid | iid == investigatorId ->
      pure $ a & assets %~ HashSet.insert aid
    ChooseAndDiscardAsset iid | iid == investigatorId -> a <$ unshiftMessage
      (Ask iid $ ChooseOne $ map DiscardAsset (HashSet.toList $ a ^. assets))
    AttachTreacheryToInvestigator tid iid | iid == investigatorId ->
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
    ChooseAndDiscardCard iid | iid == investigatorId -> a <$ unshiftMessage
      (Ask iid
      $ ChooseOne
      $ [ DiscardCard iid (getCardId card) | card <- investigatorHand ]
      )
    DiscardCard iid cardId | iid == investigatorId -> do
      let
        card = fromJustNote "must be in hand"
          $ find ((== cardId) . getCardId) investigatorHand
      case card of
        PlayerCard pc ->
          pure
            $ a
            & hand
            %~ filter ((/= cardId) . getCardId)
            & discard
            %~ (pc :)
        EncounterCard _ -> pure $ a & hand %~ filter ((/= cardId) . getCardId) -- TODO: This should discard to the encounter discard
    RemoveCardFromHand iid cardCode | iid == investigatorId ->
      pure $ a & hand %~ filter ((/= cardCode) . getCardCode)
    Discard (TreacheryTarget tid) ->
      pure $ a & treacheries %~ HashSet.delete tid
    Discard (EnemyTarget eid) ->
      pure $ a & engagedEnemies %~ HashSet.delete eid
    AssetDiscarded aid cardCode | aid `elem` investigatorAssets ->
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
      [ TakeAction iid (actionCost a Action.Fight) (Just Action.Fight)
      , EngageEnemy iid eid False
      ]
    EngageEnemy iid eid False | iid == investigatorId ->
      pure $ a & engagedEnemies %~ HashSet.insert eid
    EngageEnemy iid eid False | iid /= investigatorId ->
      pure $ a & engagedEnemies %~ HashSet.delete eid
    FightEnemy iid eid skillType tempModifiers tokenResponses True
      | iid == investigatorId -> a <$ unshiftMessages
        [ TakeAction iid (actionCost a Action.Fight) (Just Action.Fight)
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
        [ TakeAction iid (actionCost a Action.Evade) (Just Action.Evade)
        , EvadeEnemy iid eid skillType onSuccess onFailure tokenResponses False
        ]
    EvadeEnemy iid eid skillType onSuccess onFailure tokenResponses False
      | iid == investigatorId -> a <$ unshiftMessages
        [ WhenEvadeEnemy iid eid
        , TryEvadeEnemy iid eid skillType onSuccess onFailure [] tokenResponses
        , AfterEvadeEnemy iid eid
        ]
    MoveAction iid lid True | iid == investigatorId -> a <$ unshiftMessages
      [ TakeAction iid (actionCost a Action.Move) (Just Action.Move)
      , CheckAttackOfOpportunity iid False
      , MoveAction iid lid False
      ]
    MoveAction iid lid False | iid == investigatorId ->
      a <$ unshiftMessage (MoveTo iid lid)
    InvestigatorAssignDamage iid source health sanity | iid == investigatorId ->
      a <$ unshiftMessages
        [InvestigatorDoAssignDamage iid source health sanity, CheckDefeated]
    InvestigatorDoAssignDamage iid _ 0 0 | iid == investigatorId -> pure a
    InvestigatorDoAssignDamage iid source health sanity
      | iid == investigatorId -> do
        healthDamageMessages <- if health > 0
          then do
            let
              assignRestOfHealthDamage = InvestigatorDoAssignDamage
                investigatorId
                source
                (health - 1)
                sanity
            healthDamageableAssets <-
              map unHealthDamageableAssetId . HashSet.toList <$> asks
                (getSet iid)
            pure
              $ Run
                  [ InvestigatorDamage investigatorId source 1 0
                  , assignRestOfHealthDamage
                  ]
              : [ Run [AssetDamage aid source 1 0, assignRestOfHealthDamage]
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
              map unSanityDamageableAssetId . HashSet.toList <$> asks
                (getSet iid)
            pure
              $ Run
                  [ InvestigatorDamage investigatorId source 0 1
                  , assignRestOfSanityDamage
                  ]
              : [ Run [AssetDamage aid source 0 1, assignRestOfSanityDamage]
                | aid <- sanityDamageableAssets
                ]
          else pure []
        a <$ unshiftMessage
          (Ask iid $ ChooseOne $ healthDamageMessages <> sanityDamageMessages)
    Investigate iid lid skillType tokenResponses True | iid == investigatorId ->
      a <$ unshiftMessages
        [ TakeAction
          iid
          (actionCost a Action.Investigate)
          (Just Action.Investigate)
        , CheckAttackOfOpportunity iid False
        , Investigate iid lid skillType tokenResponses False
        ]
    InvestigatorDiscoverClues iid lid n | iid == investigatorId ->
      a <$ unshiftMessage
        (DiscoverCluesAtLocation iid lid (cluesToDiscover a n))
    DiscoverClues iid lid n | iid == investigatorId -> do
      availableAbilities <- getAvailableAbilities a
      filteredAbilities <- filterM
        (flip (abilityInWindows [WhenDiscoverClues You YourLocation]) a)
        availableAbilities
      a <$ unshiftMessage
        (Ask iid
        $ ChooseOne
        $ map
            (\Ability {..} -> Run
              [ UseCardAbility iid abilitySource abilityProvider abilityIndex
              , DiscoverClues iid lid n
              ]
            )
            filteredAbilities
        <> [AfterDiscoverClues iid lid n]
        )
    AfterDiscoverClues iid _ n | iid == investigatorId -> pure $ a & clues +~ n
    PayCardCost iid cardId | iid == investigatorId -> do
      let
        card = fromJustNote "not in hand"
          $ find ((== cardId) . getCardId) (a ^. hand)
        cost = getCost card
      pure $ a & resources -~ cost
    PlayCard iid cardId True | iid == investigatorId -> do
      let
        card = fromJustNote "not in hand"
          $ find ((== cardId) . getCardId) investigatorHand
        isFast = case card of
          PlayerCard pc -> pcFast pc
          _ -> False
        maction = case card of
          PlayerCard pc -> pcAction pc
          _ -> Nothing
        actionCost' = if isFast then 0 else maybe 1 (actionCost a) maction
        aooMessage =
          if maction
              `notElem` [ Just Action.Evade
                        , Just Action.Parley
                        , Just Action.Resign
                        , Just Action.Fight
                        ]
            then [CheckAttackOfOpportunity iid isFast]
            else []
      a <$ unshiftMessages
        ([TakeAction iid actionCost' (Just Action.Play), PayCardCost iid cardId]
        <> aooMessage
        <> [PlayCard iid cardId False]
        )
    PlayedCard iid cardId discarded | iid == investigatorId -> do
      let
        card = fromJustNote "not in hand... spooky"
          $ find ((== cardId) . getCardId) investigatorHand
        discardUpdate = case card of
          PlayerCard pc -> if discarded then discard %~ (pc :) else id
          _ -> error "We should decide what happens here"
      pure $ a & hand %~ filter ((/= cardId) . getCardId) & discardUpdate
    InvestigatorPlayAsset iid aid slotTypes traits | iid == investigatorId -> do
      let assetsUpdate = assets %~ HashSet.insert aid
      if not (null slotTypes)
        then case slotTypes of
          [slotType] -> if hasEmptySlot slotType traits a
            then
              pure
              $ a
              & assetsUpdate
              & slots
              . ix slotType
              %~ placeInAvailableSlot aid traits
            else if not (null $ discardableAssets slotType a)
              then a <$ unshiftMessage
                (Ask iid $ ChooseOne
                  [ Run
                      [ DiscardAsset aid'
                      , InvestigatorPlayAsset iid aid slotTypes traits
                      ]
                  | aid' <- discardableAssets slotType a
                  ]
                )
              else error "could not find asset to discard"
          _ -> error "multi-slot items not handled yet"
        else pure $ a & assetsUpdate
    InvestigatorDamage iid _ health sanity | iid == investigatorId ->
      pure $ a & healthDamage +~ health & sanityDamage +~ sanity
    CheckDefeated -> a <$ when
      (facingDefeat a)
      (unshiftMessage (InvestigatorWhenDefeated investigatorId))
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
    AddedConnection lid1 lid2
      | lid1 == investigatorLocation || lid2 == investigatorLocation
      -> pure
        $ a
        & (connectedLocations %~ HashSet.insert lid1)
        & (connectedLocations %~ HashSet.insert lid2)
    AddModifier (InvestigatorTarget iid) modifier | iid == investigatorId ->
      pure $ a & modifiers %~ (modifier :)
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
                  [ DiscardAsset aid'
                  , RefillSlots iid slotType (filter (/= aid') assetIds)
                  ]
              | aid' <- assetIds
              ]
            )
          pure a
    RemoveAllModifiersOnTargetFrom (InvestigatorTarget iid) source
      | iid == investigatorId -> do
        unshiftMessages
          [ RefillSlots iid slotType (mapMaybe slotItem slots')
          | (slotType, slots') <- HashMap.toList investigatorSlots
          ]
        pure
          $ a
          & (modifiers %~ filter ((source /=) . sourceOfModifier))
          & (slots %~ HashMap.map (filter ((source /=) . sourceOfSlot)))
    ChooseEndTurn iid | iid == investigatorId -> pure $ a & endedTurn .~ True
    BeginRound ->
      pure
        $ a
        & (endedTurn .~ False)
        & (remainingActions .~ 3)
        & (actionsTaken .~ mempty)
    EndRound ->
      pure $ a & modifiers %~ filter (not . sourceIsEvent . sourceOfModifier)
    DrawCards iid n True | iid == investigatorId -> a <$ unshiftMessages
      [ TakeAction iid (actionCost a Action.Draw) (Just Action.Draw)
      , CheckAttackOfOpportunity iid False
      , DrawCards iid n False
      ]
    DrawCards iid n False | iid == investigatorId ->
      if null (unDeck investigatorDeck)
        then if null investigatorDiscard
          then pure a
          else
            a <$ unshiftMessages
              [ShuffleDiscardBackIn iid, DrawCards iid n False]
        else do
          let
            (mcard, deck') = drawCard (coerce investigatorDeck)
            handUpdate = maybe id ((:) . PlayerCard) mcard
          case mcard of
            Just MkPlayerCard {..} -> do
              when pcRevelation
                $ unshiftMessage (DrewRevelation iid pcCardCode pcId)
              when (pcCardType == PlayerEnemyType)
                $ unshiftMessage (DrewPlayerEnemy iid pcCardCode pcId)
            Nothing -> pure ()
          pure $ a & hand %~ handUpdate & deck .~ Deck deck'
    InvestigatorSpendClues iid n | iid == investigatorId ->
      pure $ a & clues -~ n
    SpendResources iid n | iid == investigatorId ->
      pure $ a & resources %~ max 0 . subtract n
    TakeResources iid n True | iid == investigatorId -> a <$ unshiftMessages
      [ TakeAction iid (actionCost a Action.Resource) (Just Action.Resource)
      , CheckAttackOfOpportunity iid False
      , TakeResources iid n False
      ]
    TakeResources iid n False | iid == investigatorId ->
      pure $ a & resources +~ n
    EmptyDeck iid | iid == investigatorId -> a <$ unshiftMessages
      [ShuffleDiscardBackIn iid, InvestigatorDamage iid EmptyDeckSource 0 1]
    AllDrawEncounterCard | not (a ^. defeated || a ^. resigned) ->
      a <$ unshiftMessage
        (Ask investigatorId
        $ ChooseOne [InvestigatorDrawEncounterCard investigatorId]
        )
    RevelationSkillTest iid source skillType difficulty onSuccess onFailure
      | iid == investigatorId -> a <$ unshiftMessage
        (BeginSkillTest
          iid
          source
          Nothing
          skillType
          difficulty
          onSuccess
          onFailure
          []
          mempty
        )
    ActivateCardAbilityAction iid Ability {..} | iid == investigatorId -> do
      unshiftMessage
        (UseCardAbility iid abilitySource abilityProvider abilityIndex) -- We should check action type when added for aoo
      case abilityType of
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
      shuffled <- liftIO $ shuffleM $ flip map deck' $ \card ->
        if pcWeakness card
          then card { pcBearer = Just $ BearerId $ unInvestigatorId iid }
          else card
      pure $ a & deck .~ Deck shuffled
    BeforeSkillTest iid skillType | iid == investigatorId -> do
      commitedCardIds <- map unCommitedCardId . HashSet.toList <$> asks
        (getSet iid)
      availableAbilities <- getAvailableAbilities a
      let
        filteredAbilities = flip filter availableAbilities $ \Ability {..} ->
          case abilityType of
            FastAbility (WhenSkillTest st) | st == skillType -> True
            _ -> False
        triggerMessage = StartSkillTest investigatorId
        beginMessage = BeforeSkillTest iid skillType
        committableCards = flip filter investigatorHand $ \case
          PlayerCard MkPlayerCard {..} ->
            pcId
              `notElem` commitedCardIds
              && (SkillWild `elem` pcSkills || skillType `elem` pcSkills)
          _ -> False
      if not (null filteredAbilities) || not (null committableCards) || not
        (null commitedCardIds)
      then
        unshiftMessage
          (SkillTestAsk $ Ask iid $ ChooseOne
            (map
                (\Ability {..} -> Run
                  [ UseCardAbility
                    iid
                    abilitySource
                    abilityProvider
                    abilityIndex
                  , beginMessage
                  ]
                )
                filteredAbilities
            <> map
                 (\card ->
                   Run
                     [SkillTestCommitCard iid (getCardId card), beginMessage]
                 )
                 committableCards
            <> map
                 (\cardId ->
                   Run [SkillTestUncommitCard iid cardId, beginMessage]
                 )
                 commitedCardIds
            <> [triggerMessage]
            )
          )
      else
        unshiftMessage (SkillTestAsk $ Ask iid $ ChooseOne [triggerMessage])
      pure a
    BeforeSkillTest iid skillType | iid /= investigatorId -> do
      locationId' <- asks (getId iid)
      when (locationId' == investigatorLocation) $ do
        commitedCardIds <- map unCommitedCardId . HashSet.toList <$> asks
          (getSet investigatorId)
        let
          beginMessage = BeforeSkillTest iid skillType
          committableCards = if not (null commitedCardIds)
            then []
            else flip filter investigatorHand $ \case
              PlayerCard MkPlayerCard {..} ->
                pcId
                  `notElem` commitedCardIds
                  && (SkillWild `elem` pcSkills || skillType `elem` pcSkills)
              _ -> False
        when (not (null committableCards) || not (null commitedCardIds))
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
                     commitedCardIds
                )
              )
      pure a
    InvestigatorStartSkillTest iid maction skillType tempModifiers
      | iid == investigatorId -> a <$ unshiftMessage
        (TriggerSkillTest
          iid
          skillType
          (skillValueFor skillType maction tempModifiers a)
        )
    CheckFastWindow iid windows | iid == investigatorId -> do
      availableAbilities <- getAvailableAbilities a
      let playableCards = filter (fastIsPlayable a windows) investigatorHand
      filteredAbilities <- filterM
        (flip (abilityInWindows windows) a)
        availableAbilities
      if not (null playableCards)
        then a <$ unshiftMessage
          (Ask iid
          $ ChooseOne
          $ map
              (\c -> Run
                [ PayCardCost iid (getCardId c)
                , PlayCard iid (getCardId c) False
                , CheckFastWindow iid windows
                ]
              )
              playableCards
          <> map
               (\Ability {..} -> Run
                 [ UseCardAbility
                   investigatorId
                   abilitySource
                   abilityProvider
                   abilityIndex
                 , CheckFastWindow iid windows
                 ]
               )
               filteredAbilities
          <> [Continue "Skip playing fast cards"]
          )
        else pure a
    LoseAction iid _ | iid == investigatorId ->
      pure $ a & remainingActions %~ max 0 . subtract 1
    GainAction iid _ | iid == investigatorId -> pure $ a & remainingActions +~ 1
    TakeAction iid actionCost' maction | iid == investigatorId -> do
      let
        actionsTakenUpdate = case maction of
          Nothing -> id
          Just action -> (<> [action])
      pure
        $ a
        & (remainingActions %~ max 0 . subtract actionCost')
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
      deck' <- liftIO $ shuffleM $ filter
        ((/= cardId) . getCardId)
        (unDeck investigatorDeck)
      case card of
        MkPlayerCard {..} -> do
          when pcRevelation
            $ unshiftMessage (DrewRevelation iid pcCardCode pcId)
          when (pcCardType == PlayerEnemyType)
            $ unshiftMessage (DrewPlayerEnemy iid pcCardCode pcId)
      pure $ a & deck .~ Deck deck' & hand %~ (PlayerCard card :)
    UnengageEnemy iid eid | iid == investigatorId -> do
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
                `intersection` HashSet.fromList (pcTraits card)
                == traits'
              ]
            )
        unshiftMessage (FocusCards $ map PlayerCard cards)
        pure $ a & deck .~ Deck deck'
    SufferTrauma iid physical mental | iid == investigatorId ->
      pure $ a & physicalTrauma +~ physical & mentalTrauma +~ mental
    GainXP iid amount | iid == investigatorId -> pure $ a & xp +~ amount
    PlayerWindow iid additionalActions | iid == investigatorId -> do
      advanceableActIds <-
        HashSet.toList . HashSet.map unAdvanceableActId <$> asks (getSet ())
      canDos <- filterM (canPerform a) Action.allActionTypes
      locationActions <- asks (getList (investigatorLocation, investigatorId))
      blockedLocationIds <- HashSet.map unBlockedLocationId <$> asks (getSet ())
      allAbilities <- getAvailableAbilities a
      enemyIds <- asks (getSet investigatorLocation)
      aloofEnemyIds <- HashSet.map unAloofEnemyId
        <$> asks (getSet investigatorLocation)
      let
        fightableEnemyIds =
          investigatorEngagedEnemies
            `union` (enemyIds `difference` aloofEnemyIds)
        unengagedEnemyIds = enemyIds `difference` investigatorEngagedEnemies
        accessibleLocations =
          investigatorConnectedLocations `difference` blockedLocationIds
        availableAbilities = flip filter allAbilities $ \Ability {..} ->
          case abilityType of
            FastAbility Any -> canPayAbilityCost abilityCost a
            ActionAbility _ Nothing -> canPayAbilityCost abilityCost a
            ActionAbility _ (Just action) ->
              action `elem` canDos && canPayAbilityCost abilityCost a
            _ -> False
      a <$ unshiftMessage
        (Ask iid $ ChooseOne
          (additionalActions
          <> [ TakeResources iid 1 True | Action.Resource `elem` canDos ]
          <> [ DrawCards iid 1 True | Action.Draw `elem` canDos ]
          <> [ ActivateCardAbilityAction iid ability
             | Action.Ability `elem` canDos
             , ability <- availableAbilities
             ]
          <> [ PlayCard iid (getCardId c) True
             | c <- investigatorHand
             , Action.Play
               `elem` canDos
               || fastIsPlayable a [DuringTurn You] c
             , isPlayable a [DuringTurn You] c
             ]
          <> [ MoveAction iid lid True
             | Action.Move `elem` canDos
             , lid <- HashSet.toList accessibleLocations
             ]
          <> [ FightEnemy iid eid SkillCombat [] mempty True
             | Action.Fight `elem` canDos
             , eid <- HashSet.toList fightableEnemyIds
             ]
          <> [ EngageEnemy iid eid True
             | Action.Engage `elem` canDos
             , eid <- HashSet.toList unengagedEnemyIds
             ]
          <> [ EvadeEnemy iid eid SkillAgility mempty mempty mempty True
             | Action.Evade `elem` canDos
             , eid <- HashSet.toList investigatorEngagedEnemies
             ]
          <> map AdvanceAct advanceableActIds
          <> locationActions
          <> [ChooseEndTurn iid]
          )
        )
    _ -> pure a
