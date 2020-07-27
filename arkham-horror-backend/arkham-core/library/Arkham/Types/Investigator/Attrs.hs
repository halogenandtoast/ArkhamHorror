{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Attrs where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.Action (Action)
import qualified Arkham.Types.Action as Action
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.FastWindow
import Arkham.Types.Helpers
import Arkham.Types.Investigator.Runner
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Stats
import Arkham.Types.Trait
import Arkham.Types.TreacheryId
import ClassyPrelude
import Data.Coerce
import qualified Data.HashSet as HashSet
import Lens.Micro
import System.Random.Shuffle

instance HasCardCode Attrs where
  getCardCode = unInvestigatorId . investigatorId

data Attrs = Attrs
  { investigatorName :: Text
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
  , investigatorModifiers :: [Modifier]
  , investigatorAbilities :: [Ability]
  , investigatorDefeated :: Bool
  , investigatorResigned :: Bool
  }
  deriving stock (Show, Generic)

instance ToJSON Attrs where
  toJSON = genericToJSON $ aesonOptions $ Just "investigator"
  toEncoding = genericToEncoding $ aesonOptions $ Just "investigator"

instance FromJSON Attrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "investigator"

locationId :: Lens' Attrs LocationId
locationId = lens investigatorLocation $ \m x -> m { investigatorLocation = x }

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
facingDefeat Attrs {..} =
  investigatorHealthDamage
    >= investigatorHealth
    || investigatorSanityDamage
    >= investigatorSanity

skillValueFor :: SkillType -> [Modifier] -> Attrs -> Int
skillValueFor skill tempModifiers attrs = foldr
  applyModifier
  baseSkillValue
  (investigatorModifiers attrs <> tempModifiers)
 where
  applyModifier (SkillModifier skillType m _) n =
    if skillType == skill then max 0 (n + m) else n
  applyModifier _ n = n
  baseSkillValue = case skill of
    SkillWillpower -> investigatorWillpower attrs
    SkillIntellect -> investigatorIntellect attrs
    SkillCombat -> investigatorCombat attrs
    SkillAgility -> investigatorAgility attrs
    SkillWild -> error "investigators do not have wild skills"

damageValueFor :: [Modifier] -> Attrs -> Int
damageValueFor tempModifiers attrs = foldr
  applyModifier
  1
  (investigatorModifiers attrs <> tempModifiers)
 where
  applyModifier (DamageDealt m _) n = max 0 (n + m)
  applyModifier _ n = n

baseAttrs :: InvestigatorId -> Text -> Stats -> [Trait] -> Attrs
baseAttrs iid name Stats {..} traits = Attrs
  { investigatorName = name
  , investigatorId = iid
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
  }

sourceIsInvestigator :: Source -> Attrs -> Bool
sourceIsInvestigator source Attrs {..} = case source of
  InvestigatorSource sourceId -> sourceId == investigatorId
  AssetSource sourceId -> sourceId `elem` investigatorAssets
  _ -> False

matchTarget :: Attrs -> ActionTarget -> Action -> Bool
matchTarget attrs (FirstOneOf as) action =
  action `elem` as && action `notElem` investigatorActionsTaken attrs
matchTarget _ (IsAction a) action = action == a

actionCost :: Attrs -> Action -> Int
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

canAfford :: Attrs -> Action -> Bool
canAfford a@Attrs {..} actionType =
  actionCost a actionType <= investigatorRemainingActions

canPerform
  :: (MonadReader env m, InvestigatorRunner env) => Attrs -> Action -> m Bool
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
  filteredAbilities <- flip filterM availableAbilities $ \case
    (_, _, ActionAbility action, _) -> canPerform a action
    (_, _, FreeAbility (SkillTestWindow _), _) -> pure False
    (_, _, ReactionAbility _, _) -> pure False

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
 where
  none f = not . any f
  prevents (CannotPlay types _) = pcCardType `elem` types
  prevents _ = False

takeAction :: Action -> Attrs -> Attrs
takeAction action a =
  a
    & (remainingActions -~ actionCost a action)
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
  canPerformAbility (_, _, ActionAbility actionType, _) =
    canAfford a actionType
  canPerformAbility (_, _, FreeAbility (SkillTestWindow _), _) = False
  canPerformAbility (_, _, ReactionAbility _, _) = False

drawOpeningHand :: Attrs -> Int -> ([PlayerCard], [Card], [PlayerCard])
drawOpeningHand a n = go n (a ^. discard, a ^. hand, coerce (a ^. deck))
 where
  go 0 (d, h, cs) = (d, h, cs)
  go _ (_, _, []) =
    error "this should never happen, it means the deck was empty during drawing"
  go m (d, h, (c : cs)) = if pcWeakness c
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
abilityInWindows windows ability _ = case ability of
  (_, _, ReactionAbility window, OncePerRound) -> if window `elem` windows
    then do
      usedAbilities <- map unUsedAbility <$> asks (getList ())
      pure $ ability `notElem` usedAbilities
    else pure False
  (_, _, ReactionAbility window, _) -> pure $ window `elem` windows
  _ -> pure False

instance (InvestigatorRunner env) => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    Setup -> do
      let (discard', hand', deck') = drawOpeningHand a 5
      unshiftMessage (ShuffleDiscardBackIn investigatorId)
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
    EnemySpawn lid eid | lid == investigatorLocation -> do
      aloofEnemyIds <- HashSet.map unAloofEnemyId
        <$> asks (getSet investigatorLocation)
      when (eid `notElem` aloofEnemyIds)
        $ unshiftMessage (EnemyEngageInvestigator eid investigatorId)
      pure a
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
      (Ask $ ChooseOne $ map
        (ChoiceResult . DiscardAsset)
        (HashSet.toList $ a ^. assets)
      )
    AttachTreacheryToInvestigator tid iid | iid == investigatorId ->
      pure $ a & treacheries %~ HashSet.insert tid
    RemoveCardFromHand iid cardCode | iid == investigatorId ->
      pure $ a & hand %~ filter ((/= cardCode) . getCardCode)
    DiscardTreachery tid | tid `elem` investigatorTreacheries ->
      pure $ a & treacheries %~ HashSet.delete tid
    AssetDiscarded aid cardCode | aid `elem` investigatorAssets ->
      pure
        $ a
        & (assets %~ HashSet.delete aid)
        & (discard %~ (lookupPlayerCard cardCode :))
    ChooseActivateCardAbilityAction iid | iid == investigatorId -> do
      availableAbilities <- getAvailableAbilities a
      a <$ unshiftMessage
        (Ask $ ChooseOne $ map
          (ChoiceResult . ActivateCardAbilityAction iid)
          availableAbilities
        )
    ChooseFightEnemyAction iid skillType tempModifiers
      | iid == investigatorId -> do
        unshiftMessage
          (Ask $ ChooseOne $ map
            (\eid -> ChoiceResult $ FightEnemy iid eid skillType tempModifiers)
            (HashSet.toList investigatorEngagedEnemies)
          )
        pure $ takeAction Action.Fight a
    FightEnemy iid eid skillType tempModifiers | iid == investigatorId -> do
      unshiftMessages
        [ WhenAttackEnemy iid eid
        , AttackEnemy iid eid skillType (damageValueFor tempModifiers a)
        , AfterAttackEnemy iid eid
        ]
      pure a
    EnemyEvaded iid eid | iid == investigatorId ->
      pure $ a & engagedEnemies %~ HashSet.delete eid
    ChooseEvadeEnemyAction iid | iid == investigatorId -> do
      unshiftMessage
        (Ask $ ChooseOne $ map
          (\eid -> ChoiceResults
            [ WhenEvadeEnemy iid eid
            , EvadeEnemy iid eid SkillAgility
            , AfterEvadeEnemy iid eid
            ]
          )
          (HashSet.toList investigatorEngagedEnemies)
        )
      pure $ takeAction Action.Fight a
    ChooseMoveAction iid | iid == investigatorId -> do
      blockedLocationIds <- HashSet.map unBlockedLocationId <$> asks (getSet ())
      let
        accessibleLocations =
          investigatorConnectedLocations `difference` blockedLocationIds
      a <$ unshiftMessage
        (Ask $ ChooseOne $ map
          (\l -> ChoiceResults [CheckAttackOfOpportunity iid, MoveAction iid l])
          (HashSet.toList accessibleLocations)
        )
    MoveAction iid l | iid == investigatorId -> do
      unshiftMessage (MoveTo iid l)
      pure $ takeAction Action.Move a
    InvestigatorAssignDamage iid eid health sanity | iid == investigatorId -> do
      allDamageableAssets <-
        HashSet.toList . HashSet.map unDamageableAssetId <$> asks (getSet iid)
      a <$ unshiftMessage
        (Ask $ ChooseOne
          (ChoiceResult
              (InvestigatorDamage investigatorId (EnemySource eid) health sanity
              )
          : map
              (\k -> ChoiceResult $ AssetDamage k eid health sanity)
              allDamageableAssets
          )
        )
    ChooseInvestigateAction iid skillType tempModifiers
      | iid == investigatorId -> do
        unshiftMessages
          [ CheckAttackOfOpportunity iid
          , Investigate skillType iid investigatorLocation tempModifiers
          ]
        pure $ takeAction Action.Investigate a
    InvestigatorDiscoverClues iid lid n | iid == investigatorId ->
      a <$ unshiftMessage
        (DiscoverCluesAtLocation iid lid (cluesToDiscover a n))
    DiscoverClues iid lid n | iid == investigatorId -> do
      availableAbilities <- getAvailableAbilities a
      filteredAbilities <- filterM
        (flip (abilityInWindows [WhenDiscoverClues You YourLocation]) a)
        availableAbilities
      a <$ unshiftMessage
        (Ask
        $ ChooseOne
        $ map
            (\ability -> ChoiceResults
              [UseCardAbility iid ability, DiscoverClues iid lid n]
            )
            filteredAbilities
        <> [ChoiceResult (AfterDiscoverClues iid lid n)]
        )
    AfterDiscoverClues iid _ n | iid == investigatorId -> pure $ a & clues +~ n
    PayCardCost iid _ n | iid == investigatorId -> do
      let cost = getCost $ a ^?! hand . ix n
      pure $ a & resources -~ cost
    InvestigatorPlayCard iid _ n | iid == investigatorId ->
      pure $ a & hand %~ without n
    SkillTestCommitCard iid (n, _) | iid == investigatorId ->
      pure $ a & hand %~ without n
    InvestigatorPlayAsset iid aid | iid == investigatorId ->
      pure $ a & assets %~ HashSet.insert aid
    InvestigatorDamage iid _ health sanity | iid == investigatorId -> do
      let a' = a & healthDamage +~ health & sanityDamage +~ sanity
      if facingDefeat a'
        then a' <$ unshiftMessage (InvestigatorWhenDefeated iid)
        else pure a'
    InvestigatorWhenDefeated iid | iid == investigatorId -> do
      unshiftMessage (InvestigatorDefeated iid)
      pure $ a & defeated .~ True
    MoveAllTo lid -> a <$ unshiftMessage (MoveTo investigatorId lid)
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
    InvestigatorAddModifier iid modifier | iid == investigatorId ->
      pure $ a & modifiers %~ (modifier :)
    InvestigatorRemoveAllModifiersFromSource iid source
      | iid == investigatorId -> pure $ a & modifiers %~ filter
        ((source /=) . sourceOfModifier)
    ChooseEndTurn iid | iid == investigatorId -> pure $ a & endedTurn .~ True
    BeginRound ->
      pure
        $ a
        & (endedTurn .~ False)
        & (remainingActions .~ 3)
        & (actionsTaken .~ mempty)
    ChooseDrawCardAction iid | iid == investigatorId -> do
      unshiftMessages [CheckAttackOfOpportunity iid, DrawCards iid 1]
      pure $ takeAction Action.Draw a
    DrawCards iid n | iid == investigatorId -> if null (unDeck investigatorDeck)
      then if null investigatorDiscard
        then pure a
        else a <$ unshiftMessages [ShuffleDiscardBackIn iid, DrawCards iid n]
      else do
        let
          (mcard, deck') = drawCard (coerce investigatorDeck)
          handUpdate = maybe id ((:) . PlayerCard) mcard
        case mcard of
          Just MkPlayerCard {..} -> when (pcCardType == PlayerTreacheryType)
            $ unshiftMessage (DrewPlayerTreachery iid pcCardCode)
          Nothing -> pure ()
        pure $ a & hand %~ handUpdate & deck .~ Deck deck'
    ChooseTakeResourceAction iid | iid == investigatorId -> do
      unshiftMessages [CheckAttackOfOpportunity iid, TakeResources iid 1]
      pure $ takeAction Action.Resource a
    ChoosePlayCardAction iid | iid == investigatorId -> do
      unshiftMessage
        (Ask $ ChooseOne $ map
          (\(i, c) -> ChoiceResults
            [ PayCardCost iid (getCardCode c) i
            , CheckAttackOfOpportunity iid
            , InvestigatorPlayCard iid (getCardCode c) i
            ]
          )
          (filter (isPlayable a [DuringTurn You] . snd)
          $ zip [0 ..] investigatorHand
          )
        )
      pure $ takeAction Action.Play a
    InvestigatorSpendClues iid n | iid == investigatorId ->
      pure $ a & clues -~ n
    SpendResources iid n | iid == investigatorId ->
      pure $ a & resources -~ n & resources %~ max 0
    TakeResources iid n | iid == investigatorId -> pure $ a & resources +~ n
    EmptyDeck iid | iid == investigatorId -> a <$ unshiftMessages
      [ShuffleDiscardBackIn iid, InvestigatorDamage iid EmptyDeckSource 0 1]
    AllDrawEncounterCard ->
      a <$ unshiftMessage (InvestigatorDrawEncounterCard investigatorId)
    RevelationSkillTest iid skillType difficulty onSuccess onFailure ->
      a <$ unshiftMessage
        (BeginSkillTest iid skillType difficulty onSuccess onFailure)
    ActivateCardAbilityAction iid ability@(_, _, abilityType, _)
      | iid == investigatorId -> do
        unshiftMessage (UseCardAbility iid ability) -- We should check action type when added for aoo
        case abilityType of
          ActionAbility actionType ->
            if actionType
                `notElem` [ Action.Fight
                          , Action.Evade
                          , Action.Resign
                          , Action.Parley
                          ]
              then unshiftMessage (CheckAttackOfOpportunity iid)
              else pure ()
          _ -> pure ()
        pure a
    AllDrawCardAndResource -> do
      let
        (mcard, deck') = drawCard (coerce investigatorDeck)
        handUpdate = maybe id ((:) . PlayerCard) mcard
      when (null deck') $ unshiftMessage (EmptyDeck investigatorId)
      pure $ a & resources +~ 1 & hand %~ handUpdate & deck .~ Deck deck'
    LoadDeck iid deck' | iid == investigatorId -> do
      shuffled <- liftIO $ shuffleM deck'
      pure $ a & deck .~ Deck shuffled
    BeforeSkillTest iid skillType | iid == investigatorId -> do
      availableAbilities <- getAvailableAbilities a
      let
        filteredAbilities = flip filter availableAbilities $ \case
          (_, _, FreeAbility (SkillTestWindow st), _) | st == skillType -> True
          _ -> False
        triggerMessage = StartSkillTest
        beginMessage = BeforeSkillTest iid skillType
        committableCards =
          flip filter (zip [0 ..] investigatorHand) $ \(_, c) -> case c of
            PlayerCard MkPlayerCard {..} ->
              SkillWild `elem` pcSkills || skillType `elem` pcSkills
            _ -> False
      if not (null filteredAbilities) || not (null committableCards)
        then unshiftMessage
          (Ask $ ChooseOne
            (map
                (\ability ->
                  ChoiceResults [UseCardAbility iid ability, beginMessage]
                )
                filteredAbilities
            <> map
                 (\card -> ChoiceResults
                   [SkillTestCommitCard iid card, beginMessage]
                 )
                 committableCards
            <> [ChoiceResult triggerMessage]
            )
          )
        else unshiftMessage triggerMessage
      pure a
    InvestigatorStartSkillTest iid skillType tempModifiers ->
      a <$ unshiftMessage
        (TriggerSkillTest
          iid
          skillType
          (skillValueFor skillType tempModifiers a)
        )
    CheckFastWindow iid windows | iid == investigatorId -> do
      availableAbilities <- getAvailableAbilities a
      let
        playableCards =
          filter (fastIsPlayable a windows . snd) (zip [0 ..] investigatorHand)
      filteredAbilities <- filterM
        (flip (abilityInWindows windows) a)
        availableAbilities
      if not (null playableCards)
        then a <$ unshiftMessage
          (Ask
          $ ChooseOne
          $ map
              (\(i, c) -> ChoiceResults
                [ PayCardCost iid (getCardCode c) i
                , InvestigatorPlayCard iid (getCardCode c) i
                , CheckFastWindow iid windows
                ]
              )
              playableCards
          <> map
               (\ability -> ChoiceResults
                 [ UseCardAbility investigatorId ability
                 , CheckFastWindow iid windows
                 ]
               )
               filteredAbilities
          <> [ChoiceResults []]
          )
        else pure a
    PlayerWindow iid | iid == investigatorId -> do
      advanceableActIds <-
        HashSet.toList . HashSet.map unAdvanceableActId <$> asks (getSet ())
      canDos <- filterM (canPerform a) Action.allActions
      a <$ unshiftMessage
        (Ask $ ChooseOne $ map
          ChoiceResult
          ([ ChooseTakeResourceAction iid | Action.Resource `elem` canDos ]
          <> [ ChooseDrawCardAction iid | Action.Draw `elem` canDos ]
          <> [ ChooseActivateCardAbilityAction iid
             | Action.Ability `elem` canDos
             ]
          <> [ ChoosePlayCardAction iid | Action.Play `elem` canDos ]
          <> [ ChooseMoveAction iid | Action.Move `elem` canDos ]
          <> [ ChooseInvestigateAction iid SkillIntellect []
             | Action.Investigate `elem` canDos
             ]
          <> [ ChooseFightEnemyAction iid SkillCombat []
             | Action.Fight `elem` canDos
             ]
          <> [ ChooseEngageEnemyAction iid | Action.Engage `elem` canDos ]
          <> [ ChooseEvadeEnemyAction iid | Action.Evade `elem` canDos ]
          <> map AdvanceAct advanceableActIds
          <> [ChooseEndTurn iid]
          )
        )
    _ -> pure a
