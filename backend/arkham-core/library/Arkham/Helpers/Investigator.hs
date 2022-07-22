module Arkham.Helpers.Investigator where

import Arkham.Prelude

import Data.UUID (nil)
import Arkham.Action
import Arkham.Card
import Arkham.Card.Id
import Arkham.Classes.Entity
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest
import Arkham.Helpers.Slot
import Arkham.Id
import Arkham.SkillTest.Base
import Arkham.Investigator.Attrs
import Arkham.Treachery.Attrs ( Field (..) )
import Arkham.Matcher hiding (InvestigatorDefeated)
import Arkham.Projection
import Arkham.SkillType
import Arkham.Source
import Arkham.Stats
import Arkham.Target
import Control.Monad.Extra (orM)
import Data.Foldable (foldrM)

getSkillValue :: (Monad m, HasGame m) => SkillType -> InvestigatorId -> m Int
getSkillValue st iid = case st of
  SkillWillpower -> field InvestigatorWillpower iid
  SkillIntellect -> field InvestigatorIntellect iid
  SkillCombat -> field InvestigatorCombat iid
  SkillAgility -> field InvestigatorAgility iid
  SkillWild -> error "no wild skill"

skillValueFor
  :: (Monad m, HasGame m) => SkillType -> Maybe Action -> [ModifierType] -> InvestigatorId -> m Int
skillValueFor skill maction tempModifiers iid = do
  base <- baseSkillValueFor skill maction tempModifiers iid
  foldrM applyModifier base tempModifiers
 where
  canBeIncreased = SkillCannotBeIncreased skill `notElem` tempModifiers
  applyModifier (AnySkillValue m) n | canBeIncreased || m < 0 = pure $ max 0 (n + m)
  applyModifier (AddSkillValue sv) n | canBeIncreased = do
    m <- getSkillValue sv iid
    pure $ max 0 (n + m)
  applyModifier (SkillModifier skillType m) n | canBeIncreased || m < 0 =
    pure $ if skillType == skill then max 0 (n + m) else n
  applyModifier (ActionSkillModifier action skillType m) n
    | canBeIncreased || m < 0 = pure $ if skillType == skill && Just action == maction
      then max 0 (n + m)
      else n
  applyModifier _ n = pure n

baseSkillValueFor
  :: (Monad m, HasGame m) => SkillType -> Maybe Action -> [ModifierType] -> InvestigatorId -> m Int
baseSkillValueFor skill _maction tempModifiers iid = do
  baseValue <- getSkillValue skill iid
  pure $ foldr applyModifier baseValue tempModifiers
 where
  applyModifier (BaseSkillOf skillType m) _ | skillType == skill = m
  applyModifier _ n = n

damageValueFor :: (Monad m, HasGame m) => Int -> InvestigatorAttrs -> m Int
damageValueFor baseValue attrs = do
  source <- fromJustNote "damage outside skill test" <$> getSkillTestSource
  modifiers <- getModifiers source (InvestigatorTarget $ investigatorId attrs)
  pure $ foldr applyModifier baseValue modifiers
 where
  applyModifier (DamageDealt m) n = max 0 (n + m)
  applyModifier NoDamageDealt _ = 0
  applyModifier _ n = n

getIsScenarioAbility :: (Monad m, HasGame m) => m Bool
getIsScenarioAbility = do
  source <- fromJustNote "damage outside skill test" <$> getSkillTestSource
  case source of
    SkillTestSource _ _ source' _ -> case source' of
      EnemySource _ -> pure True
      AgendaSource _ -> pure True
      LocationSource _ -> pure True
      TreacherySource tid ->
        -- If treachery has a subtype then it is a weakness not an encounter card
        isNothing . cdCardSubType <$> field TreacheryCardDef tid
      ActSource _ -> pure True
      _ -> pure False
    _ -> pure False

getHandSize :: (Monad m, HasGame m) => InvestigatorAttrs -> m Int
getHandSize attrs = do
  source <- fromMaybe (toSource attrs) <$> getSkillTestSource
  modifiers <- getModifiers source (InvestigatorTarget $ investigatorId attrs)
  pure $ foldr applyModifier 8 modifiers
 where
  applyModifier (HandSize m) n = max 0 (n + m)
  applyModifier _ n = n

getInHandCount :: (Monad m, HasGame m) => InvestigatorAttrs -> m Int
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

getAbilitiesForTurn :: (Monad m, HasGame m) => InvestigatorAttrs -> m Int
getAbilitiesForTurn attrs = do
  modifiers <- getModifiers (toSource attrs) (toTarget attrs)
  pure $ foldr applyModifier 3 modifiers
 where
  applyModifier (AdditionalActions m) n = max 0 (n + m)
  applyModifier _ n = n

getCanDiscoverClues :: (Monad m, HasGame m) => InvestigatorAttrs -> m Bool
getCanDiscoverClues attrs = do
  modifiers <- getModifiers (toSource attrs) (toTarget attrs)
  pure $ not (any match modifiers)
 where
  match CannotDiscoverClues{} = True
  match _ = False

getCanSpendClues :: (Monad m, HasGame m) => InvestigatorAttrs -> m Bool
getCanSpendClues attrs = do
  modifiers <- getModifiers (toSource attrs) (toTarget attrs)
  pure $ not (any match modifiers)
 where
  match CannotSpendClues{} = True
  match _ = False

removeFromSlots
  :: AssetId -> HashMap SlotType [Slot] -> HashMap SlotType [Slot]
removeFromSlots aid = fmap (map (removeIfMatches aid))

fitsAvailableSlots :: IsCard a => [SlotType] -> a -> InvestigatorAttrs -> Bool
fitsAvailableSlots slotTypes cardDef a = null
  (slotTypes \\ concatMap
    (\slotType -> availableSlotTypesFor slotType cardDef a)
    (nub slotTypes)
  )

availableSlotTypesFor :: IsCard a => SlotType -> a -> InvestigatorAttrs -> [SlotType]
availableSlotTypesFor slotType a attrs = case lookup slotType (attrs ^. slotsL) of
  Nothing -> []
  Just slots ->
    replicate (length (filter (canPutIntoSlot a) slots)) slotType

placeInAvailableSlot :: IsCard a => AssetId -> a -> [Slot] -> [Slot]
placeInAvailableSlot _ _ [] = []
placeInAvailableSlot aid card (x : xs) = if canPutIntoSlot card x
  then putIntoSlot aid x : xs
  else x : placeInAvailableSlot aid card xs

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
          . headMay . setToList $ cdClassSymbols cardDef
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
        , investigatorAdditionalActions = []
        , investigatorHorrorHealed = 0
        }
      }

matchTarget :: InvestigatorAttrs -> ActionTarget -> Action -> Bool
matchTarget attrs (FirstOneOf as) action =
  action `elem` as && all (`notElem` investigatorActionsTaken attrs) as
matchTarget _ (IsAction a) action = action == a
matchTarget _ (EnemyAction a _) action = action == a

getActionCost :: (Monad m, HasGame m) => InvestigatorAttrs -> [Action] -> m Int
getActionCost attrs as = do
  modifiers <- getModifiers (toSource attrs) (toTarget attrs)
  pure $ foldr applyModifier 1 modifiers
 where
  applyModifier (ActionCostOf match m) n =
    if any (matchTarget attrs match) as then n + m else n
  applyModifier _ n = n

getSpendableClueCount :: (Monad m, HasGame m) => InvestigatorAttrs -> m Int
getSpendableClueCount a = do
  canSpendClues <- getCanSpendClues a
  pure $ if canSpendClues then investigatorClues a else 0

getCanAfford :: (Monad m, HasGame m) => InvestigatorAttrs -> [Action] -> m Bool
getCanAfford a@InvestigatorAttrs {..} as = do
  actionCost <- getActionCost a as
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

canCommitToAnotherLocation :: (Monad m, HasGame m) => InvestigatorAttrs -> m Bool
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

findCard :: HasCallStack => CardId -> InvestigatorAttrs -> Card
findCard cardId a =
  fromJustNote "not in hand or discard or deck"
    $ findMatch
    $ (a ^. handL)
    <> map PlayerCard (a ^. discardL)
    <> map PlayerCard (unDeck $ a ^. deckL)
  where findMatch = find ((== cardId) . toCardId)

getJustLocation :: (HasCallStack, Monad m, HasGame m) => InvestigatorId -> m LocationId
getJustLocation =
  fieldMap InvestigatorLocation (fromJustNote "must be at a location")

enemiesColocatedWith :: InvestigatorId -> EnemyMatcher
enemiesColocatedWith = EnemyAt . LocationWithInvestigator . InvestigatorWithId

modifiedStatsOf
  :: (Monad m, HasGame m) => Source
  -> Maybe Action
  -> InvestigatorId
  -> m Stats
modifiedStatsOf source maction i = do
  modifiers' <- getModifiers source (InvestigatorTarget i)
  remainingHealth <- field InvestigatorRemainingHealth i
  remainingSanity <- field InvestigatorRemainingSanity i
  willpower' <- skillValueFor SkillWillpower maction modifiers' i
  intellect' <- skillValueFor SkillIntellect maction modifiers' i
  combat' <- skillValueFor SkillCombat maction modifiers' i
  agility' <- skillValueFor SkillAgility maction modifiers' i
  pure Stats
    { willpower = willpower'
    , intellect = intellect'
    , combat = combat'
    , agility = agility'
    , health = remainingHealth
    , sanity = remainingSanity
    }

getAvailableSkillsFor :: (Monad m, HasGame m) => SkillType -> InvestigatorId -> m [SkillType]
getAvailableSkillsFor skillType iid = do
  modifiers <- getModifiers (InvestigatorSource iid) (InvestigatorTarget iid)
  pure $ foldr applyModifier [skillType] modifiers
 where
  applyModifier (UseSkillInPlaceOf toReplace toUse) skills
    | toReplace == skillType = toUse : skills
  applyModifier _ skills = skills

isEliminated :: (Monad m, HasGame m) => InvestigatorId -> m Bool
isEliminated iid = orM $ sequence [field InvestigatorResigned, field InvestigatorDefeated] iid
