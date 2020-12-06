{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator
  ( getIsPrey
  , baseInvestigator
  , getEngagedEnemies
  , investigatorAttrs
  , hasEndedTurn
  , hasResigned
  , getHasSpendableClues
  , getInvestigatorSpendableClueCount
  , isDefeated
  , actionsRemaining
  , lookupInvestigator
  , getAvailableSkillsFor
  , getSkillValueOf
  , handOf
  , discardOf
  , deckOf
  , locationOf
  , getRemainingHealth
  , getRemainingSanity
  , modifiedStatsOf
  , Investigator
  )
where

import Arkham.Import

import Arkham.Types.Action (Action)
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Cards
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait
import Data.Coerce

data Investigator
  = AgnesBaker' AgnesBaker
  | AshcanPete' AshcanPete
  | DaisyWalker' DaisyWalker
  | DaisyWalkerParallel' DaisyWalkerParallel
  | JennyBarnes' JennyBarnes
  | JimCulver' JimCulver
  | RexMurphy' RexMurphy
  | RolandBanks' RolandBanks
  | SkidsOToole' SkidsOToole
  | WendyAdams' WendyAdams
  | ZoeySamaras' ZoeySamaras
  | BaseInvestigator' BaseInvestigator
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

deriving anyclass instance HasCount AssetCount env (InvestigatorId, [Trait]) => HasModifiersFor env Investigator
deriving anyclass instance HasCount ClueCount env LocationId => HasTokenValue env Investigator

instance Eq Investigator where
  a == b = toId a == toId b

baseInvestigator
  :: InvestigatorId
  -> Text
  -> ClassSymbol
  -> Stats
  -> [Trait]
  -> (Attrs -> Attrs)
  -> Investigator
baseInvestigator a b c d e f =
  BaseInvestigator' . BaseInvestigator . f $ baseAttrs a b c d e

instance IsInvestigator Investigator where
  isDefeated = view defeatedL . investigatorAttrs
  isResigned = view resignedL . investigatorAttrs
  hasEndedTurn = view endedTurnL . investigatorAttrs
  hasResigned = view resignedL . investigatorAttrs

instance HasTokenValue env BaseInvestigator where
  getTokenValue (BaseInvestigator attrs) iid token =
    getTokenValue attrs iid token

newtype BaseInvestigator = BaseInvestigator Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance HasModifiersFor env BaseInvestigator where
  getModifiersFor source target (BaseInvestigator attrs) =
    getModifiersFor source target attrs

instance ActionRunner env => HasActions env BaseInvestigator where
  getActions iid window (BaseInvestigator attrs) = getActions iid window attrs

instance InvestigatorRunner env => RunMessage env BaseInvestigator where
  runMessage msg (BaseInvestigator attrs) =
    BaseInvestigator <$> runMessage msg attrs

instance ActionRunner env => HasActions env Investigator where
  getActions iid window investigator = do
    modifiers' <- getModifiersFor
      (toSource investigator)
      (toTarget investigator)
      ()
    if any isBlank modifiers'
      then getActions iid window (investigatorAttrs investigator)
      else defaultGetActions iid window investigator

instance InvestigatorRunner env => RunMessage env Investigator where
  runMessage msg@(ResolveToken _ _ iid) i | iid == toId i = do
    modifiers' <- getModifiersFor (toSource i) (toTarget i) ()
    if any isBlank modifiers' then pure i else defaultRunMessage msg i
  runMessage msg i = defaultRunMessage msg i

instance HasId InvestigatorId () Investigator where
  getId = pure . toId

instance HasList DiscardedPlayerCard env Investigator where
  getList =
    pure . map DiscardedPlayerCard . investigatorDiscard . investigatorAttrs

instance HasList HandCard env Investigator where
  getList = pure . map HandCard . investigatorHand . investigatorAttrs

instance HasCard () Investigator where
  getCard _ cardId =
    fromJustNote "player does not have this card"
      . find ((== cardId) . getCardId)
      . investigatorHand
      . investigatorAttrs

instance HasCardCode Investigator where
  getCardCode = getCardCode . investigatorAttrs

instance HasDamage Investigator where
  getDamage i = (investigatorHealthDamage, investigatorSanityDamage)
    where Attrs {..} = investigatorAttrs i

instance HasTrauma Investigator where
  getTrauma i = (investigatorPhysicalTrauma, investigatorMentalTrauma)
    where Attrs {..} = investigatorAttrs i

instance HasSet EnemyId env Investigator where
  getSet = pure . investigatorEngagedEnemies . investigatorAttrs

instance HasSet TreacheryId env Investigator where
  getSet = pure . investigatorTreacheries . investigatorAttrs

instance HasList DiscardableHandCard env Investigator where
  getList =
    pure
      . map DiscardableHandCard
      . filter (not . isWeakness)
      . investigatorHand
      . investigatorAttrs
   where
    isWeakness = \case
      PlayerCard pc -> pcWeakness pc
      EncounterCard _ -> True -- maybe?

instance HasCount ActionTakenCount env Investigator where
  getCount =
    pure
      . ActionTakenCount
      . length
      . investigatorActionsTaken
      . investigatorAttrs

instance HasCount ActionRemainingCount env (Maybe Action, [Trait], Investigator) where
  getCount (_maction, traits, i) =
    let
      tomeActionCount = if Tome `elem` traits
        then fromMaybe 0 (investigatorTomeActions a)
        else 0
    in
      pure
      . ActionRemainingCount
      $ investigatorRemainingActions a
      + tomeActionCount
    where a = investigatorAttrs i

instance HasSet EnemyId env Investigator => HasCount EnemyCount env Investigator where
  getCount = (EnemyCount . length <$>) . getSet @EnemyId

instance HasCount ResourceCount env Investigator where
  getCount = pure . ResourceCount . investigatorResources . investigatorAttrs

instance HasCount DiscardCount env Investigator where
  getCount =
    pure . DiscardCount . length . investigatorDiscard . investigatorAttrs

instance HasCount CardCount env Investigator where
  getCount = pure . CardCount . length . investigatorHand . investigatorAttrs

instance HasCount ClueCount env Investigator where
  getCount = pure . ClueCount . investigatorClues . investigatorAttrs

getInvestigatorSpendableClueCount
  :: (MonadReader env m, HasModifiersFor env ())
  => Investigator
  -> m SpendableClueCount
getInvestigatorSpendableClueCount =
  (SpendableClueCount <$>) . getSpendableClueCount . investigatorAttrs

instance HasSet AssetId env Investigator where
  getSet = pure . investigatorAssets . investigatorAttrs

instance HasSkill Investigator where
  getSkill skillType = skillValueFor skillType Nothing [] . investigatorAttrs

allInvestigators :: HashMap InvestigatorId Investigator
allInvestigators = mapFromList $ map
  (toFst $ investigatorId . investigatorAttrs)
  [ AgnesBaker' agnesBaker
  , AshcanPete' ashcanPete
  , DaisyWalker' daisyWalker
  , DaisyWalkerParallel' daisyWalkerParallel
  , JennyBarnes' jennyBarnes
  , JimCulver' jimCulver
  , RexMurphy' rexMurphy
  , RolandBanks' rolandBanks
  , SkidsOToole' skidsOToole
  , WendyAdams' wendyAdams
  , ZoeySamaras' zoeySamaras
  ]

lookupInvestigator :: InvestigatorId -> Investigator
lookupInvestigator iid =
  fromMaybe (lookupPromoInvestigator iid) $ lookup iid allInvestigators

-- | Handle promo investigators
--
-- Some investigators have book versions that are just alternative art
-- with some replacement cards. Since these investigators are functionally
-- the same, we proxy the lookup to their non-promo version.
--
-- Parallel investigators will need to be handled differently since they
-- are not functionally the same.
--
lookupPromoInvestigator :: InvestigatorId -> Investigator
lookupPromoInvestigator "98001" = lookupInvestigator "02003" -- Jenny Barnes
lookupPromoInvestigator "98004" = lookupInvestigator "01001" -- Roland Banks
lookupPromoInvestigator iid = error $ "Unknown investigator: " <> show iid

getEngagedEnemies :: Investigator -> HashSet EnemyId
getEngagedEnemies = investigatorEngagedEnemies . investigatorAttrs

-- TODO: This does not work for more than 2 players
getIsPrey
  :: ( MonadReader env m
     , HasList (InvestigatorId, Distance) env EnemyTrait
     , HasSet CardCount env ()
     , HasSet ClueCount env ()
     , HasSet RemainingSanity env ()
     , HasSet RemainingHealth env ()
     , HasSet Int env SkillType -- hmmm
     , HasModifiersFor env ()
     )
  => Prey
  -> Investigator
  -> m Bool
getIsPrey (OnlyPrey prey) i = getIsPrey prey i
getIsPrey AnyPrey _ = pure True
getIsPrey (HighestSkill skillType) i = do
  highestSkill <- fromMaybe 0 . maximumMay <$> getSetList skillType
  pure $ highestSkill == skillValueFor
    skillType
    Nothing
    []
    (investigatorAttrs i)
getIsPrey (LowestSkill skillType) i = do
  lowestSkillValue <- fromMaybe 100 . minimumMay <$> getSetList skillType
  pure $ lowestSkillValue == skillValueFor
    skillType
    Nothing
    []
    (investigatorAttrs i)
getIsPrey LowestRemainingHealth i = do
  remainingHealth <- getRemainingHealth i
  lowestRemainingHealth <-
    fromJustNote "has to be"
    . minimumMay
    . map unRemainingHealth
    <$> getSetList ()
  pure $ lowestRemainingHealth == remainingHealth
getIsPrey LowestRemainingSanity i = do
  remainingSanity <- getRemainingSanity i
  lowestRemainingSanity <-
    fromJustNote "has to be"
    . minimumMay
    . map unRemainingSanity
    <$> getSetList ()
  pure $ lowestRemainingSanity == remainingSanity
getIsPrey (Bearer bid) i = pure $ unBearerId bid == unInvestigatorId
  (investigatorId $ investigatorAttrs i)
getIsPrey MostClues i = do
  clueCount <- unClueCount <$> getCount i
  mostClueCount <- fromMaybe 0 . maximumMay . map unClueCount <$> getSetList ()
  pure $ mostClueCount == clueCount
getIsPrey FewestCards i = do
  cardCount <- unCardCount <$> getCount i
  minCardCount <- fromMaybe 100 . minimumMay . map unCardCount <$> getSetList ()
  pure $ minCardCount == cardCount
getIsPrey (NearestToEnemyWithTrait trait) i = do
  mappings :: [(InvestigatorId, Distance)] <- getList (EnemyTrait trait)
  let
    mappingsMap :: HashMap InvestigatorId Distance = mapFromList mappings
    minDistance :: Int =
      fromJustNote "error" . minimumMay $ map (unDistance . snd) mappings
    investigatorDistance :: Int = unDistance $ findWithDefault
      (error "investigator not found")
      (investigatorId $ investigatorAttrs i)
      mappingsMap
  pure $ investigatorDistance == minDistance
getIsPrey SetToBearer _ = error "The bearer was not correctly set"

getAvailableSkillsFor
  :: (MonadReader env m, HasModifiersFor env ())
  => Investigator
  -> SkillType
  -> m [SkillType]
getAvailableSkillsFor i s = getPossibleSkillTypeChoices s (investigatorAttrs i)

getSkillValueOf
  :: (MonadReader env m, HasModifiersFor env ())
  => SkillType
  -> Investigator
  -> m Int
getSkillValueOf skillType i = do
  modifiers' <- getModifiersFor (toSource i) (toTarget i) ()
  let
    mBaseValue = foldr
      (\modifier current -> case modifier of
        BaseSkillOf stype n | stype == skillType -> Just n
        _ -> current
      )
      Nothing
      modifiers'
  pure $ fromMaybe (skillValueOf skillType i) mBaseValue

skillValueOf :: SkillType -> Investigator -> Int
skillValueOf SkillWillpower = investigatorWillpower . investigatorAttrs
skillValueOf SkillIntellect = investigatorIntellect . investigatorAttrs
skillValueOf SkillCombat = investigatorCombat . investigatorAttrs
skillValueOf SkillAgility = investigatorAgility . investigatorAttrs
skillValueOf SkillWild = error "should not look this up"

handOf :: Investigator -> [Card]
handOf = investigatorHand . investigatorAttrs

discardOf :: Investigator -> [PlayerCard]
discardOf = investigatorDiscard . investigatorAttrs

deckOf :: Investigator -> [PlayerCard]
deckOf = unDeck . investigatorDeck . investigatorAttrs

locationOf :: Investigator -> LocationId
locationOf = investigatorLocation . investigatorAttrs

getRemainingSanity
  :: (MonadReader env m, HasModifiersFor env ()) => Investigator -> m Int
getRemainingSanity i = do
  modifiedSanity <- getModifiedSanity a
  pure $ modifiedSanity - investigatorSanityDamage a
  where a = investigatorAttrs i

getRemainingHealth
  :: (MonadReader env m, HasModifiersFor env ()) => Investigator -> m Int
getRemainingHealth i = do
  modifiedHealth <- getModifiedHealth a
  pure $ modifiedHealth - investigatorHealthDamage a
  where a = investigatorAttrs i

instance Entity Investigator where
  type EntityId Investigator = InvestigatorId
  toId = toId . investigatorAttrs
  toSource = toSource . investigatorAttrs
  toTarget = toTarget . investigatorAttrs
  isSource = isSource . investigatorAttrs
  isTarget = isTarget . investigatorAttrs

modifiedStatsOf
  :: (MonadReader env m, HasModifiersFor env ())
  => Source
  -> Maybe Action
  -> Investigator
  -> m Stats
modifiedStatsOf source maction i = do
  modifiers' <- getModifiersFor source (toTarget i) ()
  remainingHealth <- getRemainingHealth i
  remainingSanity <- getRemainingSanity i
  let
    a = investigatorAttrs i
    willpower' = skillValueFor SkillWillpower maction modifiers' a
    intellect' = skillValueFor SkillIntellect maction modifiers' a
    combat' = skillValueFor SkillCombat maction modifiers' a
    agility' = skillValueFor SkillAgility maction modifiers' a
  pure Stats
    { willpower = willpower'
    , intellect = intellect'
    , combat = combat'
    , agility = agility'
    , health = remainingHealth
    , sanity = remainingSanity
    }

getHasSpendableClues
  :: (MonadReader env m, HasModifiersFor env ()) => Investigator -> m Bool
getHasSpendableClues i = (> 0) <$> getSpendableClueCount (investigatorAttrs i)

actionsRemaining :: Investigator -> Int
actionsRemaining = investigatorRemainingActions . investigatorAttrs

investigatorAttrs :: Investigator -> Attrs
investigatorAttrs = \case
  AgnesBaker' attrs -> coerce attrs
  AshcanPete' attrs -> coerce attrs
  DaisyWalker' attrs -> coerce attrs
  DaisyWalkerParallel' attrs -> coerce attrs
  JennyBarnes' attrs -> coerce attrs
  JimCulver' attrs -> coerce attrs
  RexMurphy' attrs -> coerce attrs
  RolandBanks' attrs -> coerce attrs
  SkidsOToole' attrs -> coerce attrs
  WendyAdams' attrs -> coerce attrs
  ZoeySamaras' attrs -> coerce attrs
  BaseInvestigator' attrs -> coerce attrs
