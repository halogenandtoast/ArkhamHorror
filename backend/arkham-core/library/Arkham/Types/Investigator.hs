module Arkham.Types.Investigator
  ( module Arkham.Types.Investigator
  )
where

import Arkham.Prelude

import Arkham.Types.Action (Action, TakenAction)
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.EnemyId
import Arkham.Types.Helpers
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Cards
import Arkham.Types.Investigator.Runner
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Stats
import Arkham.Types.Trait
import Arkham.Types.TreacheryId

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
  -> (InvestigatorAttrs -> InvestigatorAttrs)
  -> Investigator
baseInvestigator a b c d e f =
  BaseInvestigator' . BaseInvestigator . f $ baseAttrs a b c d e

instance IsInvestigator Investigator where
  isDefeated = view defeatedL . toAttrs
  isResigned = view resignedL . toAttrs
  hasEndedTurn = view endedTurnL . toAttrs
  hasResigned = view resignedL . toAttrs

instance {-# OVERLAPS #-} HasTraits Investigator where
  toTraits = toTraits . toAttrs

instance HasTokenValue env BaseInvestigator where
  getTokenValue (BaseInvestigator attrs) iid token =
    getTokenValue attrs iid token

newtype BaseInvestigator = BaseInvestigator InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env BaseInvestigator where
  getModifiersFor source target (BaseInvestigator attrs) =
    getModifiersFor source target attrs

instance InvestigatorRunner env => HasActions env BaseInvestigator where
  getActions iid window (BaseInvestigator attrs) = getActions iid window attrs

instance InvestigatorRunner env => RunMessage env BaseInvestigator where
  runMessage msg (BaseInvestigator attrs) =
    BaseInvestigator <$> runMessage msg attrs

instance InvestigatorRunner env => HasActions env Investigator where
  getActions iid window investigator = do
    modifiers' <- getModifiersFor
      (toSource investigator)
      (toTarget investigator)
      ()
    if any isBlank modifiers'
      then getActions iid window (toAttrs investigator)
      else defaultGetActions iid window investigator

instance InvestigatorRunner env => RunMessage env Investigator where
  runMessage msg i = do
    modifiers' <- getModifiersFor (toSource i) (toTarget i) ()
    let msg' = if any isBlank modifiers' then Blanked msg else msg
    defaultRunMessage msg' i

instance HasId InvestigatorId () Investigator where
  getId = pure . toId

instance HasList TakenAction env Investigator where
  getList = getList . toAttrs

instance HasList DiscardedPlayerCard env Investigator where
  getList = pure . map DiscardedPlayerCard . investigatorDiscard . toAttrs

instance HasList HandCard env Investigator where
  getList = pure . map HandCard . investigatorHand . toAttrs

instance HasList DeckCard env Investigator where
  getList = pure . map DeckCard . unDeck . investigatorDeck . toAttrs

instance HasCard Investigator () where
  getCard cardId _ =
    asks
      $ fromJustNote "player does not have this card"
      . find ((== cardId) . toCardId)
      . investigatorHand
      . toAttrs

instance HasDamage Investigator where
  getDamage i = (investigatorHealthDamage, investigatorSanityDamage)
    where InvestigatorAttrs {..} = toAttrs i

instance HasTrauma Investigator where
  getTrauma i = (investigatorPhysicalTrauma, investigatorMentalTrauma)
    where InvestigatorAttrs {..} = toAttrs i

instance HasSet EnemyId env Investigator where
  getSet = pure . investigatorEngagedEnemies . toAttrs

instance HasSet TreacheryId env Investigator where
  getSet = pure . investigatorTreacheries . toAttrs

instance HasList DiscardableHandCard env Investigator where
  getList =
    pure
      . map DiscardableHandCard
      . filter (not . isWeakness)
      . investigatorHand
      . toAttrs
   where
    isWeakness = \case
      PlayerCard pc -> cdWeakness (pcDef pc)
      EncounterCard _ -> True -- maybe?

instance HasCount MentalTraumaCount env Investigator where
  getCount = pure . MentalTraumaCount . investigatorMentalTrauma . toAttrs

instance HasCount ActionTakenCount env Investigator where
  getCount =
    pure . ActionTakenCount . length . investigatorActionsTaken . toAttrs

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
    where a = toAttrs i

instance HasCount ActionRemainingCount env Investigator where
  getCount = getCount . toAttrs

instance HasCount EnemyCount env Investigator where
  getCount = (EnemyCount . length <$>) . getSet @EnemyId

instance HasCount ResourceCount env Investigator where
  getCount = pure . ResourceCount . investigatorResources . toAttrs

instance HasCount DiscardCount env Investigator where
  getCount = pure . DiscardCount . length . investigatorDiscard . toAttrs

instance HasCount CardCount env Investigator where
  getCount = pure . CardCount . length . investigatorHand . toAttrs

instance HasCount ClueCount env Investigator where
  getCount = pure . ClueCount . investigatorClues . toAttrs

instance HasCount DamageCount env Investigator where
  getCount = pure . DamageCount . investigatorHealthDamage . toAttrs

getInvestigatorSpendableClueCount
  :: (MonadReader env m, HasModifiersFor env ())
  => Investigator
  -> m SpendableClueCount
getInvestigatorSpendableClueCount =
  (SpendableClueCount <$>) . getSpendableClueCount . toAttrs

instance HasSet AssetId env Investigator where
  getSet = pure . investigatorAssets . toAttrs

instance HasSkill Investigator where
  getSkill skillType = skillValueFor skillType Nothing [] . toAttrs

allInvestigators :: HashMap InvestigatorId Investigator
allInvestigators = mapFromList $ map
  (toFst $ investigatorId . toAttrs)
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
getEngagedEnemies = investigatorEngagedEnemies . toAttrs

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
  pure $ highestSkill == skillValueFor skillType Nothing [] (toAttrs i)
getIsPrey (LowestSkill skillType) i = do
  lowestSkillValue <- fromMaybe 100 . minimumMay <$> getSetList skillType
  pure $ lowestSkillValue == skillValueFor skillType Nothing [] (toAttrs i)
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
getIsPrey (Bearer bid) i =
  pure $ unBearerId bid == toId i
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
      (investigatorId $ toAttrs i)
      mappingsMap
  pure $ investigatorDistance == minDistance
getIsPrey SetToBearer _ = error "The bearer was not correctly set"

getAvailableSkillsFor
  :: (MonadReader env m, HasModifiersFor env ())
  => Investigator
  -> SkillType
  -> m [SkillType]
getAvailableSkillsFor i s = getPossibleSkillTypeChoices s (toAttrs i)

getSkillValueOf
  :: (MonadReader env m, HasModifiersFor env ())
  => SkillType
  -> Investigator
  -> m Int
getSkillValueOf skillType i = do
  modifiers' <-
    map modifierType <$> getModifiersFor (toSource i) (toTarget i) ()
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
skillValueOf SkillWillpower = investigatorWillpower . toAttrs
skillValueOf SkillIntellect = investigatorIntellect . toAttrs
skillValueOf SkillCombat = investigatorCombat . toAttrs
skillValueOf SkillAgility = investigatorAgility . toAttrs
skillValueOf SkillWild = error "should not look this up"

handOf :: Investigator -> [Card]
handOf = investigatorHand . toAttrs

discardOf :: Investigator -> [PlayerCard]
discardOf = investigatorDiscard . toAttrs

deckOf :: Investigator -> [PlayerCard]
deckOf = unDeck . investigatorDeck . toAttrs

locationOf :: Investigator -> LocationId
locationOf = investigatorLocation . toAttrs

getRemainingSanity
  :: (MonadReader env m, HasModifiersFor env ()) => Investigator -> m Int
getRemainingSanity i = do
  modifiedSanity <- getModifiedSanity a
  pure $ modifiedSanity - investigatorSanityDamage a
  where a = toAttrs i

getRemainingHealth
  :: (MonadReader env m, HasModifiersFor env ()) => Investigator -> m Int
getRemainingHealth i = do
  modifiedHealth <- getModifiedHealth a
  pure $ modifiedHealth - investigatorHealthDamage a
  where a = toAttrs i

instance Entity Investigator where
  type EntityId Investigator = InvestigatorId
  type EntityAttrs Investigator = InvestigatorAttrs

instance NamedEntity Investigator where
  toName = toName . toAttrs

instance TargetEntity Investigator where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Investigator where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

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
    a = toAttrs i
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
getHasSpendableClues i = (> 0) <$> getSpendableClueCount (toAttrs i)

actionsRemaining :: Investigator -> Int
actionsRemaining = investigatorRemainingActions . toAttrs
