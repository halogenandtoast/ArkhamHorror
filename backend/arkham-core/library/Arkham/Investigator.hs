module Arkham.Investigator
  ( module Arkham.Investigator
  ) where

import Arkham.Prelude

import Arkham.Action (Action, TakenAction)
import Arkham.SkillType
import Arkham.Card
import Arkham.Helpers
import Arkham.Id
import Arkham.Investigator.Investigators
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Query
import Arkham.Slot
import Arkham.Source

data Investigator = Investigator
  { investigatorAttrs :: InvestigatorAttrs
  , investigatorBehaviors :: InvestigatorBehaviors
  }

instance ToJSON Investigator where
  toJSON = toJSON . investigatorAttrs

instance FromJSON Investigator where
  parseJSON o = do
    attrs <- parseJSON o
    let mbehaviors = lookupInvestigatorBehaviors (investigatorCardCode attrs)
    case mbehaviors of
      Nothing -> error "Could not find behaviors for investigator"
      Just behaviors -> pure $ Investigator
        { investigatorAttrs = attrs
        , investigatorBehaviors = behaviors
        }

instance Show Investigator where
  show = show . investigatorAttrs

instance Eq Investigator where
  a == b = toId a == toId b

lookupInvestigatorBehaviors :: CardCode -> Maybe InvestigatorBehaviors
lookupInvestigatorBehaviors _ = Just defaultInvestigatorBehaviors

instance HasModifiersFor env Investigator where
  getModifiersFor source target i = (ibGetModifiersFor $ investigatorBehaviors i) source target (investigatorAttrs i)

instance HasTokenValue env Investigator where
  getTokenValue iid tFace i = (ibGetTokenValue $ investigatorBehaviors i) iid tFace (investigatorAttrs i)

isEliminated :: Investigator -> Bool
isEliminated = uncurry (||) . (isResigned &&& isDefeated)

isDefeated :: Investigator -> Bool
isDefeated = view defeatedL . toAttrs

isResigned :: Investigator -> Bool
isResigned = view resignedL . toAttrs

hasEndedTurn :: Investigator -> Bool
hasEndedTurn = view endedTurnL . toAttrs

hasResigned :: Investigator -> Bool
hasResigned = view resignedL . toAttrs

instance {-# OVERLAPPING #-} HasTraits Investigator where
  toTraits = toTraits . toAttrs

instance HasAbilities Investigator where
  getAbilities i = (ibGetAbilities $ investigatorBehaviors i) (investigatorAttrs i)

instance InvestigatorRunner env => RunMessage env Investigator where
  runMessage msg i = do
    modifiers' <- getModifiers (toSource i) (toTarget i)
    let msg' = if Blank `elem` modifiers' then Blanked msg else msg
    attrs <- (ibRunMessage $ investigatorBehaviors i) msg' (investigatorAttrs i)
    pure $ i { investigatorAttrs = attrs }

instance HasId InvestigatorId () Investigator where
  getId = pure . toId

instance HasList TakenAction env Investigator where
  getList = getList . toAttrs

instance HasList DiscardedPlayerCard env Investigator where
  getList = pure . map DiscardedPlayerCard . investigatorDiscard . toAttrs

instance HasList HandCard env Investigator where
  getList = pure . map HandCard . investigatorHand . toAttrs

instance HasModifiersFor env () => HasList PlayableHandCard env Investigator where
  getList i = do
    asIfInHandCards <- getAsIfInHandCards (toAttrs i)
    pure
      . map PlayableHandCard
      . (<> asIfInHandCards)
      . investigatorHand
      $ toAttrs i

instance HasList UnderneathCard env Investigator where
  getList = pure . map UnderneathCard . investigatorCardsUnderneath . toAttrs

instance HasList DeckCard env Investigator where
  getList = pure . map DeckCard . unDeck . investigatorDeck . toAttrs

instance HasCard Investigator () where
  getCard cardId _ =
    asks
      $ fromJustNote "player does not have this card"
      . find ((== cardId) . toCardId)
      . allCards
      . toAttrs
   where
     allCards a = investigatorHand a <> map PlayerCard (unDeck $ investigatorDeck a)

instance HasDamage Investigator where
  getDamage i = (investigatorHealthDamage, investigatorSanityDamage)
    where InvestigatorAttrs {..} = toAttrs i

instance HasTrauma Investigator where
  getTrauma i = (investigatorPhysicalTrauma, investigatorMentalTrauma)
    where InvestigatorAttrs {..} = toAttrs i

instance HasSet ClassSymbol env Investigator where
  getSet = pure . singleton . investigatorClass . toAttrs

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
      PlayerCard pc   -> isJust $ cdCardSubType $ toCardDef pc
      EncounterCard _ -> True -- maybe?

instance HasCount MentalTraumaCount env Investigator where
  getCount = pure . MentalTraumaCount . investigatorMentalTrauma . toAttrs

instance HasModifiersFor env () => HasCount DoomCount env Investigator where
  getCount i = do
    modifiers <- getModifiers (toSource i) (toTarget i)
    let f = if DoomSubtracts `elem` modifiers then negate else id
    pure . DoomCount . f . investigatorDoom $ toAttrs i

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

instance HasCount ResourceCount env Investigator where
  getCount = pure . ResourceCount . investigatorResources . toAttrs

instance HasCount DiscardCount env Investigator where
  getCount = pure . DiscardCount . length . investigatorDiscard . toAttrs

instance HasCount CardCount env Investigator where
  getCount = pure . CardCount . length . investigatorHand . toAttrs

instance HasCount ClueCount env Investigator where
  getCount = pure . ClueCount . investigatorClues . toAttrs

instance HasCount HorrorCount env Investigator where
  getCount = pure . HorrorCount . investigatorSanityDamage . toAttrs

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

instance HasSkillValue env Investigator where
  getSkillValue skillType i = do
    modifiers' <- getModifiers (toSource i) (toTarget i)
    let base = skillValueFor skillType Nothing [] (toAttrs i)
    pure $ foldl' applyModifier base modifiers'
   where
    applyModifier _ (BaseSkillOf skillType' n) | skillType == skillType' = n
    applyModifier n _ = n

allInvestigators :: HashMap InvestigatorId Investigator
allInvestigators = mapFromList $ map
  (InvestigatorId . cbCardCode &&& convert . ($ ()) . cbCardBuilder)
  [rolandBanks]
 where
   convert a = Investigator
    { investigatorAttrs = toAttrs a
    , investigatorBehaviors = defaultInvestigatorBehaviors
    }

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
lookupPromoInvestigator iid     = error $ "Unknown investigator: " <> show iid

getEngagedEnemies :: Investigator -> HashSet EnemyId
getEngagedEnemies = investigatorEngagedEnemies . toAttrs

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
  modifiers' <- getModifiers (toSource i) (toTarget i)
  let
    mBaseValue = foldr
      (\modifier current -> case modifier of
        BaseSkillOf stype n | stype == skillType -> Just n
        _                                        -> current
      )
      Nothing
      modifiers'
  pure $ fromMaybe (skillValueOf skillType i) mBaseValue

skillValueOf :: SkillType -> Investigator -> Int
skillValueOf SkillWillpower = investigatorWillpower . toAttrs
skillValueOf SkillIntellect = investigatorIntellect . toAttrs
skillValueOf SkillCombat    = investigatorCombat . toAttrs
skillValueOf SkillAgility   = investigatorAgility . toAttrs
skillValueOf SkillWild      = error "should not look this up"

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
  toId = toId . toAttrs
  toAttrs = investigatorAttrs

instance HasName env Investigator where
  getName = pure . toName

instance Named Investigator where
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
  modifiers' <- getModifiers source (toTarget i)
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

getPotentialSlots
  :: (MonadReader env m, Query AssetMatcher env)
  => HashSet Trait
  -> Investigator
  -> m [PotentialSlot]
getPotentialSlots traits i = do
  let
    slots :: [(SlotType, Slot)] =
      concatMap (\(slotType, slots') -> map (slotType, ) slots')
        . mapToList
        . investigatorSlots
        $ toAttrs i
  map (PotentialSlot . fst)
    <$> filterM
          (\(_, slot) -> do
            let
              passesRestriction = case slot of
                           TraitRestrictedSlot _ t _ -> t `member` traits
                           Slot{}                    -> True

             in if passesRestriction
                  then case slotItem slot of
                    Nothing  -> pure True
                    Just aid -> member aid <$> select DiscardableAsset
                  else pure False
          )
          slots

instance ToGameLoggerFormat Investigator where
  format = format . toAttrs
