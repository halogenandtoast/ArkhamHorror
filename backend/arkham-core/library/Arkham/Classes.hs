{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
module Arkham.Classes
  ( module Arkham.Classes
  , module X
  ) where

import Arkham.Prelude hiding (to)

import Arkham.Ability
import Arkham.Card
import Arkham.Action hiding (Ability)
import Arkham.Card
import Arkham.Card.Id
import Arkham.Classes.Entity as X
import Arkham.Classes.GameLogger as X
import Arkham.Classes.HasQueue as X
import Arkham.Classes.HasRecord as X
import Arkham.Classes.HasTokenValue as X
import Arkham.Classes.RunMessage as X
import Arkham.History
import Arkham.Id
import Arkham.Keyword
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Name
import Arkham.Phase
import Arkham.Query
import Arkham.SkillType
import Arkham.Source
import Arkham.Stats
import Arkham.Target
import Arkham.Trait
import Data.Char qualified as C
import Data.HashSet qualified as HashSet
import GHC.Generics
import Language.Haskell.TH.Syntax hiding (Name)
import Language.Haskell.TH.Syntax qualified as TH

newtype Distance = Distance { unDistance :: Int }
  deriving newtype (Ord, Eq)

class HasPhase env where
  getPhase :: MonadReader env m => m Phase

class HasStep step env a where
  getStep :: MonadReader env m => a -> m step

class HasHistory env where
  getHistory :: MonadReader env m => HistoryType -> InvestigatorId -> m History

class (Hashable set, Eq set) => HasSet set env a where
  getSet :: (HasCallStack, MonadReader env m) => a -> m (HashSet set)
  getSetList :: (HasCallStack, MonadReader env m) => a -> m [set]
  getSetList a = setToList <$> getSet a

getSetListMap
  :: (HasSet set env a, MonadReader env m) => (set -> set') -> a -> m [set']
getSetListMap f a = map f <$> getSetList a

type family QueryElement a where
  QueryElement AssetMatcher = AssetId
  QueryElement InvestigatorMatcher = InvestigatorId
  QueryElement LocationMatcher = LocationId
  QueryElement EnemyMatcher = EnemyId
  QueryElement TreacheryMatcher = TreacheryId
  QueryElement ExtendedCardMatcher = Card
  QueryElement AbilityMatcher = Ability
  QueryElement SkillMatcher = SkillId
  QueryElement EventMatcher = EventId
  QueryElement ActMatcher = ActId

selectCount :: (HasCallStack, MonadReader env m, Query a env) => a -> m Int
selectCount = fmap HashSet.size . select

selectAny :: (HasCallStack, MonadReader env m, Query a env) => a -> m Bool
selectAny = fmap notNull . selectListMap id

selectList
  :: (HasCallStack, MonadReader env m, Query a env) => a -> m [QueryElement a]
selectList = selectListMap id

selectRandom
  :: (HasCallStack, MonadRandom m, MonadReader env m, Query a env)
  => a
  -> m (Maybe (QueryElement a))
selectRandom matcher = do
  results <- selectList matcher
  maybe (pure Nothing) (fmap Just . sample) (nonEmpty results)

selectListMap
  :: (HasCallStack, MonadReader env m, Query a env)
  => (QueryElement a -> b)
  -> a
  -> m [b]
selectListMap f = fmap (map f . setToList) . select

selectJust
  :: (HasCallStack, Show a, MonadReader env m, Query a env)
  => a
  -> m (QueryElement a)
selectJust matcher = fromJustNote errorNote <$> selectOne matcher
  where errorNote = "Could not find any matches for: " <> show matcher

-- | Get a set aside card
--
-- Some cards may be double sided and completely different types
-- like Daniel Chesterfield. In these cases, we want to consider
-- the card a match, but "flip" it to the correct side.
--
-- This logic is a bit too generous and we may want to specify
-- on double sided cards which card code is on the other side.
getSetAsideCard
  :: (HasCallStack, MonadReader env m, Query ExtendedCardMatcher env)
  => CardDef
  -> m Card
getSetAsideCard def = do
  card <- selectJust . SetAsideCardMatch $ cardIs def
  pure $ if cardCodeExactEq (toCardCode card) (toCardCode def)
    then card
    else lookupCard (toCardCode def) (toCardId card)

getSetAsideEncounterCard
  :: (HasCallStack, MonadReader env m, Query ExtendedCardMatcher env)
  => CardDef
  -> m EncounterCard
getSetAsideEncounterCard =
  fmap (fromJustNote "must be encounter card" . preview _EncounterCard)
    . getSetAsideCard

getSetAsideCardsMatching
  :: (HasCallStack, MonadReader env m, Query ExtendedCardMatcher env)
  => CardMatcher
  -> m [Card]
getSetAsideCardsMatching = selectList . SetAsideCardMatch

selectOne
  :: (HasCallStack, MonadReader env m, Query a env)
  => a
  -> m (Maybe (QueryElement a))
selectOne matcher = do
  result <- selectList matcher
  pure $ case result of
    [] -> Nothing
    x : _ -> Just x

class (Hashable (QueryElement a), Eq (QueryElement a)) => Query a env where
  select :: (HasCallStack, MonadReader env m) => a -> m (HashSet (QueryElement a))

class HasList list env a where
  getList :: MonadReader env m => a -> m [list]

class HasId id env a where
  getId :: MonadReader env m => a -> m id

class HasCount count env a where
  getCount :: MonadReader env m => a -> m count

class HasName env a where
  getName :: MonadReader env m => a -> m Name

class HasPlayerCard env a where
  getPlayerCard :: MonadReader env m => a -> m (Maybe PlayerCard)

type HasCostPayment env
  = ( HasCount ActionRemainingCount env (Maybe Action, [Trait], InvestigatorId)
    , HasCount PlayerCount env ()
    , HasCount ResourceCount env InvestigatorId
    , HasCount SpendableClueCount env ()
    , HasCount SpendableClueCount env InvestigatorId
    , HasCount UsesCount env AssetId
    , HasList InPlayCard env InvestigatorId
    , HasId (Maybe LocationId) env LocationMatcher
    , HasList HandCard env InvestigatorId
    , HasList TakenAction env InvestigatorId
    , Query AssetMatcher env
    , Query InvestigatorMatcher env
    , Query ExtendedCardMatcher env
    , HasSet InvestigatorId env LocationId
    )

class HasStats env a where
  getStats :: MonadReader env m => a -> Source -> m Stats

class HasSkillValue env a where
  getSkillValue :: (HasModifiersFor env (), MonadReader env m) => SkillType -> a -> m Int

class HasVictoryPoints a where
  getVictoryPoints :: a -> Maybe Int

class HasDamage a where
  getDamage :: a -> (Int, Int)

class HasTrauma a where
  getTrauma :: a -> (Int, Int)

instance HasVictoryPoints Card where
  getVictoryPoints (PlayerCard card) = getVictoryPoints card
  getVictoryPoints (EncounterCard card) = getVictoryPoints card

instance HasVictoryPoints EncounterCard where
  getVictoryPoints = cdVictoryPoints . toCardDef

instance HasVictoryPoints PlayerCard where
  getVictoryPoints = cdVictoryPoints . toCardDef

type ActionRunner env
  = ( HasTokenValue env ()
    , HasSet LocationId env LocationMatcher
    , HasSet TreacheryId env LocationId
    , HasSet FarthestLocationId env (InvestigatorId, LocationMatcher)
    , HasList TakenAction env InvestigatorId
    , Query AssetMatcher env
    , Query LocationMatcher env
    , GetCardDef env EnemyId
    , HasCostPayment env
    , ( HasCount
          ActionRemainingCount
          env
          (Maybe Action, [Trait], InvestigatorId)
      , HasCount ActionRemainingCount env InvestigatorId
      , HasCount ActionTakenCount env InvestigatorId
      , HasCount AssetCount env (InvestigatorId, [Trait])
      , HasCount CardCount env InvestigatorId
      , HasCount ClueCount env InvestigatorId
      , HasCount ClueCount env LocationId
      , HasCount DamageCount env InvestigatorId
      , HasCount DoomCount env AssetId
      , HasCount DoomCount env InvestigatorId
      , HasCount HorrorCount env InvestigatorId
      , HasCount SetAsideCount env CardCode
      )
    , HasId (Maybe LocationId) env AssetId
    , HasId (Maybe OwnerId) env AssetId
    , HasId CardCode env EnemyId
    , HasId LeadInvestigatorId env ()
    , HasId LocationId env EnemyId
    , HasId LocationId env InvestigatorId
    , HasList CommittedCard env InvestigatorId
    , HasList CommittedSkillIcon env InvestigatorId
    , HasList DiscardedPlayerCard env InvestigatorId
    , HasList InPlayCard env InvestigatorId
    , HasList UnderneathCard env InvestigatorId
    , HasList UsedAbility env ()
    , HasModifiersFor env ()
    , HasSet AccessibleLocationId env LocationId
    , HasSet CommittedCardId env InvestigatorId
    , HasSet ConnectedLocationId env LocationId
    , HasSet EnemyId env ([Trait], LocationId)
    , HasSet EnemyId env CardCode
    , HasSet EnemyId env EnemyMatcher
    , HasSet EnemyId env InvestigatorId
    , HasSet EnemyId env LocationId
    , HasSet FightableEnemyId env (InvestigatorId, Source)
    , HasSet InvestigatorId env ()
    , HasSet InvestigatorId env (HashSet LocationId)
    , HasSet InvestigatorId env EnemyId
    , HasSet Keyword env EnemyId
    , HasSet LocationId env ()
    , HasSet LocationId env [Trait]
    , HasSet Trait env (InvestigatorId, CardId)
    , HasSet Trait env EnemyId
    , HasSet Trait env LocationId
    , HasSet Trait env Source
    , HasStep ActStep env ()
    )

class HasAbilities1 f where
  getAbilities1 :: f p -> [Ability]

instance HasAbilities1 f => HasAbilities1 (M1 i c f) where
  getAbilities1 (M1 x) = getAbilities1 x

instance (HasAbilities1 l, HasAbilities1 r) => HasAbilities1 (l :+: r) where
  getAbilities1 (L1 x) = getAbilities1 x
  getAbilities1 (R1 x) = getAbilities1 x

instance (HasAbilities p) => HasAbilities1 (K1 R p) where
  getAbilities1 (K1 x) = getAbilities x

genericGetAbilities :: (Generic a, HasAbilities1 (Rep a)) => a -> [Ability]
genericGetAbilities = getAbilities1 . from

class HasAbilities a where
  getAbilities :: a -> [Ability]
  getAbilities = const []

class HasModifiersFor1 env f where
  getModifiersFor1 :: (HasCallStack, MonadReader env m) => Source -> Target -> f p -> m [Modifier]

instance HasModifiersFor1 env f => HasModifiersFor1 env (M1 i c f) where
  getModifiersFor1 source target (M1 x) = getModifiersFor1 source target x

instance (HasModifiersFor1 env l, HasModifiersFor1 env r) => HasModifiersFor1 env (l :+: r) where
  getModifiersFor1 source target (L1 x) = getModifiersFor1 source target x
  getModifiersFor1 source target (R1 x) = getModifiersFor1 source target x

instance (HasModifiersFor env p) => HasModifiersFor1 env (K1 R p) where
  getModifiersFor1 source target (K1 x) = getModifiersFor source target x

genericGetModifiersFor
  :: (HasCallStack, Generic a, HasModifiersFor1 env (Rep a), MonadReader env m)
  => Source
  -> Target
  -> a
  -> m [Modifier]
genericGetModifiersFor source target = getModifiersFor1 source target . from

getModifiers
  :: (HasModifiersFor env (), MonadReader env m)
  => Source
  -> Target
  -> m [ModifierType]
getModifiers source target =
  map modifierType <$> getModifiersFor source target ()

class HasModifiersFor env a where
  getModifiersFor :: (HasCallStack, MonadReader env m) => Source -> Target -> a -> m [Modifier]
  getModifiersFor _ _ _ = pure []

class Discardable a where
  canBeDiscarded :: a -> Bool

class CanBeWeakness env a where
  getIsWeakness :: MonadReader env m => a -> m Bool

class Exhaustable a where
  isExhausted :: a -> Bool
  isReady :: a -> Bool

  isExhausted = not . isReady
  isReady = not . isExhausted
  {-# MINIMAL isExhausted | isReady #-}

buildEntity :: String -> Q [Dec]
buildEntity nm = do
  ClassI _ instances <- reify (TH.mkName $ "Is" ++ nm)
  let conz = mapMaybe extractCon instances
  pure
    [ DataD
        []
        (TH.mkName nm)
        []
        Nothing
        conz
        [ DerivClause (Just StockStrategy) (map ConT [''Show, ''Generic, ''Eq])
        , DerivClause (Just AnyclassStrategy) (map ConT [''ToJSON, ''FromJSON])
        ]
    ]
 where
  extractCon (InstanceD _ _ (AppT _ con@(ConT name)) _) = Just $ NormalC
    (TH.mkName $ nameBase name ++ "'")
    [(Bang TH.NoSourceUnpackedness TH.NoSourceStrictness, con)]
  extractCon _ = Nothing

buildEntityLookupList :: String -> Q Exp
buildEntityLookupList nm = do
  ClassI _ instances <- reify (TH.mkName $ "Is" ++ nm)
  let conz = mapMaybe extractCon instances
  pure $ ListE conz
 where
  extractCon (InstanceD _ _ (AppT _ (ConT name)) _) = Just $ AppE
    (AppE (VarE $ TH.mkName "fmap") (ConE $ TH.mkName $ nameBase name ++ "'"))
    (VarE $ toFunName $ nameBase name)
  extractCon _ = Nothing
  toFunName [] = TH.mkName ""
  toFunName (x : xs) = TH.mkName $ C.toLower x : xs
