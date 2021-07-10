
module Arkham.Types.Classes
  ( module Arkham.Types.Classes
  , module X
  ) where

import Arkham.Prelude hiding (to)

import Arkham.Card
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.Action hiding (Ability)
import Arkham.Types.Asset.Uses (UseType)
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Classes.Entity as X
import Arkham.Types.Classes.HasQueue as X
import Arkham.Types.Classes.HasRecord as X
import Arkham.Types.Classes.HasTokenValue as X
import Arkham.Types.Classes.RunMessage as X
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.Keyword
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Phase
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Stats
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Window (Window)
import GHC.Generics

newtype Distance = Distance { unDistance :: Int }

checkWindows
  :: (MonadIO m, HasQueue env, MonadReader env m, HasSet InvestigatorId env ())
  => [Window]
  -> m ()
checkWindows windows = do
  investigatorIds <- getSetList ()
  pushAll [ CheckWindow iid windows | iid <- investigatorIds ]

class HasPhase env where
  getPhase :: MonadReader env m => m Phase

class HasStep env a where
  getStep :: MonadReader env m => m a

class HasRoundHistory env where
  getRoundHistory :: MonadReader env m => m [Message]

class HasPhaseHistory env where
  getPhaseHistory :: MonadReader env m => m [Message]

class (Hashable set, Eq set) => HasSet set env a where
  getSet :: (HasCallStack, MonadReader env m) => a -> m (HashSet set)
  getSetList :: (HasCallStack, MonadReader env m) => a -> m [set]
  getSetList a = setToList <$> getSet a

class HasList list env a where
  getList :: MonadReader env m => a -> m [list]

class HasId id env a where
  getId :: MonadReader env m => a -> m id

getLocationIdWithTitle
  :: (MonadReader env m, HasId (Maybe LocationId) env LocationMatcher)
  => Text
  -> m (Maybe LocationId)
getLocationIdWithTitle = getId . LocationWithTitle

class HasCount count env a where
  getCount :: MonadReader env m => a -> m count

class HasName env a where
  getName :: MonadReader env m => a -> m Name

class HasPlayerCard env a where
  getPlayerCard :: MonadReader env m => a -> m (Maybe PlayerCard)

type HasCostPayment env
  = ( HasCount SpendableClueCount env InvestigatorId
    , HasCount SpendableClueCount env ()
    , HasCount ActionRemainingCount env (Maybe Action, [Trait], InvestigatorId)
    , HasCount PlayerCount env ()
    , HasList HandCard env InvestigatorId
    , HasCount ResourceCount env InvestigatorId
    , HasCount UsesCount env AssetId
    , HasSet ExhaustedAssetId env ()
    , HasSet InvestigatorId env LocationId
    , HasId (Maybe LocationId) env LocationMatcher
    )

class HasStats env a where
  getStats :: MonadReader env m => a -> Source -> m Stats

class HasSkillValue a where
  toSkillValue :: SkillType -> a -> Int

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
  getVictoryPoints MkEncounterCard {..} = cdVictoryPoints ecDef

instance HasVictoryPoints PlayerCard where
  getVictoryPoints MkPlayerCard {..} = cdVictoryPoints pcDef

type ActionRunner env
  = ( HasQueue env
    , GetActions env
    , HasCount AssetCount env (InvestigatorId, [Trait])
    , HasCount ActionRemainingCount env InvestigatorId
    , HasCount ActionRemainingCount env (Maybe Action, [Trait], InvestigatorId)
    , HasCount ActionTakenCount env InvestigatorId
    , HasCount CardCount env InvestigatorId
    , HasCount ClueCount env LocationId
    , HasCount ClueCount env InvestigatorId
    , HasCount HorrorCount env InvestigatorId
    , HasCount PlayerCount env ()
    , HasCount ResourceCount env InvestigatorId
    , HasCount SetAsideCount env CardCode
    , HasCount SpendableClueCount env InvestigatorId
    , HasCount SpendableClueCount env ()
    , HasCount UsesCount env AssetId
    , HasId (Maybe LocationId) env AssetId
    , HasId (Maybe LocationId) env LocationMatcher
    , HasId (Maybe OwnerId) env AssetId
    , HasId (Maybe StoryAssetId) env CardCode
    , HasId (Maybe StoryEnemyId) env CardCode
    , HasId CardCode env EnemyId
    , HasId LeadInvestigatorId env ()
    , HasId LocationId env InvestigatorId
    , HasId LocationId env EnemyId
    , HasList DiscardedPlayerCard env InvestigatorId
    , HasList HandCard env InvestigatorId
    , HasList InPlayCard env InvestigatorId
    , HasList UsedAbility env ()
    , HasModifiersFor env ()
    , HasSet AccessibleLocationId env LocationId
    , HasSet AssetId env ()
    , HasSet AssetId env (InvestigatorId, UseType)
    , HasSet ConnectedLocationId env LocationId
    , HasSet EnemyId env CardCode
    , HasSet EnemyId env InvestigatorId
    , HasSet EnemyId env LocationId
    , HasSet EnemyId env ([Trait], LocationId)
    , HasSet ExhaustedAssetId env InvestigatorId
    , HasSet ExhaustedAssetId env ()
    , HasSet ExhaustedEnemyId env LocationId
    , HasSet FightableEnemyId env (InvestigatorId, Source)
    , HasSet InvestigatorId env ()
    , HasSet InvestigatorId env EnemyId
    , HasSet InvestigatorId env LocationId
    , HasSet InvestigatorId env (HashSet LocationId)
    , HasSet LocationId env [Trait]
    , HasSet Keyword env EnemyId
    , HasSet StoryEnemyId env CardCode
    , HasSet Trait env EnemyId
    , HasSet Trait env LocationId
    , HasSet Trait env Source
    , HasSet Trait env (InvestigatorId, CardId)
    , HasStep env ActStep
    , GetCardDef env EnemyId
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

class GetActions env where
  getActions :: (MonadReader env m) => m [Message]

class HasAbilities a where
  getAbilities :: a -> [Ability]
  getAbilities _ = []

class HasModifiersFor1 env f where
  getModifiersFor1 :: MonadReader env m => Source -> Target -> f p -> m [Modifier]

instance HasModifiersFor1 env f => HasModifiersFor1 env (M1 i c f) where
  getModifiersFor1 source target (M1 x) = getModifiersFor1 source target x

instance (HasModifiersFor1 env l, HasModifiersFor1 env r) => HasModifiersFor1 env (l :+: r) where
  getModifiersFor1 source target (L1 x) = getModifiersFor1 source target x
  getModifiersFor1 source target (R1 x) = getModifiersFor1 source target x

instance (HasModifiersFor env p) => HasModifiersFor1 env (K1 R p) where
  getModifiersFor1 source target (K1 x) = getModifiersFor source target x

genericGetModifiersFor
  :: (Generic a, HasModifiersFor1 env (Rep a), MonadReader env m)
  => Source
  -> Target
  -> a
  -> m [Modifier]
genericGetModifiersFor source target = getModifiersFor1 source target . from

class HasModifiersFor env a where
  getModifiersFor :: MonadReader env m => Source -> Target -> a -> m [Modifier]
  getModifiersFor _ _ _ = pure []

class IsEnemy enemy where
  isAloof :: enemy -> Bool

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

class (HasTraits a, HasCardDef a) => IsCard a where
  toCard :: a -> Card
  toCard a = lookupCard (cdCardCode $ toCardDef a) (toCardId a)
  toCardId :: a -> CardId

instance IsCard Card where
  toCardId = \case
    PlayerCard pc -> toCardId pc
    EncounterCard ec -> toCardId ec

instance IsCard PlayerCard where
  toCardId = pcId

instance IsCard EncounterCard where
  toCardId = ecId

class IsInvestigator a where
  isResigned :: a -> Bool
  isDefeated :: a -> Bool
  isEliminated :: a -> Bool
  isEliminated = uncurry (||) . (isResigned &&& isDefeated)
  hasEndedTurn :: a -> Bool
  hasResigned :: a -> Bool
