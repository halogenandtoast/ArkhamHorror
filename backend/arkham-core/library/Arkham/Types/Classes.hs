{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Classes
  ( module Arkham.Types.Classes
  , module Arkham.Types.Classes.HasRecord
  )
where

import Arkham.Types.Ability
import Arkham.Types.Action
import Arkham.Types.Asset.Uses (UseType)
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Classes.HasRecord
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.Keyword
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Stats
import Arkham.Types.Target
import Arkham.Types.Token (Token, TokenValue(..))
import Arkham.Types.Trait
import Arkham.Types.Window (Who, Window)
import qualified Arkham.Types.Window as Window
import ClassyPrelude
import Control.Monad.Fail
import qualified Data.HashSet as HashSet
import GHC.Generics
import Lens.Micro hiding (to)
import Lens.Micro.Extras

newtype Distance = Distance { unDistance :: Int }

class HasQueue a where
  messageQueue :: Lens' a (IORef [Message])

class (HasQueue env) => RunMessage1 env f where
  runMessage1 :: (MonadIO m, MonadReader env m, MonadFail m) => Message -> f p -> m (f p)

instance (HasQueue env, RunMessage1 env f) => RunMessage1 env (M1 i c f) where
  runMessage1 msg (M1 x) = M1 <$> runMessage1 msg x

instance (HasQueue env, RunMessage1 env l, RunMessage1 env r) => RunMessage1 env (l :+: r) where
  runMessage1 msg (L1 x) = L1 <$> runMessage1 msg x
  runMessage1 msg (R1 x) = R1 <$> runMessage1 msg x

instance (HasQueue env, RunMessage env p) => RunMessage1 env (K1 R p) where
  runMessage1 msg (K1 x) = K1 <$> runMessage msg x

class (HasQueue env) => RunMessage env a where
  runMessage :: (MonadIO m, MonadReader env m, MonadFail m) => Message -> a -> m a
  default runMessage :: (Generic a, RunMessage1 env (Rep a), MonadIO m, MonadReader env m, MonadFail m) => Message -> a -> m a
  runMessage = defaultRunMessage

defaultRunMessage
  :: ( Generic a
     , RunMessage1 env (Rep a)
     , MonadIO m
     , MonadReader env m
     , MonadFail m
     )
  => Message
  -> a
  -> m a
defaultRunMessage msg = fmap to . runMessage1 msg . from

class HasTokenValue1 env f where
  getTokenValue1 :: MonadReader env m => f p -> InvestigatorId -> Token -> m TokenValue

instance (HasTokenValue1 env f) => HasTokenValue1 env (M1 i c f) where
  getTokenValue1 (M1 x) iid token = getTokenValue1 x iid token

instance (HasTokenValue1 env l, HasTokenValue1 env r) => HasTokenValue1 env (l :+: r) where
  getTokenValue1 (L1 x) iid token = getTokenValue1 x iid token
  getTokenValue1 (R1 x) iid token = getTokenValue1 x iid token

instance (HasTokenValue env p) => HasTokenValue1 env (K1 R p) where
  getTokenValue1 (K1 x) iid token = getTokenValue x iid token

class HasTokenValue env a where
  getTokenValue :: MonadReader env m => a -> InvestigatorId -> Token -> m TokenValue
  default getTokenValue :: (Generic a, HasTokenValue1 env (Rep a), MonadReader env m) => a -> InvestigatorId -> Token -> m TokenValue
  getTokenValue = defaultGetTokenValue

defaultGetTokenValue
  :: (Generic a, HasTokenValue1 env (Rep a), MonadReader env m)
  => a
  -> InvestigatorId
  -> Token
  -> m TokenValue
defaultGetTokenValue a iid token = getTokenValue1 (from a) iid token

withQueue
  :: (MonadIO m, MonadReader env m, HasQueue env)
  => ([Message] -> ([Message], r))
  -> m r
withQueue body = do
  ref <- asks $ view messageQueue
  liftIO $ atomicModifyIORef' ref body

fromQueue
  :: (MonadIO m, MonadReader env m, HasQueue env) => ([Message] -> r) -> m r
fromQueue f = f <$> (readIORef =<< asks (view messageQueue))

popMessage :: (MonadIO m, MonadReader env m, HasQueue env) => m (Maybe Message)
popMessage = withQueue $ \case
  [] -> ([], Nothing)
  (m : ms) -> (ms, Just m)

clearQueue :: (MonadIO m, MonadReader env m, HasQueue env) => m ()
clearQueue = withQueue $ const ([], ())

peekMessage :: (MonadIO m, MonadReader env m, HasQueue env) => m (Maybe Message)
peekMessage = withQueue $ \case
  [] -> ([], Nothing)
  (m : ms) -> (m : ms, Just m)

pushMessage :: (MonadIO m, MonadReader env m, HasQueue env) => Message -> m ()
pushMessage = pushMessages . pure

pushMessages
  :: (MonadIO m, MonadReader env m, HasQueue env) => [Message] -> m ()
pushMessages msgs = withQueue $ \queue -> (queue <> msgs, ())

unshiftMessage
  :: (MonadIO m, MonadReader env m, HasQueue env) => Message -> m ()
unshiftMessage = unshiftMessages . pure

unshiftMessages
  :: (MonadIO m, MonadReader env m, HasQueue env) => [Message] -> m ()
unshiftMessages msgs = withQueue $ \queue -> (msgs <> queue, ())

pairInvestigatorIdsForWindow
  :: ( MonadReader env m
     , HasSet InvestigatorId env ()
     , HasSet ConnectedLocationId env LocationId
     , HasId LocationId env InvestigatorId
     )
  => InvestigatorId
  -> m [(InvestigatorId, Window.Who)]
pairInvestigatorIdsForWindow iid = do
  investigatorIds <- getSetList @InvestigatorId ()
  lid <- getId iid
  connectedLocationIds <- HashSet.map unConnectedLocationId <$> getSet lid
  for investigatorIds $ \iid2 -> do
    lid2 <- getId iid2
    pure $ if iid2 == iid
      then (iid2, Window.You)
      else if lid2 == lid
        then (iid2, Window.InvestigatorAtYourLocation)
        else if lid2 `member` connectedLocationIds
          then (iid2, Window.InvestigatorAtAConnectedLocation)
          else (iid2, Window.InvestigatorInGame)

checkWindows
  :: ( MonadReader env m
     , HasSet InvestigatorId env ()
     , HasSet ConnectedLocationId env LocationId
     , HasId LocationId env InvestigatorId
     )
  => InvestigatorId
  -> (Who -> m [Window])
  -> m [Message]
checkWindows iid f = do
  windowPairings <- pairInvestigatorIdsForWindow iid
  sequence [ CheckWindow iid' <$> f who | (iid', who) <- windowPairings ]

class HasStep c a where
  getStep ::  a -> c

class HasSource b a where
  getSource :: b -> a -> Maybe Source

class HasRoundHistory a where
  getRoundHistory :: MonadIO m => a -> m [Message]

class HasTarget b a where
  getTarget :: b -> a -> Maybe Target

class (Hashable set, Eq set) => HasSet set env a where
  getSet :: MonadReader env m => a -> m (HashSet set)
  getSetList :: MonadReader env m => a -> m [set]
  getSetList a = setToList <$> getSet a

class HasList list env a where
  getList :: (MonadReader env m) => a -> m [list]

class HasId id env a where
  getId :: (MonadReader env m) => a -> m id

class HasCount count env a where
  getCount :: (MonadReader env m) => a -> m count

class HasStats env a where
  getStats :: (MonadReader env m) => a -> Source -> m Stats

class HasTraits a where
  getTraits :: a -> HashSet Trait

instance HasTraits Card where
  getTraits (PlayerCard card) = getTraits card
  getTraits (EncounterCard card) = getTraits card

instance HasTraits PlayerCard where
  getTraits = pcTraits

instance HasTraits EncounterCard where
  getTraits = ecTraits

class IsAdvanceable a where
  isAdvanceable :: a -> Bool

class HasSkill a where
  getSkill :: SkillType -> a -> Int

class HasKeywords a where
  getKeywords :: a -> HashSet Keyword

class HasAbilities a where
  getAbilities :: a -> [Ability]

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
  getVictoryPoints MkEncounterCard {..} = ecVictoryPoints

instance HasVictoryPoints PlayerCard where
  getVictoryPoints MkPlayerCard {..} = pcVictoryPoints

type ActionRunner env
  = ( HasQueue env
    , HasActions env ActionType
    , HasCount AssetCount env (InvestigatorId, [Trait])
    , HasCount ActionRemainingCount env (Maybe Action, [Trait], InvestigatorId)
    , HasCount ActionTakenCount env InvestigatorId
    , HasCount CardCount env InvestigatorId
    , HasCount ClueCount env LocationId
    , HasCount HorrorCount env InvestigatorId
    , HasCount PlayerCount env ()
    , HasCount ResourceCount env InvestigatorId
    , HasCount SpendableClueCount env InvestigatorId
    , HasId (Maybe LocationId) env AssetId
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
    , HasSet AssetId env (InvestigatorId, UseType)
    , HasSet ConnectedLocationId env LocationId
    , HasSet EnemyId env LocationId
    , HasSet ExhaustedAssetId env InvestigatorId
    , HasSet ExhaustedEnemyId env LocationId
    , HasSet InvestigatorId env ()
    , HasSet InvestigatorId env EnemyId
    , HasSet InvestigatorId env LocationId
    , HasSet Keyword env EnemyId
    , HasSet Trait env EnemyId
    , HasSet Trait env LocationId
    , HasSet Trait env (InvestigatorId, CardId)
    , HasSource ForSkillTest env
    )

class HasActions1 env f where
  getActions1 :: (MonadIO m, MonadReader env m) => InvestigatorId -> Window -> f p -> m [Message]

instance HasActions1 env f => HasActions1 env (M1 i c f) where
  getActions1 iid window (M1 x) = getActions1 iid window x

instance (HasActions1 env l, HasActions1 env r) => HasActions1 env (l :+: r) where
  getActions1 iid window (L1 x) = getActions1 iid window x
  getActions1 iid window (R1 x) = getActions1 iid window x

instance (HasActions env p) => HasActions1 env (K1 R p) where
  getActions1 iid window (K1 x) = getActions iid window x

defaultGetActions
  :: (Generic a, HasActions1 env (Rep a), MonadIO m, MonadReader env m)
  => InvestigatorId
  -> Window
  -> a
  -> m [Message]
defaultGetActions iid window = getActions1 iid window . from

class HasActions env a where
  getActions :: (MonadReader env m, MonadIO m) => InvestigatorId -> Window -> a -> m [Message]
  default getActions :: (Generic a, HasActions1 env (Rep a), MonadIO m, MonadReader env m) => InvestigatorId -> Window -> a -> m [Message]
  getActions = defaultGetActions

instance HasActions env ActionType => HasActions env () where
  getActions iid window _ = do
    locationActions <- getActions iid window LocationActionType
    enemyActions <- getActions iid window EnemyActionType
    assetActions <- getActions iid window AssetActionType
    treacheryActions <- getActions iid window TreacheryActionType
    actActions <- getActions iid window ActActionType
    agendaActions <- getActions iid window AgendaActionType
    investigatorActions <- getActions iid window InvestigatorActionType
    pure
      $ enemyActions
      <> locationActions
      <> assetActions
      <> treacheryActions
      <> actActions
      <> agendaActions
      <> investigatorActions

class HasModifiersFor1 env f where
  getModifiersFor1 :: (MonadReader env m) => Source -> Target -> f p -> m [Modifier]

instance HasModifiersFor1 env f => HasModifiersFor1 env (M1 i c f) where
  getModifiersFor1 source target (M1 x) = getModifiersFor1 source target x

instance (HasModifiersFor1 env l, HasModifiersFor1 env r) => HasModifiersFor1 env (l :+: r) where
  getModifiersFor1 source target (L1 x) = getModifiersFor1 source target x
  getModifiersFor1 source target (R1 x) = getModifiersFor1 source target x

instance (HasModifiersFor env p) => HasModifiersFor1 env (K1 R p) where
  getModifiersFor1 source target (K1 x) = getModifiersFor source target x

defaultGetModifiersFor
  :: (Generic a, HasModifiersFor1 env (Rep a), MonadReader env m)
  => Source
  -> Target
  -> a
  -> m [Modifier]
defaultGetModifiersFor source target = getModifiersFor1 source target . from

class HasModifiersFor env a where
  getModifiersFor :: (MonadReader env m) => Source -> Target -> a -> m [Modifier]
  default getModifiersFor :: (Generic a, HasModifiersFor1 env (Rep a), MonadReader env m) => Source -> Target -> a -> m [Modifier]
  getModifiersFor = defaultGetModifiersFor

noModifiersFor :: (MonadReader env m) => Source -> Target -> a -> m [Modifier]
noModifiersFor _ _ _ = pure []

class (HasId EnemyId () enemy) => IsEnemy enemy where
  isAloof :: enemy -> Bool

class Discardable a where
  canBeDiscarded :: a -> Bool

class CanBeWeakness env a where
  getIsWeakness :: (MonadReader env m) => a -> m Bool

class Entity a where
  type EntityId a
  toId :: a -> EntityId a
  toTarget :: a -> Target
  isTarget :: a -> Target -> Bool
  toSource :: a -> Source
  isSource :: a -> Source -> Bool

class Exhaustable a where
  isExhausted :: a -> Bool
  isReady :: a -> Bool

  isExhausted = not . isReady
  isReady = not . isExhausted
  {-# MINIMAL isExhausted | isReady #-}

class IsCard a where
  toCard :: a -> Card

class IsInvestigator a where
  isResigned :: a -> Bool
  isDefeated :: a -> Bool
  isEliminated :: a -> Bool
  isEliminated = uncurry (||) . (isResigned &&& isDefeated)

class HasAttrs a where
  type AttrsT a
  toAttrs :: a -> AttrsT a
  default toAttrs :: (Generic a, HasAttrs1 (Rep a), AttrsT1 (Rep a) ~ AttrsT a) => a -> AttrsT a
  toAttrs = toAttrs1 . from

class HasAttrs1 f where
  type AttrsT1 f
  toAttrs1 :: f p -> AttrsT1 f

instance HasAttrs1 f => HasAttrs1 (M1 i c f) where
  type AttrsT1 (M1 i c f) = AttrsT1 f
  toAttrs1 (M1 x) = toAttrs1 x

instance (HasAttrs1 l, HasAttrs1 r, AttrsT1 l ~ AttrsT1 r) => HasAttrs1 (l :+: r) where
  type AttrsT1 (l :+: r) = AttrsT1 l
  toAttrs1 (L1 x) = toAttrs1 x
  toAttrs1 (R1 x) = toAttrs1 x

instance HasAttrs p => HasAttrs1 (K1 R p) where
  type AttrsT1 (K1 R p) = AttrsT p
  toAttrs1 (K1 x) = toAttrs x
