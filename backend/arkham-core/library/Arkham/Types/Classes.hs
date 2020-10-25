{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Classes
  ( module Arkham.Types.Classes
  , module Arkham.Types.Classes.HasRecord
  )
where

import Arkham.Types.Ability
import Arkham.Types.Action
import Arkham.Types.AssetId
import Arkham.Types.Card
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
import Arkham.Types.Token (DrawnToken, Token, TokenValue(..))
import Arkham.Types.Trait
import Arkham.Types.Window (Who, Window)
import qualified Arkham.Types.Window as Window
import ClassyPrelude
import Control.Monad.Fail
import qualified Data.HashSet as HashSet
import GHC.Generics
import GHC.Stack
import Lens.Micro hiding (to)
import Lens.Micro.Extras

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

class (HasQueue env) => HasTokenValue1 env f where
  getTokenValue1 :: (MonadIO m, MonadReader env m) => f p -> InvestigatorId -> DrawnToken -> m TokenValue

instance (HasQueue env, HasTokenValue1 env f) => HasTokenValue1 env (M1 i c f) where
  getTokenValue1 (M1 x) iid token = getTokenValue1 x iid token

instance (HasQueue env, HasTokenValue1 env l, HasTokenValue1 env r) => HasTokenValue1 env (l :+: r) where
  getTokenValue1 (L1 x) iid token = getTokenValue1 x iid token
  getTokenValue1 (R1 x) iid token = getTokenValue1 x iid token

instance (HasQueue env, HasTokenValue env p) => HasTokenValue1 env (K1 R p) where
  getTokenValue1 (K1 x) iid token = getTokenValue x iid token

class (HasQueue env) => HasTokenValue env a where
  getTokenValue :: (MonadIO m, MonadReader env m) => a -> InvestigatorId -> DrawnToken -> m TokenValue
  default getTokenValue :: (Generic a, HasTokenValue1 env (Rep a), MonadIO m, MonadReader env m) => a -> InvestigatorId -> DrawnToken -> m TokenValue
  getTokenValue = defaultGetTokenValue

defaultGetTokenValue
  :: (Generic a, HasTokenValue1 env (Rep a), MonadIO m, MonadReader env m)
  => a
  -> InvestigatorId
  -> DrawnToken
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
     , HasSet InvestigatorId () env
     , HasSet ConnectedLocationId LocationId env
     , HasId LocationId InvestigatorId env
     )
  => InvestigatorId
  -> m [(InvestigatorId, Window.Who)]
pairInvestigatorIdsForWindow iid = do
  investigatorIds <- setToList <$> asks (getSet @InvestigatorId ())
  lid <- asks (getId iid)
  connectedLocationIds <- HashSet.map unConnectedLocationId
    <$> asks (getSet lid)
  for investigatorIds $ \iid2 -> do
    lid2 <- asks (getId iid2)
    pure $ if iid2 == iid
      then (iid2, Window.You)
      else if lid2 == lid
        then (iid2, Window.InvestigatorAtYourLocation)
        else if lid2 `member` connectedLocationIds
          then (iid2, Window.InvestigatorAtAConnectedLocation)
          else (iid2, Window.InvestigatorInGame)

checkWindows
  :: ( MonadReader env m
     , HasSet InvestigatorId () env
     , HasSet ConnectedLocationId LocationId env
     , HasId LocationId InvestigatorId env
     )
  => InvestigatorId
  -> (Who -> m [Window])
  -> m [Message]
checkWindows iid f = do
  windowPairings <- pairInvestigatorIdsForWindow iid
  sequence [ CheckWindow iid' <$> f who | (iid', who) <- windowPairings ]

class HasModifiers1 env f where
  getModifiers1 :: (MonadIO m, MonadReader env m) => Source -> f p -> m [Modifier]

instance HasModifiers1 env f => HasModifiers1 env (M1 i c f) where
  getModifiers1 source (M1 x) = getModifiers1 source x

instance (HasModifiers1 env l, HasModifiers1 env r) => HasModifiers1 env (l :+: r) where
  getModifiers1 source (L1 x) = getModifiers1 source x
  getModifiers1 source (R1 x) = getModifiers1 source x

instance (HasModifiers env p) => HasModifiers1 env (K1 R p) where
  getModifiers1 source (K1 x) = getModifiers source x

defaultGetModifiers
  :: (Generic a, HasModifiers1 env (Rep a), MonadIO m, MonadReader env m)
  => Source
  -> a
  -> m [Modifier]
defaultGetModifiers source = getModifiers1 source . from

class HasModifiers env a where
  getModifiers :: (MonadReader env m, MonadIO m) => Source -> a -> m [Modifier]
  default getModifiers :: (Generic a, HasModifiers1 env (Rep a), MonadIO m, MonadReader env m) => Source -> a -> m [Modifier]
  getModifiers = defaultGetModifiers

class HasSource b a where
  getSource :: b -> a -> Maybe Source

class HasTarget b a where
  getTarget :: b -> a -> Maybe Target

class HasSet c b a where
  getSet :: b -> a -> HashSet c

class HasList c b a where
  getList :: b -> a -> [c]

class HasId c b a where
  getId :: HasCallStack => b -> a -> c

class HasCount c b a where
  getCount :: b -> a -> c

class HasStats b a where
  getStats :: (MonadReader env m, MonadIO m, HasModifiers env InvestigatorId) => b -> Source -> a -> m Stats

class HasTraits a where
  getTraits :: a -> HashSet Trait

instance HasTraits PlayerCard where
  getTraits = pcTraits

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
    , HasActions env (ActionType, env)
    , HasCount ActionRemainingCount (InvestigatorId, Maybe Action, [Trait]) env
    , HasCount CardCount InvestigatorId env
    , HasCount ClueCount LocationId env
    , HasCount HorrorCount InvestigatorId env
    , HasCount PlayerCount () env
    , HasCount ResourceCount InvestigatorId env
    , HasCount SpendableClueCount InvestigatorId env
    , HasId (Maybe LocationId) AssetId env
    , HasId (Maybe OwnerId) AssetId env
    , HasId (Maybe StoryAssetId) CardCode env
    , HasId (Maybe StoryEnemyId) CardCode env
    , HasId CardCode EnemyId env
    , HasId LocationId InvestigatorId env
    , HasId LocationId EnemyId env
    , HasList DiscardedPlayerCard InvestigatorId env
    , HasList HandCard InvestigatorId env
    , HasList InPlayCard InvestigatorId env
    , HasList UsedAbility () env
    , HasModifiersFor env env
    , HasSet AccessibleLocationId LocationId env
    , HasSet ConnectedLocationId LocationId env
    , HasSet EnemyId LocationId env
    , HasSet ExhaustedAssetId InvestigatorId env
    , HasSet ExhaustedEnemyId LocationId env
    , HasSet InvestigatorId () env
    , HasSet InvestigatorId EnemyId env
    , HasSet InvestigatorId LocationId env
    , HasSet Keyword EnemyId env
    , HasSet Trait EnemyId env
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

class HasModifiersFor1 env f where
  getModifiersFor1 :: (MonadIO m, MonadReader env m) => Source -> Target -> f p -> m [Modifier]

instance HasModifiersFor1 env f => HasModifiersFor1 env (M1 i c f) where
  getModifiersFor1 source target (M1 x) = getModifiersFor1 source target x

instance (HasModifiersFor1 env l, HasModifiersFor1 env r) => HasModifiersFor1 env (l :+: r) where
  getModifiersFor1 source target (L1 x) = getModifiersFor1 source target x
  getModifiersFor1 source target (R1 x) = getModifiersFor1 source target x

instance (HasModifiersFor env p) => HasModifiersFor1 env (K1 R p) where
  getModifiersFor1 source target (K1 x) = getModifiersFor source target x

defaultGetModifiersFor
  :: (Generic a, HasModifiersFor1 env (Rep a), MonadIO m, MonadReader env m)
  => Source
  -> Target
  -> a
  -> m [Modifier]
defaultGetModifiersFor source target = getModifiersFor1 source target . from

class HasModifiersFor env a where
  getModifiersFor :: (MonadReader env m, MonadIO m) => Source -> Target -> a -> m [Modifier]
  default getModifiersFor :: (Generic a, HasModifiersFor1 env (Rep a), MonadIO m, MonadReader env m) => Source -> Target -> a -> m [Modifier]
  getModifiersFor = defaultGetModifiersFor

class (HasId LocationId () location) => IsLocation location where
  isBlocked :: location -> Bool

class (HasId EnemyId () enemy) => IsEnemy enemy where
  isAloof :: enemy -> Bool

class IsScenario scenario where
  tokensWithNegativeModifier :: scenario -> HashSet Token

class Exhaustable a where
  isExhausted :: a -> Bool
  isReady :: a -> Bool

  isExhausted = not . isReady
  isReady = not . isExhausted
  {-# MINIMAL isExhausted | isReady #-}

class IsCard a where
  toCard :: a -> Card
