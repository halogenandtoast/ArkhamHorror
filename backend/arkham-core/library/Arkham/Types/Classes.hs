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
import Arkham.Types.Token (Token)
import Arkham.Types.Trait
import Arkham.Types.Window (Window)
import qualified Arkham.Types.Window as Window
import ClassyPrelude
import Control.Monad.Fail
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

withQueue
  :: (MonadIO m, MonadReader env m, HasQueue env)
  => ([Message] -> ([Message], r))
  -> m r
withQueue body = do
  ref <- asks $ view messageQueue
  liftIO $ atomicModifyIORef' ref body

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

runTest
  :: (HasQueue env, MonadReader env m, MonadIO m)
  => InvestigatorId
  -> Int
  -> m ()
runTest iid tokenValue = if tokenValue < 0
  then unshiftMessages
    [ CheckWindow iid [Window.WhenRevealTokenWithNegativeModifier Window.You]
    , RunSkillTest tokenValue
    ]
  else unshiftMessage (RunSkillTest tokenValue)

class HasSet c b a where
  getSet :: b -> a -> HashSet c

class HasList c b a where
  getList :: b -> a -> [c]

class HasId c b a where
  getId :: HasCallStack => b -> a -> c

class HasCount c b a where
  getCount :: b -> a -> c

class HasInvestigatorStats c b a where
  getStats :: b -> a -> c

class HasTraits a where
  getTraits :: a -> HashSet Trait

class IsAdvanceable a where
  isAdvanceable :: a -> Bool

class HasSkill a where
  getSkill :: SkillType -> a -> Int

class HasKeywords a where
  getKeywords :: a -> HashSet Keyword

class HasAbilities a where
  getAbilities :: a -> [Ability]

class HasModifiers a where
  getModifiers :: a -> [Modifier]

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

type ActionRunner env investigator
  = ( IsInvestigator investigator
    , HasActions env investigator (ActionType, env)
    , HasId (Maybe StoryAssetId) CardCode env
    , HasId (Maybe OwnerId) AssetId env
    , HasId (Maybe LocationId) AssetId env
    , HasId LocationId InvestigatorId env
    , HasSet EnemyId LocationId env
    , HasList UsedAbility () env
    , HasCount PlayerCount () env
    , HasCount SpendableClueCount AllInvestigators env
    , HasCount ClueCount LocationId env
    , HasQueue env
    )

class HasActions1 env investigator f where
  getActions1 :: (MonadIO m, MonadReader env m) => investigator -> Window -> f p -> m [Message]

instance HasActions1 env investigator f => HasActions1 env investigator (M1 i c f) where
  getActions1 investigator window (M1 x) = getActions1 investigator window x

instance (HasActions1 env investigator l, HasActions1 env investigator r) => HasActions1 env investigator (l :+: r) where
  getActions1 investigator window (L1 x) = getActions1 investigator window x
  getActions1 investigator window (R1 x) = getActions1 investigator window x

instance (HasActions env investigator p) => HasActions1 env investigator (K1 R p) where
  getActions1 investigator window (K1 x) = getActions investigator window x

defaultGetActions
  :: ( Generic a
     , HasActions1 env investigator (Rep a)
     , MonadIO m
     , MonadReader env m
     )
  => investigator
  -> Window
  -> a
  -> m [Message]
defaultGetActions investigator window = getActions1 investigator window . from

class HasActions env investigator a where
  getActions :: (MonadReader env m, MonadIO m) => investigator -> Window -> a -> m [Message]
  default getActions :: (Generic a, HasActions1 env investigator (Rep a), MonadIO m, MonadReader env m) => investigator -> Window -> a -> m [Message]
  getActions = defaultGetActions

class (HasId LocationId () location) => IsLocation location where
  isBlocked :: location -> Bool

class (HasId EnemyId () enemy) => IsEnemy enemy where
  isAloof :: enemy -> Bool

class IsScenario scenario where
  tokensWithNegativeModifier :: scenario -> HashSet Token

class (HasId InvestigatorId () investigator) => IsInvestigator investigator where
  locationOf :: investigator -> LocationId
  canInvestigate :: (IsLocation location) => location -> investigator -> Bool
  canMoveTo :: (IsLocation location) => location -> investigator -> Bool
  canFight :: (IsEnemy enemy) => enemy -> investigator -> Bool
  canEngage :: (IsEnemy enemy) => enemy -> investigator -> Bool
  canEvade :: (IsEnemy enemy) => enemy -> investigator -> Bool
  resourceCount :: investigator -> Int
  clueCount :: investigator -> Int
  spendableClueCount :: investigator -> Int
  cardCount :: investigator -> Int
  discardableCardCount :: investigator -> Int
  canDo :: Action -> investigator -> Bool
  hasActionsRemaining :: investigator -> Maybe Action -> HashSet Trait -> Bool
  canTakeDirectDamage :: investigator -> Bool
  discardOf :: investigator -> [PlayerCard]
