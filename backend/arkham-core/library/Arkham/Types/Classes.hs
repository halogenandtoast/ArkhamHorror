{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
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
import Arkham.Types.FastWindow (FastWindow)
import Arkham.Types.InvestigatorId
import Arkham.Types.Keyword
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Trait
import ClassyPrelude
import GHC.Stack
import Lens.Micro
import Lens.Micro.Extras

class HasQueue a where
  messageQueue :: Lens' a (IORef [Message])

class (HasQueue env) => RunMessage env a where
  runMessage :: (MonadIO m, MonadReader env m) => Message -> a -> m a

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

runTest :: (HasQueue env, MonadReader env m, MonadIO m) => Int -> Int -> m ()
runTest skillValue tokenValue =
  unshiftMessage (RunSkillTest skillValue tokenValue)

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
    , HasCount SpendableClueCount AllInvestigators env
    , HasQueue env
    )

class HasActions env investigator a where
  getActions :: forall m. (MonadReader env m, MonadIO m) => investigator -> FastWindow -> a -> m [Message]

class (HasId LocationId () location) => IsLocation location where
  isBlocked :: location -> Bool

class (HasId EnemyId () enemy) => IsEnemy enemy where
  isAloof :: enemy -> Bool

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
  hasActionsRemaining :: investigator -> Bool
  canTakeDirectDamage :: investigator -> Bool
  discardOf :: investigator -> [PlayerCard]
