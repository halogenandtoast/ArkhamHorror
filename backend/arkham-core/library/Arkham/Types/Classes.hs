{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Arkham.Types.Classes
  ( module Arkham.Types.Classes
  , module Arkham.Types.Classes.HasRecord
  )
where

import Arkham.Types.Ability
import Arkham.Types.Classes.HasRecord
import Arkham.Types.Keyword
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.InvestigatorId
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

class HasLocation a where
  locationOf :: a -> LocationId

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

class HasActions a where
  getActions :: a -> InvestigatorId -> [Message]

class HasAbilities a where
  getAbilities :: a -> [Ability]

class HasVictoryPoints a where
  getVictoryPoints :: a -> Maybe Int
