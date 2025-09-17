module Arkham.Message.Lifted.CreateEnemy where

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Enemy.Creation
import Arkham.Enemy.Creation qualified as Msg
import Arkham.Helpers.Message qualified as Msg
import Arkham.Id
import Arkham.Message
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Queue
import Control.Monad.State.Strict

newtype CreateEnemyT m a = CreateEnemyT {unCreateEnemyT :: StateT (EnemyCreation Message) m a}
  deriving newtype
    (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadState (EnemyCreation Message), MonadRandom)

instance HasGame m => HasGame (CreateEnemyT m) where
  getGame = lift getGame
  getCache = GameCache \_ build -> build

instance CardGen m => CardGen (CreateEnemyT m) where
  genEncounterCard = lift . genEncounterCard
  genPlayerCard = lift . genPlayerCard
  replaceCard cId card = lift $ replaceCard cId card
  removeCard = lift . removeCard
  clearCardCache = lift clearCardCache

instance HasQueue Message m => HasQueue Message (CreateEnemyT m) where
  messageQueue = lift messageQueue
  pushAll = lift . pushAll

instance ReverseQueue m => ReverseQueue (CreateEnemyT m) where
  filterInbox = lift . filterInbox

createEnemyT
  :: (ReverseQueue m, FetchCard a, IsEnemyCreationMethod creation)
  => a
  -> creation
  -> (EnemyId -> CreateEnemyT m ())
  -> m EnemyId
createEnemyT a creation body = do
  card <- fetchCard a
  eff <- Msg.createEnemy card creation
  eff' <- execStateT (unCreateEnemyT $ body eff.enemy) eff
  push $ toMessage eff'
  pure eff'.enemy

runCreateEnemyT
  :: (ReverseQueue m, FetchCard a, IsEnemyCreationMethod creation)
  => a
  -> creation
  -> (EnemyId -> CreateEnemyT m ())
  -> m ()
runCreateEnemyT a creation body = do
  card <- fetchCard a
  void $ createEnemyT card creation body

afterCreate :: MonadIO m => QueueT Message m () -> CreateEnemyT m ()
afterCreate body = do
  msgs <- lift $ capture body
  modify' \creation -> creation {enemyCreationAfter = msgs}

createExhausted :: Monad m => CreateEnemyT m ()
createExhausted = modify' Msg.createExhausted

setCreationInvestigator :: Monad m => InvestigatorId -> CreateEnemyT m ()
setCreationInvestigator iid = modify' \creation -> creation {enemyCreationInvestigator = Just iid}
