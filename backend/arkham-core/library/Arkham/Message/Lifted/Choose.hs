module Arkham.Message.Lifted.Choose where

import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Id
import Arkham.Message (Message)
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Question
import Arkham.SkillType
import Arkham.Target
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict

newtype ChooseT m a = ChooseT {unChooseT :: StateT Bool (WriterT [UI Message] m) a}
  deriving newtype
    (Functor, Applicative, Monad, MonadWriter [UI Message], MonadState Bool, MonadIO)

instance HasGame m => HasGame (ChooseT m) where
  getGame = lift getGame

instance MonadTrans ChooseT where
  lift = ChooseT . lift . lift

runChooseT :: ReverseQueue m => ChooseT m a -> m [UI Message]
runChooseT = execWriterT . (`evalStateT` False) . unChooseT

chooseOneM :: ReverseQueue m => InvestigatorId -> ChooseT m a -> m ()
chooseOneM iid choices = do
  choices' <- runChooseT choices
  unless (null choices') $ chooseOne iid choices'

chooseOneFromEachM :: ReverseQueue m => InvestigatorId -> [ChooseT m a] -> m ()
chooseOneFromEachM iid choices = do
  choices' <- traverse runChooseT choices
  unless (null choices') $ chooseOneFromEach iid choices'

chooseOrRunOneM :: ReverseQueue m => InvestigatorId -> ChooseT m a -> m ()
chooseOrRunOneM iid choices = do
  choices' <- runChooseT choices
  unless (null choices') $ chooseOrRunOne iid choices'

chooseNM :: ReverseQueue m => InvestigatorId -> Int -> ChooseT m a -> m ()
chooseNM iid n choices = do
  choices' <- runChooseT choices
  unless (null choices') $ chooseN iid n choices'

chooseUpToNM :: ReverseQueue m => InvestigatorId -> Int -> Text -> ChooseT m a -> m ()
chooseUpToNM iid n done choices = do
  choices' <- runChooseT choices
  unless (null choices') $ chooseUpToN iid n done choices'

chooseOneAtATimeM :: ReverseQueue m => InvestigatorId -> ChooseT m a -> m ()
chooseOneAtATimeM iid choices = do
  choices' <- runChooseT choices
  unless (null choices') $ chooseOneAtATime iid choices'

forcedWhen :: Monad m => Bool -> ChooseT m () -> ChooseT m ()
forcedWhen b action =
  if b
    then do
      censor id action
      put True
    else action

unterminated :: ReverseQueue m => ChooseT m () -> ChooseT m ()
unterminated action = do
  b <- get
  unless b action

labeled :: ReverseQueue m => Text -> QueueT Message m () -> ChooseT m ()
labeled label action = unterminated do
  msgs <- lift $ evalQueueT action
  tell [Label label msgs]

damageLabeled :: ReverseQueue m => InvestigatorId -> QueueT Message m () -> ChooseT m ()
damageLabeled iid action = unterminated do
  msgs <- lift $ evalQueueT action
  tell [DamageLabel iid msgs]

horrorLabeled :: ReverseQueue m => InvestigatorId -> QueueT Message m () -> ChooseT m ()
horrorLabeled iid action = unterminated do
  msgs <- lift $ evalQueueT action
  tell [HorrorLabel iid msgs]

assetDamageLabeled :: ReverseQueue m => AssetId -> QueueT Message m () -> ChooseT m ()
assetDamageLabeled aid action = unterminated do
  msgs <- lift $ evalQueueT action
  tell [AssetDamageLabel aid msgs]

assetHorrorLabeled :: ReverseQueue m => AssetId -> QueueT Message m () -> ChooseT m ()
assetHorrorLabeled aid action = unterminated do
  msgs <- lift $ evalQueueT action
  tell [AssetHorrorLabel aid msgs]

skillLabeled :: ReverseQueue m => SkillType -> QueueT Message m () -> ChooseT m ()
skillLabeled skillType action = unterminated do
  msgs <- lift $ evalQueueT action
  tell [SkillLabel skillType msgs]

targeting :: (ReverseQueue m, Targetable target) => target -> QueueT Message m () -> ChooseT m ()
targeting target action = unterminated do
  msgs <- lift $ evalQueueT action
  tell [targetLabel target msgs]

targets
  :: (ReverseQueue m, Targetable target) => [target] -> (target -> QueueT Message m ()) -> ChooseT m ()
targets ts action = unterminated $ for_ ts \t -> targeting t (action t)

nothing :: Monad m => QueueT Message m ()
nothing = pure ()
