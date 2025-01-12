{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Arkham.Script where

import Arkham.Classes.Entity
import Arkham.Classes.HasQueue
import Arkham.Classes.HasGame
import Arkham.Classes.RunMessage
import Arkham.Classes.Query
import Arkham.GameT
import Arkham.Id
import Arkham.Message.Lifted.Choose (chooseTargetM)
import Arkham.Message.Lifted.Queue
import Arkham.Message.Lifted qualified as Msg
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.ChaosToken.Types
import Arkham.Prelude hiding (Handler)
import Arkham.Queue
import Arkham.Cost (Payment)
import Arkham.Source
import Control.Monad.State.Strict
import GHC.Records
import Arkham.Window qualified as Window
import Arkham.Window (Window)

newtype Handler a
  = Handler (forall. (?this :: a) => Message -> Maybe (QueueT Message GameT a))

newtype RunState a = RunState [Handler a]

newtype RunBuilderT b a = RunBuilderT { runBuilderT :: StateT (RunState b) (QueueT Message GameT) a }
  deriving newtype (Functor, Applicative, Monad, MonadState (RunState b))

this :: (?this :: a) => a
this = ?this

you :: (?you :: InvestigatorId) => InvestigatorId
you = ?you

ability :: (?ability :: Source) => Source
ability = ?ability

onAbility
  :: (Entity a, attrs ~ EntityAttrs a, Sourceable attrs, HasField "ability" attrs (Int -> Source))
  => Int
  -> (( ?ability :: Source
      , ?you :: InvestigatorId
      , ?windows :: [Window]
      , ?payment :: Payment
      , ?this :: a
      , Entity a
      , Sourceable attrs
      , HasField "ability" attrs (Int -> Source)
      )
    => QueueT Message GameT a)
  -> RunBuilderT a ()
onAbility n body = onMessage \case
  UseCardAbility iid source n' windows payment | n == n' && isSource (toAttrs this) source -> Just $
    let ?you = iid; ?ability = (toAttrs this).ability n; ?windows = windows; ?payment = payment
    in body
  _ -> Nothing

onMessage :: ((?this :: a) => Message -> Maybe (QueueT Message GameT a)) -> RunBuilderT a ()
onMessage f = modify \(RunState xs) -> RunState (Handler f : xs)

script
  :: forall a. (RunMessage (EntityAttrs a), Entity a)
  => ((?this :: a) => RunBuilderT a ())
  -> Message
  -> a
  -> GameT a
script body msg a = runQueueT do
  (RunState mapping :: RunState a) <- execStateT (runBuilderT $ let ?this = a in body) (RunState mempty)
  go $ reverse mapping
 where
  go :: [Handler a] -> QueueT Message GameT a
  go [] = overAttrsM (let ?this = a in liftRunMessage msg) a
  go (Handler f : fs) = case (let ?this = a in f msg) of
    Just handler -> let ?this = a in handler
    Nothing -> go fs

passedWithElderSign
  :: (Entity a, Is attrs InvestigatorId, attrs ~ EntityAttrs a)
  =>
  (( ?you :: InvestigatorId
   , ?this :: a
   )
    => QueueT Message GameT a) -> RunBuilderT a ()
passedWithElderSign body = onMessage \case
  PassedSkillTest iid _ _ (ChaosTokenTarget (chaosTokenFace -> ElderSign)) _ _ | is (toAttrs this) iid -> Just $
    let ?you = iid
    in body
  _ -> Nothing

class Yours a where
  yours :: (?you :: InvestigatorId) => a

instance Yours AssetMatcher where
  yours = assetControlledBy you

class AtYourLocation a where
  atYourLocation :: (?you :: InvestigatorId) => a

instance AtYourLocation EnemyMatcher where
  atYourLocation = enemyAtLocationWith you

class ChooseAmong a where
  type ChosenType a :: Type
  toChooseAmong :: HasGame m => a -> m [ChosenType a]

instance ChooseAmong [Target] where
  type ChosenType [Target] = Target
  toChooseAmong = pure

instance ChooseAmong EnemyMatcher where
  type ChosenType EnemyMatcher = EnemyId
  toChooseAmong = select

chooseAmong :: (ChooseAmong a, el ~ ChosenType a, Targetable el, ?you :: InvestigatorId, ReverseQueue m) => a -> (el -> QueueT Message m ()) -> m ()
chooseAmong a body = do
  xs <- toChooseAmong a
  chooseTargetM you xs body

chooseEnemy :: (?you :: InvestigatorId, ReverseQueue m) => EnemyMatcher -> (EnemyId -> QueueT Message m ()) -> m ()
chooseEnemy = chooseAmong

elderSignEffect :: (Entity a, attrs ~ EntityAttrs a, Is attrs InvestigatorId) => ((?this :: a) => QueueT Message GameT a) -> RunBuilderT a ()
elderSignEffect body = onMessage \case
  ResolveChaosToken _ ElderSign iid | is (toAttrs this) iid -> Just $ let ?you = iid in body
  _ -> Nothing
    
revealedChaosTokens :: (?windows :: [Window]) => [ChaosToken]
revealedChaosTokens = Window.revealedChaosTokens ?windows

drawAnotherChaosToken :: (?you :: InvestigatorId, ReverseQueue m) => m ()
drawAnotherChaosToken = Msg.drawAnotherChaosToken you
