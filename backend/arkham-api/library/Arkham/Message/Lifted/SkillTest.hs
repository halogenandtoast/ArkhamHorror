module Arkham.Message.Lifted.SkillTest where

import Arkham.Calculation
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Id
import Arkham.Matcher
import Arkham.Message.Lifted qualified as Lifted
import Arkham.Modifier
import Arkham.Prelude
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Treachery.Import.Lifted qualified as Lifted (revelationSkillTest)
import Control.Monad.Random
import Control.Monad.State

data Origin where
  Origin :: (Sourceable a, Targetable a) => a -> Origin

instance Sourceable Origin where
  toSource (Origin a) = toSource a

instance Targetable Origin where
  toTarget (Origin a) = toTarget a

data SkillTestState = SkillTestState
  { skillTestStateId :: SkillTestId
  , skillTestStateInvestigator :: InvestigatorId
  , skillTestStateOrigin :: Origin
  , skillTestStateSkillType :: SkillType
  , skillTestStateCalculation :: GameCalculation
  , skillTestStateIsRevelation :: Bool
  }

newtype SkillTestT m a = SkillTestT {runSkillTestT :: StateT SkillTestState m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadState SkillTestState)

instance HasQueue msg m => HasQueue msg (SkillTestT m) where
  messageQueue = lift messageQueue
  pushAll = lift . pushAll

instance HasGame m => HasGame (SkillTestT m) where
  getGame = lift getGame

instance CardGen m => CardGen (SkillTestT m) where
  genEncounterCard = lift . genEncounterCard
  genPlayerCard = lift . genPlayerCard
  replaceCard cid card = lift $ replaceCard cid card
  clearCardCache = lift clearCardCache

instance MonadRandom m => MonadRandom (SkillTestT m) where
  getRandom = lift getRandom
  getRandomR = lift . getRandomR
  getRandomRs = lift . getRandomRs
  getRandoms = lift getRandoms

instance Lifted.ReverseQueue m => Lifted.ReverseQueue (SkillTestT m)

skillTest
  :: (Lifted.ReverseQueue m, Sourceable attrs, Targetable attrs)
  => InvestigatorId
  -> attrs
  -> SkillType
  -> GameCalculation
  -> SkillTestT m ()
  -> m ()
skillTest iid attrs skillType calculation action = do
  sid <- getRandom
  SkillTestState {..} <-
    execStateT (runSkillTestT action)
      $ SkillTestState
        { skillTestStateId = sid
        , skillTestStateInvestigator = iid
        , skillTestStateOrigin = Origin attrs
        , skillTestStateSkillType = skillType
        , skillTestStateCalculation = calculation
        , skillTestStateIsRevelation = False
        }
  if skillTestStateIsRevelation
    then
      Lifted.revelationSkillTest
        skillTestStateId
        skillTestStateInvestigator
        skillTestStateOrigin
        skillTestStateSkillType
        skillTestStateCalculation
    else
      Lifted.beginSkillTest
        skillTestStateId
        skillTestStateInvestigator
        skillTestStateOrigin
        skillTestStateOrigin
        skillTestStateSkillType
        skillTestStateCalculation

revelationSkillTest
  :: (Lifted.ReverseQueue m, Sourceable attrs, Targetable attrs)
  => InvestigatorId
  -> attrs
  -> SkillType
  -> GameCalculation
  -> SkillTestT m ()
  -> m ()
revelationSkillTest iid attrs skillType calculation action = do
  skillTest iid attrs skillType calculation $ do
    revelation
    action

failOnReveal :: Lifted.ReverseQueue m => ChaosTokenMatcher -> SkillTestT m ()
failOnReveal matcher = do
  sid <- gets skillTestStateId
  Origin attrs <- gets skillTestStateOrigin
  Lifted.failOnReveal matcher sid attrs

revelation :: Monad m => SkillTestT m ()
revelation = get >>= \st -> put st {skillTestStateIsRevelation = True}

discoverAdditionalClues
  :: (Lifted.ReverseQueue m, Sourceable source) => source -> InvestigatorId -> Int -> m ()
discoverAdditionalClues source iid n = withSkillTest \sid -> Lifted.skillTestModifier sid source iid (DiscoveredClues n)
