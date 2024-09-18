{-# LANGUAGE ImplicitParams #-}
module Arkham.Script where

import Arkham.Treachery.Import.Lifted qualified as Msg
import Arkham.Prelude
import Arkham.Queue
import Arkham.Classes.HasQueue
import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Classes.RunMessage
import Arkham.Message.Lifted.Queue
import Arkham.Card
import Arkham.Calculation
import Arkham.Source
import Arkham.Target
import Arkham.Message
import Arkham.Matcher
import Arkham.Id
import Arkham.GameT
import Arkham.SkillType
import Control.Monad.Reader

data ScriptState = ScriptState { isRevelation :: Bool, shouldRun :: Bool }

newtype ScriptT a = ScriptT { runScriptT :: ReaderT ScriptState (QueueT Message GameT) a }
  deriving newtype (Functor, Applicative, Monad, MonadRandom, MonadIO, MonadReader ScriptState)

instance ReverseQueue ScriptT

instance CardGen ScriptT where
  genEncounterCard a = ScriptT $ lift $ genEncounterCard a
  genPlayerCard a = ScriptT $ lift $ genPlayerCard a
  replaceCard cid card = ScriptT $ lift $ replaceCard cid card
  clearCardCache = ScriptT $ lift clearCardCache

instance HasGame ScriptT where
  getGame = ScriptT $ lift getGame

instance HasQueue Message (ScriptT) where
  messageQueue = ScriptT $ lift messageQueue
  pushAll = ScriptT . lift . pushAll

script :: (Entity a, attrs ~ EntityAttrs a, RunMessage attrs) => ((?this :: a, ?msg :: Message) => ScriptT ()) -> Message -> a -> GameT a
script body msg a = 
  let ?msg = msg
      ?this = a
  in do 
    runQueueT do
      void $ runReaderT (runScriptT body) (ScriptState False True)
      overAttrsM (liftRunMessage msg) a

revelation :: (?msg :: Message, ?this :: a, Entity a, b ~ EntityAttrs a, Sourceable b) => ((?iid :: InvestigatorId) => ScriptT ()) -> ScriptT ()
revelation body = do
  case ?msg of
    Revelation iid (isSource attrs -> True) -> do
      local (\st -> st { shouldRun = True, isRevelation = True }) $ let ?iid = iid in body
    _ -> terminate $ let ?iid = "00000" in body

skillTest :: (?iid :: InvestigatorId, ?this :: a, Entity a, b ~ EntityAttrs a, Sourceable b, Targetable b) => SkillType -> GameCalculation -> ((?sid :: SkillTestId) => ScriptT ()) -> ScriptT ()
skillTest sType amount body = do
  st <- ask
  sid <- getRandom
  let ?sid = sid in body
  unterminated $ if st.isRevelation
    then Msg.revelationSkillTest sid you attrs sType amount
    else Msg.beginSkillTest sid you attrs attrs sType amount

unterminated :: ScriptT () -> ScriptT ()
unterminated action = do
  st <- ask
  when st.shouldRun action

terminate :: ScriptT () -> ScriptT ()
terminate action = local (\st -> st { shouldRun = False }) action

resume :: ScriptT () -> ScriptT ()
resume action = local (\st -> st { shouldRun = True }) action

failOnReveal :: (?sid :: SkillTestId, ?this :: a, Entity a, b ~ EntityAttrs a, Targetable b, Sourceable b) => ChaosTokenMatcher -> ScriptT ()
failOnReveal matcher = unterminated $ Msg.failOnReveal matcher ?sid attrs

you :: (?iid :: InvestigatorId) => InvestigatorId
you = ?iid

attrs :: (?this :: a, Entity a, b ~ EntityAttrs a) => b
attrs = toAttrs ?this

this :: (?this :: a) => a
this = ?this

onFail :: (?msg :: Message, ?this :: a, Entity a, b ~ EntityAttrs a, Sourceable b) => ((?iid :: InvestigatorId) => ScriptT ()) -> ScriptT ()
onFail body = do
  case ?msg of
    FailedThisSkillTest iid (isSource attrs -> True) -> resume $ let ?iid = iid in body
    _ -> terminate $ let ?iid = "00000" in body

takeHorror :: (?this :: a, Entity a, b ~ EntityAttrs a, Sourceable b) => InvestigatorId -> Int -> ScriptT ()
takeHorror iid n = unterminated $ Msg.assignHorror iid attrs n
