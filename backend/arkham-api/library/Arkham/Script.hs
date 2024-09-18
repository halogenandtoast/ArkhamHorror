{-# LANGUAGE ImplicitParams #-}
module Arkham.Script where

import Arkham.Treachery.Import.Lifted qualified as Msg
import Arkham.Prelude hiding (handle)
import Arkham.Queue
import Arkham.Discover (IsInvestigate(..))
import Arkham.Projection
import Arkham.Investigator.Types (Field(..))
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
import Control.Monad.State

data YouKind = Known | Unknown
data You (a :: YouKind) where
  YouKnown :: InvestigatorId -> You 'Known
  YouUnknown :: You 'Unknown

data ScriptEnv = ScriptEnv { isRevelation :: Bool, shouldRun :: Bool }
data ScriptState a = ScriptState { obj :: a, handled :: Bool }

newtype ScriptT x a = ScriptT { runScriptT :: ReaderT ScriptEnv (StateT (ScriptState x) (QueueT Message GameT)) a }
  deriving newtype (Functor, Applicative, Monad, MonadRandom, MonadIO, MonadReader ScriptEnv, MonadState (ScriptState x))

instance ReverseQueue (ScriptT a)

instance CardGen (ScriptT a) where
  genEncounterCard a = ScriptT $ lift $ genEncounterCard a
  genPlayerCard a = ScriptT $ lift $ genPlayerCard a
  replaceCard cid card = ScriptT $ lift $ replaceCard cid card
  clearCardCache = ScriptT $ lift clearCardCache

instance HasGame (ScriptT a) where
  getGame = ScriptT $ lift getGame

instance HasQueue Message (ScriptT a) where
  messageQueue = ScriptT $ lift messageQueue
  pushAll = ScriptT . lift . pushAll

script :: (Entity a, attrs ~ EntityAttrs a, RunMessage attrs) => ((?this :: a, ?msg :: Message) => ScriptT a ()) -> Message -> a -> GameT a
script body msg' a = 
  let ?msg = msg'
      ?this = a
  in do 
    runQueueT do
      ScriptState { obj, handled } <- execStateT (runReaderT (runScriptT body)(ScriptEnv False True)) (ScriptState a False)
      if handled then pure obj else overAttrsM (liftRunMessage msg) a

handle :: ScriptT x () -> ScriptT x ()
handle body = modify (\st -> st { handled = True }) >> body

revelation :: (?msg :: Message, ?this :: a, Entity a, b ~ EntityAttrs a, Sourceable b) => (forall (kind :: YouKind). (?you :: You kind) => ScriptT x ()) -> ScriptT x ()
revelation body = do
  case ?msg of
    Revelation iid (isSource attrs -> True) -> handle do
      local (\st -> st { shouldRun = True, isRevelation = True }) $ let ?you = YouKnown iid in body
    _ -> terminate $ let ?you = YouUnknown in body

class IsCalculation a where
  toCalculation :: a -> GameCalculation

instance IsCalculation GameCalculation where
  toCalculation = id

instance IsCalculation Int where
  toCalculation = Fixed

instance IsCalculation Integer where
  toCalculation = Fixed . fromIntegral

test :: (?you :: You kind, ?this :: a, IsCalculation c, Entity a, b ~ EntityAttrs a, Sourceable b, Targetable b) => SkillType -> c -> ((?sid :: SkillTestId) => ScriptT x ()) -> ScriptT x ()
test sType (toCalculation -> amount) body = do
  st <- ask
  sid <- getRandom
  let ?sid = sid in body
  unterminated $ knownYou do
    if st.isRevelation
      then Msg.revelationSkillTest sid you attrs sType amount
      else Msg.beginSkillTest sid you attrs attrs sType amount

unterminated :: ScriptT x () -> ScriptT x ()
unterminated action = do
  st <- ask
  when st.shouldRun action

terminate :: ScriptT x () -> ScriptT x ()
terminate action = local (\st -> st { shouldRun = False }) action

resume :: ScriptT x () -> ScriptT x ()
resume action = local (\st -> st { shouldRun = True }) action

failOnReveal :: (?sid :: SkillTestId, ?this :: a, Entity a, b ~ EntityAttrs a, Targetable b, Sourceable b) => ChaosTokenMatcher -> ScriptT x ()
failOnReveal matcher = unterminated $ Msg.failOnReveal matcher ?sid attrs

you :: (?you :: You 'Known) => InvestigatorId
you = case ?you of
  YouKnown iid -> iid

knownYou :: (?you :: You kind) => ((?you :: You 'Known) => ScriptT x ()) -> ScriptT x ()
knownYou body = case ?you of
  YouKnown _ -> body
  YouUnknown -> error "No investigator in scope"

attrs :: (?this :: a, Entity a, b ~ EntityAttrs a) => b
attrs = toAttrs ?this

this :: (?this :: a) => a
this = ?this

msg :: (?msg :: a) => a
msg = ?msg

source :: (?source :: a) => a
source = ?source

yourLocation :: (?you :: You kind) => ScriptT x (Maybe LocationId)
yourLocation = case ?you of
  YouKnown iid -> field InvestigatorLocation iid
  YouUnknown -> pure Nothing

at :: (ScriptT x (Maybe LocationId)) -> ((?lid :: LocationId) => ScriptT x ()) -> ScriptT x ()
at getLocation body = do
  mLocationId <- getLocation
  case mLocationId of
    Just lid -> let ?lid = lid in body
    Nothing -> pure ()

discoverClues :: (?you :: You kind, ?source :: Source) => Int -> ScriptT x (Maybe LocationId) -> ScriptT x ()
discoverClues n getLocation = unterminated $ knownYou do
  getLocation >>= \case 
    Just lid -> Msg.discoverAt NotInvestigate you ?source lid n
    Nothing -> pure ()

onFail :: (?msg :: Message, ?this :: a, Entity a, b ~ EntityAttrs a, Sourceable b) => ((?you :: You 'Known) => ScriptT x ()) -> ScriptT x ()
onFail body = do
  case ?msg of
    FailedThisSkillTest iid (isSource attrs -> True) -> resume $ handle $ let ?you = YouKnown iid in body
    _ -> pure ()

takeHorror :: (?this :: a, Entity a, b ~ EntityAttrs a, Sourceable b) => InvestigatorId -> Int -> ScriptT x ()
takeHorror iid n = unterminated $ Msg.assignHorror iid attrs n

ability :: (?msg :: Message, ?this :: a, Entity a, b ~ EntityAttrs a, Sourceable b) => Int -> ((?you :: You 'Known, ?source :: Source) => ScriptT x ()) -> ScriptT x ()
ability n body = case ?msg of
  UseThisAbility iid (isSource attrs -> True) m | m == n -> resume $ handle $ let { ?you = YouKnown iid; ?source = toAbilitySource attrs m } in body
  _ -> pure ()
