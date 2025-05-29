{-# LANGUAGE ImplicitParams #-}

module Arkham.Script where

import Arkham.Calculation
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Classes.RunMessage
import Arkham.Effect.Builder
import Arkham.Effect.Types
import Arkham.Effect.Window
import Arkham.GameT
import Arkham.Helpers.SkillTest.Lifted qualified as Msg (revelationSkillTest)
import Arkham.Helpers.Window qualified as Window
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Message.Lifted qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Message.Lifted.Queue
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Queue
import Arkham.SkillType
import Arkham.Investigator.Projection ()
import Arkham.Slot
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Trait
import Arkham.Window (Window)
import Arkham.Window qualified as Window
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import GHC.Records

newtype ScriptState = ScriptState {scriptStateInHand :: Bool}

instance HasGame m => HasGame (ReaderT ScriptState m) where
  getGame = lift getGame

initScriptState :: ScriptState
initScriptState = ScriptState False

data CardHandler a = CardHandler
  { matchMessage :: Message -> Bool
  , handleMessage :: Message -> ScriptT a ()
  }

newtype ScriptT b a = Script
  { runScriptT :: ReaderT ScriptState (WriterT [CardHandler b] (StateT b (QueueT Message GameT))) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadState b
    , MonadWriter [CardHandler b]
    , MonadReader ScriptState
    , MonadIO
    , HasGame
    , CardGen
    , MonadRandom
    )

instance ReverseQueue (ScriptT a) where
  filterInbox = Script . lift . lift . lift . filterInbox

instance HasQueue Message (ScriptT a) where
  messageQueue = Script messageQueue
  pushAll = Script . pushAll

runScript :: ScriptT a () -> a -> QueueT Message GameT a
runScript s a = execStateT (void $ runWriterT $ runReaderT (runScriptT s) initScriptState) a

you :: (?you :: InvestigatorId) => InvestigatorId
you = ?you

this :: (?this :: a) => a
this = ?this

ability :: (?ability :: Source) => Source
ability = ?ability

source :: (?source :: Source) => Source
source = ?source

windows :: (?windows :: [Window]) => [Window]
windows = ?windows

yourLocation :: (?you :: InvestigatorId) => LocationMatcher
yourLocation = locationWithInvestigator ?you

onMessage :: (Message -> Bool) -> (Message -> ScriptT a ()) -> ScriptT a ()
onMessage match h = Script $ tell [CardHandler match h]

revelation
  :: (?this :: a, Sourceable (EntityAttrs a), Entity a)
  => ((?you :: InvestigatorId, ?source :: Source, ?revelation :: Bool) => ScriptT a ()) -> ScriptT a ()
revelation handler = onMessage matchHandler \case
  Revelation iid s | isSource (toAttrs ?this) s -> let ?you = iid; ?source = s; ?revelation = True in handler
  _ -> pure ()
 where
  matchHandler (Revelation _ s) = isSource (toAttrs ?this) s
  matchHandler _ = False

additionalSlots
  :: forall a attrs
   . ( ?this :: a
     , Entity a
     , RunType attrs ~ attrs
     , attrs ~ EntityAttrs a
     , RunMessage attrs
     , HasField "cardId" attrs CardId
     )
  => SlotType -> Int -> Slot -> ScriptT a ()
additionalSlots sType n slot = onMessage @a matchHandler \case
  msg@(CardIsEnteringPlay iid card) | card.id == (toAttrs this).cardId -> do
    pushAll $ replicate n (AddSlot iid sType slot)
    put =<< (Script (lift $ lift $ lift $ overAttrsM (liftRunMessage msg) this) :: ScriptT a a)
  _ -> pure ()
 where
  matchHandler (CardIsEnteringPlay _ card) = card.id == (toAttrs this).cardId
  matchHandler _ = False

inHand :: ScriptT a () -> ScriptT a ()
inHand body = local (\s -> s {scriptStateInHand = True}) body

onAbility
  :: ( ?this :: a
     , HasField "ability" (EntityAttrs a) (Int -> Source)
     , Entity a
     , Sourceable (EntityAttrs a)
     )
  => Int
  -> ( ( ?this :: a
       , ?msg :: Message
       , ?you :: InvestigatorId
       , ?ability :: Source
       , ?source :: Source
       , ?windows :: [Window]
       )
       => ScriptT a ()
     )
  -> ScriptT a ()
onAbility n handler = do
  ss <- ask
  onMessage (matchHandler ss) \case
    msg@(UseCardAbility iid s n' ws p)
      | not (scriptStateInHand ss) && isSource (toAttrs ?this) s && n' == n ->
          let ?you = iid
              ?source = (toAttrs ?this).ability n
              ?msg = msg
              ?windows = ws
              ?payment = p
              ?ability = (toAttrs ?this).ability n
           in handler
    InHand iid' msg@(UseCardAbility iid s n' ws p)
      | scriptStateInHand ss && iid' == iid && isSource (toAttrs ?this) s && n' == n ->
          let ?you = iid
              ?source = (toAttrs ?this).ability n
              ?msg = msg
              ?windows = ws
              ?payment = p
              ?ability = (toAttrs ?this).ability n
           in handler
    _ -> pure ()
 where
  matchHandler ss (UseThisAbility _ s n') =
    not (scriptStateInHand ss)
      && isSource (toAttrs ?this) s
      && n'
      == n
  matchHandler ss (InHand iid' (UseThisAbility iid s n')) =
    scriptStateInHand ss
      && iid'
      == iid
      && isSource (toAttrs ?this) s
      && n'
      == n
  matchHandler _ _ = False

onAbilityThen
  :: ( ?this :: a
     , HasField "ability" (EntityAttrs a) (Int -> Source)
     , Entity a
     , Sourceable (EntityAttrs a)
     )
  => Int
  -> ( (?this :: a, ?msg :: Message, ?you :: InvestigatorId, ?ability :: Source, ?source :: Source)
       => ScriptT a ()
     )
  -> ( (?this :: a, ?msg :: Message, ?you :: InvestigatorId, ?ability :: Source, ?source :: Source)
       => ScriptT a ()
     )
  -> ScriptT a ()
onAbilityThen n handler thenHandler = onMessage matchHandler \case
  msg@(UseCardAbility iid s n' ws p)
    | isSource (toAttrs ?this) s && n' == n ->
        let ?you = iid
            ?source = (toAttrs ?this).ability n
            ?msg = msg
            ?windows = ws
            ?payment = p
            ?ability = (toAttrs ?this).ability n
         in handler >> Msg.doStep 1 msg
  DoStep 1 msg@(UseCardAbility iid s n' ws p)
    | isSource (toAttrs ?this) s && n' == n ->
        let ?you = iid
            ?source = (toAttrs ?this).ability n
            ?msg = msg
            ?windows = ws
            ?payment = p
            ?ability = (toAttrs ?this).ability n
         in thenHandler
  _ -> pure ()
 where
  matchHandler (UseThisAbility _ s n') = isSource (toAttrs ?this) s && n' == n
  matchHandler (DoStep 1 (UseThisAbility _ s n')) = isSource (toAttrs ?this) s && n' == n
  matchHandler _ = False

script
  :: (Entity a, attrs ~ EntityAttrs a, RunMessage attrs, RunType attrs ~ attrs)
  => ((?this :: a) => ScriptT a ()) -> Message -> a -> GameT a
script dsl msg a = runQueueT do
  let ?msg = msg
  let ?this = a
  let ?revelation = False
  handlers <- evalStateT (execWriterT $ runReaderT (runScriptT dsl) initScriptState) a
  let result = foldr (\h acc -> acc <|> runHandler h msg) Nothing handlers
  fromMaybe (overAttrsM (liftRunMessage msg) a) result
 where
  runHandler (CardHandler match h) m
    | match m = Just $ let ?this = a in runScript (h m) a
    | otherwise = Nothing

holds
  :: (?this :: a, Entity a, attrs ~ EntityAttrs a, Sourceable attrs, MonoFoldable t, Element t ~ Trait)
  => t -> Slot
holds ts = RestrictedSlot (toSource $ toAttrs ?this) (mapOneOf CardWithTrait ts) []

moveTokens
  :: (?source :: Source, Sourceable from, Targetable onto)
  => from -> onto -> Token -> Int -> ScriptT a ()
moveTokens from onto = Msg.moveTokens ?source (toSource from) (toTarget onto)

discardThis
  :: ( ?this :: a
     , ?you :: InvestigatorId
     , ?source :: Source
     , Entity a
     , attrs ~ EntityAttrs a
     , Targetable attrs
     )
  => ScriptT a ()
discardThis = Msg.toDiscardBy you ?source (toAttrs this)

class CanBeInThreat ref where
  placeInYourThreatArea
    :: ( ?you :: InvestigatorId
       , ?this :: a
       , Placeable attrs
       , Entity a
       , attrs ~ EntityAttrs a
       , AsId attrs
       , IdOf attrs ~ ref
       )
    => ScriptT a ()
  placeInYourThreatArea = place (toAttrs this) (InThreatArea you)

instance CanBeInThreat TreacheryId
instance CanBeInThreat AssetId

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

chooseAmong
  :: ( ?ability :: Source
     , ?this :: x
     , ChooseAmong a
     , el ~ ChosenType a
     , Targetable el
     , ?you :: InvestigatorId
     , ?source :: Source
     )
  => a
  -> ( (?you :: InvestigatorId, ?ability :: Source, ?source :: Source, ?this :: x)
       => el -> QueueT Message (ScriptT x) ()
     )
  -> ScriptT x ()
chooseAmong a body = do
  xs <- toChooseAmong a
  chooseTargetM you xs body

chooseEnemy
  :: (?ability :: Source, ?this :: x, ?you :: InvestigatorId, ?source :: Source)
  => EnemyMatcher
  -> ( (?you :: InvestigatorId, ?ability :: Source, ?source :: Source, ?this :: x)
       => EnemyId -> QueueT Message (ScriptT x) ()
     )
  -> ScriptT x ()
chooseEnemy = chooseAmong

elderSignEffect
  :: (?this :: a, Entity a, Is (EntityAttrs a) InvestigatorId)
  => ( ( ?this :: a
       , ?source :: Source
       , ?you :: InvestigatorId
       , ?msg :: Message
       , ?windows :: [Window]
       , ?ability :: Source
       )
       => ScriptT a ()
     )
  -> ScriptT a ()
elderSignEffect body = onMessage matchHandler \case
  msg@(ResolveChaosToken _ ElderSign iid)
    | is (toAttrs this) iid ->
        let ?you = iid
            ?source = toSource ElderSign
            ?msg = msg
            ?windows = []
            ?ability = (toSource ElderSign)
         in body
  _ -> pure ()
 where
  matchHandler (ResolveChaosToken _ ElderSign iid) = is (toAttrs this) iid
  matchHandler _ = False

revealedChaosTokens :: (?windows :: [Window]) => [ChaosToken]
revealedChaosTokens = Window.revealedChaosTokens ?windows

discoveredClues :: (?windows :: [Window]) => Int
discoveredClues = Window.discoveredClues ?windows

drawAnotherChaosToken :: (?you :: InvestigatorId) => ScriptT a ()
drawAnotherChaosToken = Msg.drawAnotherChaosToken you

chooseFightEnemyWithModifiers
  :: (ReverseQueue m, Sourceable source, ?you :: InvestigatorId)
  => SkillTestId
  -> source
  -> [ModifierType]
  -> m ()
chooseFightEnemyWithModifiers sid = Msg.chooseFightEnemyWithModifiers sid you

class WithClues a where
  withClues :: a

instance WithClues LocationMatcher where
  withClues = LocationWithAnyClues

passedWithElderSign
  :: (?this :: a, Entity a, Is attrs InvestigatorId, attrs ~ EntityAttrs a)
  => ((?this :: a, ?source :: Source, ?you :: InvestigatorId) => ScriptT a ()) -> ScriptT a ()
passedWithElderSign body = onMessage matchHandler \case
  PassedSkillTest iid _ _ (ChaosTokenTarget (chaosTokenFace -> ElderSign)) _ _
    | is (toAttrs this) iid ->
        let ?you = iid; ?source = toSource ElderSign in body
  _ -> pure ()
 where
  matchHandler (PassedSkillTest iid _ _ (ChaosTokenTarget (chaosTokenFace -> ElderSign)) _ _) = is (toAttrs this) iid
  matchHandler _ = False

newtype FightDetails = FightDetails {fightDetailsSkillTestId :: SkillTestId}

newtype FightT m a = FightT {runFightT :: StateT FightDetails m a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadState FightDetails
    , HasGame
    , MonadTrans
    , CardGen
    , MonadRandom
    , MonadIO
    )

instance HasQueue msg m => HasQueue msg (FightT m) where
  messageQueue = lift messageQueue
  pushAll = lift . pushAll

instance ReverseQueue m => ReverseQueue (FightT m) where
  filterInbox = lift . filterInbox

instance WithEffect (ScriptT a)

instance HasGame m => WithEffect (FightT m) where
  effect target body = do
    effectId <- getRandom
    details <- get
    builder <-
      execStateT
        (runEffectBuilder $ during (EffectSkillTestWindow (fightDetailsSkillTestId details)) >> body)
        =<< makeEffectBuilder "genef" Nothing ?source target
    push
      $ CreateEffect
        builder
          { effectBuilderEffectId = Just effectId
          , effectBuilderSkillTest = Just (fightDetailsSkillTestId details)
          }

fight
  :: (?you :: InvestigatorId, ?source :: source, ReverseQueue m, Sourceable source)
  => FightT m () -> m ()
fight body = do
  sid <- getRandom
  let ?skillTestId = sid in evalStateT (runFightT body) (FightDetails sid)
  Msg.chooseFightEnemy sid ?you ?source

sufferMentalTrauma :: (?you :: InvestigatorId) => Int -> ScriptT x ()
sufferMentalTrauma = Msg.sufferMentalTrauma you

insteadOfDiscoveringClues :: (?you :: InvestigatorId) => ScriptT a () -> ScriptT a ()
insteadOfDiscoveringClues body = do
  Script $ lift $ lift $ lift $ lift $ popMessageMatching_ \case
    DoStep 1 (Arkham.Message.DiscoverClues iid' _) -> you == iid'
    _ -> False
  body

discardTokens
  :: (?source :: source, ?this :: a, Sourceable source, Entity a, Targetable (EntityAttrs a))
  => Token -> Int -> ScriptT a ()
discardTokens t n = push $ RemoveTokens (toSource ?source) (toTarget $ toAttrs ?this) t n

cancelChaosToken
  :: (?you :: InvestigatorId, Sourceable source) => source -> ChaosToken -> ScriptT a ()
cancelChaosToken s token = Script $ lift $ lift $ lift $ Msg.cancelChaosToken (toSource s) ?you token

onPlay
  :: (?this :: a, Entity a, Is (EntityAttrs a) EventId, HasField "windows" (EntityAttrs a) [Window])
  => ((?you :: InvestigatorId, ?source :: Source, ?windows :: [Window]) => ScriptT a ()) -> ScriptT a ()
onPlay handler = onMessage matchHandler \case
  PlayThisEvent iid eid
    | is (toAttrs ?this) eid ->
        let ?you = iid; ?source = EventSource eid; ?windows = (toAttrs ?this).windows in handler
  _ -> pure ()
 where
  matchHandler (PlayThisEvent _ eid) = is (toAttrs ?this) eid
  matchHandler _ = False

removeDiscardFromGame :: (?you :: InvestigatorId) => ScriptT a ()
removeDiscardFromGame = push $ RemoveDiscardFromGame ?you

directHorror :: (?this :: a, Sourceable a) => InvestigatorId -> Int -> ScriptT a ()
directHorror iid n = Msg.directHorror iid (toSource ?this) n

gainResources
  :: (?source :: Source, AsId investigator, IdOf investigator ~ InvestigatorId)
  => investigator -> Int -> ScriptT a ()
gainResources investigator n = Msg.gainResources investigator ?source n

gainActions
  :: (?source :: Source, AsId investigator, IdOf investigator ~ InvestigatorId)
  => investigator -> Int -> ScriptT a ()
gainActions investigator n = Msg.gainActions investigator ?source n

healDamage
  :: (?source :: Source, Targetable target) => target -> Int -> ScriptT a ()
healDamage target n = Msg.healDamage (toTarget target) ?source n

healHorror
  :: (?source :: Source, Targetable target) => target -> Int -> ScriptT a ()
healHorror target n = Msg.healHorror (toTarget target) ?source n

drawnCard :: (?windows :: [Window]) => Window.DrawnCard
drawnCard = Window.drawnCard ?windows

shuffleDrawnCardBackIntoDeck :: (?windows :: [Window]) => ScriptT a ()
shuffleDrawnCardBackIntoDeck = Msg.shuffleCardsIntoDeck drawnCard.drawnFrom drawnCard

cancelCardDraw :: (?windows :: [Window], ?source :: Source) => ScriptT a ()
cancelCardDraw = Script $ lift $ lift $ lift do
  Msg.cancelCardDraw ?source (Window.cardDrawn windows)

yourNextSkillTest :: (?you :: InvestigatorId) => EffectWindow
yourNextSkillTest = #nextSkillTest ?you

endOfCurrentPhase :: EffectWindow
endOfCurrentPhase = #endOfCurrentPhase

test
  :: (?source :: Source, ?you :: InvestigatorId, ?revelation :: Bool)
  => SkillType -> GameCalculation -> ScriptT a () -> ScriptT a ()
test skillType calculation body = do
  sid <- getRandom
  let ?skillTestId = sid
  if ?revelation
    then Script $ lift $ lift $ lift $ Msg.revelationSkillTest sid you source skillType calculation
    else Script $ lift $ lift $ lift $ Msg.beginSkillTest sid you source you skillType calculation
  body

onFail
  :: (?this :: a, Sourceable (EntityAttrs a), Entity a)
  => ((?you :: InvestigatorId, ?source :: Source) => ScriptT a ()) -> ScriptT a ()
onFail handler = onMessage matchHandler \case
  FailedThisSkillTest iid s | isSource (toAttrs ?this) s -> let ?you = iid; ?source = s in handler
  _ -> pure ()
 where
  matchHandler (FailedThisSkillTest _ s) = isSource (toAttrs ?this) s
  matchHandler _ = False

takeDamage :: (?source :: Source, ?you :: InvestigatorId) => Int -> ScriptT a ()
takeDamage n = Script $ lift $ lift $ lift $ Msg.assignDamage you ?source n

horrorOn :: InvestigatorId -> ScriptT a Int
horrorOn iid = Script $ lift $ lift $ lift iid.horror

sanityOf :: InvestigatorId -> ScriptT a Int
sanityOf iid = Script $ lift $ lift $ lift iid.sanity
