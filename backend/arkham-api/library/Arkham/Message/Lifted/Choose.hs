module Arkham.Message.Lifted.Choose where

import Arkham.Ability.Types
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Helpers.FlavorText (FlavorTextBuilder, buildFlavor)
import Arkham.Helpers.Query (getLead)
import Arkham.I18n
import Arkham.Id
import Arkham.Message (Message (Would), uiToRun)
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Query
import Arkham.Question
import Arkham.Queue
import Arkham.SkillType
import Arkham.Target
import Arkham.Text (FlavorText, toI18n)
import Arkham.Window (defaultWindows)
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict

data ChooseState = ChooseState
  { terminated :: Bool
  , label :: Maybe Text
  , labelCardCode :: Maybe CardCode
  }

newtype ChooseT m a = ChooseT {unChooseT :: StateT ChooseState (WriterT [UI Message] m) a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadWriter [UI Message]
    , MonadState ChooseState
    , MonadIO
    , MonadRandom
    , CardGen
    )

instance HasGame m => HasGame (ChooseT m) where
  getGame = lift getGame

instance MonadTrans ChooseT where
  lift = ChooseT . lift . lift

runChooseT :: ChooseT m a -> m ((a, ChooseState), [UI Message])
runChooseT = runWriterT . (`runStateT` ChooseState False Nothing Nothing) . unChooseT

leadChooseOneM :: ReverseQueue m => ChooseT m a -> m ()
leadChooseOneM choices = do
  lead <- getLead
  chooseOneM lead choices

chooseOneM :: ReverseQueue m => InvestigatorId -> ChooseT m a -> m ()
chooseOneM iid choices = do
  ((_, ChooseState {label, labelCardCode}), choices') <- runChooseT choices
  unless (null choices') do
    case label of
      Nothing -> chooseOne iid choices'
      Just l -> case labelCardCode of
        Nothing -> questionLabel l iid $ ChooseOne choices'
        Just cCode -> questionLabelWithCard l cCode iid $ ChooseOne choices'

chooseSomeM :: ReverseQueue m => InvestigatorId -> Text -> ChooseT m a -> m ()
chooseSomeM iid txt choices = do
  ((_, ChooseState {label}), choices') <- runChooseT choices
  unless (null choices') do
    case label of
      Nothing -> chooseSome iid txt choices'
      Just l -> questionLabel l iid $ ChooseSome (Done txt : choices')

chooseSome1M :: ReverseQueue m => InvestigatorId -> Text -> ChooseT m a -> m ()
chooseSome1M iid txt choices = do
  ((_, ChooseState {label}), choices') <- runChooseT choices
  unless (null choices') do
    case label of
      Nothing -> chooseSome1 iid txt choices'
      Just l -> questionLabel l iid $ ChooseSome1 txt choices'

chooseOneFromEachM :: ReverseQueue m => InvestigatorId -> [ChooseT m a] -> m ()
chooseOneFromEachM iid choices = do
  choices' <- traverse runChooseT choices
  unless (null choices') $ chooseOneFromEach iid $ map snd choices'

chooseOrRunOneM :: ReverseQueue m => InvestigatorId -> ChooseT m a -> m ()
chooseOrRunOneM iid choices = do
  ((_, ChooseState {label}), choices') <- runChooseT choices
  unless (null choices') do
    case label of
      Nothing -> chooseOrRunOne iid choices'
      Just l -> case choices' of
        [x] -> push $ uiToRun x
        _ -> questionLabel l iid $ ChooseOne choices'

chooseOrRunNM :: ReverseQueue m => InvestigatorId -> Int -> ChooseT m a -> m ()
chooseOrRunNM iid n choices = do
  (_, choices') <- runChooseT choices
  unless (null choices') $ chooseOrRunN iid n choices'

chooseOrRunOneAtATimeM :: ReverseQueue m => InvestigatorId -> ChooseT m a -> m ()
chooseOrRunOneAtATimeM iid choices = do
  ((_, ChooseState {label}), choices') <- runChooseT choices
  unless (null choices') do
    case label of
      Nothing -> chooseOrRunOneAtATime iid choices'
      Just l -> case choices' of
        [x] -> push $ uiToRun x
        _ -> questionLabel l iid $ ChooseOneAtATime choices'

chooseNM :: ReverseQueue m => InvestigatorId -> Int -> ChooseT m a -> m ()
chooseNM iid n choices = do
  when (n > 0) do
    ((_, ChooseState {label}), choices') <- runChooseT choices
    unless (null choices') do
      case label of
        Nothing -> chooseN iid n choices'
        Just l -> questionLabel l iid $ ChooseN n choices'

chooseUpToNM :: ReverseQueue m => InvestigatorId -> Int -> Text -> ChooseT m a -> m ()
chooseUpToNM iid n done choices = do
  (_, choices') <- runChooseT choices
  unless (null choices') $ chooseUpToN iid n done choices'

chooseOneAtATimeM :: ReverseQueue m => InvestigatorId -> ChooseT m a -> m ()
chooseOneAtATimeM iid choices = do
  (_, choices') <- runChooseT choices
  unless (null choices') $ chooseOneAtATime iid choices'

forcedWhen :: Monad m => Bool -> ChooseT m () -> ChooseT m ()
forcedWhen b action =
  if b
    then do
      censor id action
      modify $ \s -> s {terminated = True}
    else action

unterminated :: ReverseQueue m => ChooseT m () -> ChooseT m ()
unterminated action = do
  ChooseState {terminated} <- get
  unless terminated action

labeled :: ReverseQueue m => Text -> QueueT Message m () -> ChooseT m ()
labeled label action = unterminated do
  msgs <- lift $ evalQueueT action
  tell [Label label msgs]

labeled' :: (HasI18n, ReverseQueue m) => Text -> QueueT Message m () -> ChooseT m ()
labeled' label action = unterminated do
  msgs <- lift $ evalQueueT action
  tell [Label ("$" <> ikey ("label." <> label)) msgs]

skip :: ReverseQueue m => Text -> ChooseT m ()
skip = (`labeled` nothing)

gridLabeled :: ReverseQueue m => Text -> QueueT Message m () -> ChooseT m ()
gridLabeled label action = unterminated do
  msgs <- lift $ evalQueueT action
  tell [GridLabel label msgs]

portraitLabeled :: ReverseQueue m => InvestigatorId -> QueueT Message m () -> ChooseT m ()
portraitLabeled iid action = unterminated do
  msgs <- lift $ evalQueueT action
  tell [PortraitLabel iid msgs]

labeledI18n :: (HasI18n, ReverseQueue m) => Text -> QueueT Message m () -> ChooseT m ()
labeledI18n label action = unterminated do
  msgs <- lift $ evalQueueT action
  tell [Label ("$" <> scope "labels" (ikey label)) msgs]

damageLabeled :: ReverseQueue m => InvestigatorId -> QueueT Message m () -> ChooseT m ()
damageLabeled iid action = unterminated do
  msgs <- lift $ evalQueueT action
  tell [DamageLabel iid msgs]

resourceLabeled :: ReverseQueue m => InvestigatorId -> QueueT Message m () -> ChooseT m ()
resourceLabeled iid action = unterminated do
  msgs <- lift $ evalQueueT action
  tell [ResourceLabel iid msgs]

clueLabeled :: ReverseQueue m => InvestigatorId -> QueueT Message m () -> ChooseT m ()
clueLabeled iid action = unterminated do
  msgs <- lift $ evalQueueT action
  tell [ClueLabel iid msgs]

cardLabeled :: (ReverseQueue m, HasCardCode a) => a -> QueueT Message m () -> ChooseT m ()
cardLabeled a action = unterminated do
  msgs <- lift $ evalQueueT action
  tell [CardLabel (toCardCode a) msgs]

deckLabeled :: ReverseQueue m => InvestigatorId -> QueueT Message m () -> ChooseT m ()
deckLabeled iid action = unterminated do
  msgs <- lift $ evalQueueT action
  tell [ComponentLabel (InvestigatorDeckComponent iid) msgs]

abilityLabeled :: ReverseQueue m => InvestigatorId -> Ability -> QueueT Message m () -> ChooseT m ()
abilityLabeled iid ab action = unterminated do
  msgs <- lift $ evalQueueT action
  tell [AbilityLabel iid ab (defaultWindows iid) [] msgs]

abilityLabeledWithBefore
  :: ReverseQueue m => InvestigatorId -> Ability -> [Message] -> QueueT Message m () -> ChooseT m ()
abilityLabeledWithBefore iid ab beforeMsgs action = unterminated do
  msgs <- lift $ evalQueueT action
  tell [AbilityLabel iid ab [] beforeMsgs msgs]

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

evading
  :: (ReverseQueue m, AsId enemy, IdOf enemy ~ EnemyId) => enemy -> QueueT Message m () -> ChooseT m ()
evading enemy action = unterminated do
  msgs <- lift $ evalQueueT action
  tell [evadeLabel enemy msgs]

fighting
  :: (ReverseQueue m, AsId enemy, IdOf enemy ~ EnemyId) => enemy -> QueueT Message m () -> ChooseT m ()
fighting enemy action = unterminated do
  msgs <- lift $ evalQueueT action
  tell [fightLabel enemy msgs]

batching :: ReverseQueue m => BatchId -> QueueT Message m () -> QueueT Message m ()
batching batchId action = do
  msgs <- lift $ evalQueueT action
  push $ Would batchId msgs

targets
  :: (ReverseQueue m, Targetable target) => [target] -> (target -> QueueT Message m ()) -> ChooseT m ()
targets ts action = unterminated $ for_ ts \t -> targeting t (action t)

targetsM
  :: (ReverseQueue m, Targetable target) => m [target] -> (target -> QueueT Message m ()) -> ChooseT m ()
targetsM ts action = unterminated $ traverse_ (\t -> targeting t (action t)) =<< lift ts

chooseTargetM
  :: (ReverseQueue m, Targetable target)
  => InvestigatorId
  -> [target]
  -> (target -> QueueT Message m ())
  -> m ()
chooseTargetM iid ts action = chooseOneM iid $ unterminated $ for_ ts \t -> targeting t (action t)

chooseOrRunTargetM
  :: (ReverseQueue m, Targetable target)
  => InvestigatorId
  -> [target]
  -> (target -> QueueT Message m ())
  -> m ()
chooseOrRunTargetM iid ts action = chooseOrRunOneM iid $ unterminated $ for_ ts \t -> targeting t (action t)

chooseSelectM
  :: (ReverseQueue m, Targetable (QueryElement query), Query query)
  => InvestigatorId
  -> query
  -> (QueryElement query -> QueueT Message m ())
  -> m ()
chooseSelectM iid query action = do
  ts <- select query
  chooseOneM iid $ unterminated $ for_ ts \t -> targeting t (action t)

chooseFromM
  :: (ReverseQueue m, Query query, Targetable (QueryElement query))
  => InvestigatorId
  -> query
  -> (QueryElement query -> QueueT Message m ())
  -> m ()
chooseFromM iid matcher action = do
  ((_, ChooseState {label, labelCardCode}), choices') <-
    runChooseT $ traverse_ (\t -> targeting t (action t)) =<< select matcher
  unless (null choices') do
    case label of
      Nothing -> chooseOne iid choices'
      Just l -> case labelCardCode of
        Nothing -> questionLabel l iid $ ChooseOne choices'
        Just cCode -> questionLabelWithCard l cCode iid $ ChooseOne choices'

nothing :: Monad m => QueueT Message m ()
nothing = pure ()

questionLabeled :: ReverseQueue m => Text -> ChooseT m ()
questionLabeled label = modify $ \s -> s {Arkham.Message.Lifted.Choose.label = Just label}

questionLabeledCard :: (ReverseQueue m, HasCardCode a) => a -> ChooseT m ()
questionLabeledCard a = modify $ \s -> s {Arkham.Message.Lifted.Choose.labelCardCode = Just (toCardCode a)}

storyWithContinue :: ReverseQueue m => FlavorText -> Text -> m ()
storyWithContinue txt button = storyWithChooseOneM txt $ labeled button nothing

storyWithChooseOneM :: ReverseQueue m => FlavorText -> ChooseT m a -> m ()
storyWithChooseOneM txt choices = do
  (_, choices') <- runChooseT choices
  storyWithChooseOne txt choices'

storyWithContinue' :: ReverseQueue m => FlavorTextBuilder () -> Text -> m ()
storyWithContinue' builder button = storyWithChooseOneM' builder $ labeled button nothing

storyWithChooseOneM' :: ReverseQueue m => FlavorTextBuilder () -> ChooseT m a -> m ()
storyWithChooseOneM' builder choices = do
  (_, choices') <- runChooseT choices
  storyWithChooseOne (buildFlavor builder) choices'

chooseSome1M' :: (HasI18n, ReverseQueue m) => InvestigatorId -> Text -> ChooseT m a -> m ()
chooseSome1M' iid txt choices = do
  ((_, ChooseState {label}), choices') <- runChooseT choices
  unless (null choices') do
    case label of
      Nothing -> chooseSome1 iid (toI18n txt) choices'
      Just l -> questionLabel l iid $ ChooseSome1 (toI18n txt) choices'
