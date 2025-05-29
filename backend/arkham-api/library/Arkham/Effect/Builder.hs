{-# LANGUAGE ImplicitParams #-}

module Arkham.Effect.Builder (
  module X,
  module Arkham.Effect.Builder,
) where

import Arkham.Duration as X

import Arkham.Calculation
import Arkham.Calculation.IsCalculation
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Criteria
import Arkham.Effect.Types
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.GameT
import Arkham.Helpers.Modifiers (toModifiers)
import Arkham.Id
import Arkham.Matcher.Enemy
import Arkham.Matcher.Location
import Arkham.Message (Message (CreateEffect))
import Arkham.Message.Lifted.Queue
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Queue
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Control.Monad.State.Strict

newtype EffectBuilderT m a = EffectBuilderT {runEffectBuilder :: StateT EffectBuilder m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState EffectBuilder)

class WithEffect m where
  effect
    :: (?source :: source, ReverseQueue m, Targetable target, Sourceable source)
    => target -> EffectBuilderT m () -> m ()
  effect target body = do
    effectId <- getRandom
    builder <- execStateT (runEffectBuilder body) =<< makeEffectBuilder "genef" Nothing ?source target
    push $ CreateEffect builder {effectBuilderEffectId = Just effectId}

instance WithEffect GameT
instance WithEffect m => WithEffect (QueueT Message m)
instance WithEffect m => WithEffect (StateT s m)

withSource :: Sourceable source => source -> ((?source :: source, Sourceable source) => b) -> b
withSource source inner = let ?source = source in inner

withYou :: InvestigatorId -> ((?you :: InvestigatorId) => b) -> b
withYou iid inner = let ?you = iid in inner

enableOn :: Monad m => EffectWindow -> EffectBuilderT m ()
enableOn window = EffectBuilderT $ do
  b <- get
  put $ b {effectBuilderWindow = Just $ combine window (effectBuilderWindow b)}
 where
  combine w = \case
    Just (FirstEffectWindow ws) -> FirstEffectWindow $ w : ws
    Just ws -> FirstEffectWindow [w, ws]
    Nothing -> w

removeOn :: Monad m => EffectWindow -> EffectBuilderT m ()
removeOn window = EffectBuilderT $ do
  b <- get
  put $ b {effectBuilderDisableWindow = Just $ combine window (effectBuilderDisableWindow b)}
 where
  combine w = \case
    Just (FirstEffectWindow ws) -> FirstEffectWindow $ w : ws
    Just ws -> FirstEffectWindow [w, ws]
    Nothing -> w

onDisable :: ReverseQueue m => QueueT Message m () -> EffectBuilderT m ()
onDisable body = EffectBuilderT do
  msgs <- lift $ evalQueueT body
  b <- get
  put $ b {effectBuilderOnDisable = Just $ maybe msgs (<> msgs) (effectBuilderOnDisable b)}

instance (Monad m, a ~ ()) => Duration (EffectBuilderT m a) where
  type DurationOf (EffectBuilderT m a) = EffectWindow
  during window = do
    enableOn window
    removeOn window

applyWhen :: HasGame m => Criterion -> EffectBuilderT m () -> EffectBuilderT m ()
applyWhen c inner = do
  builder <- EffectBuilderT $ StateT $ \s ->
    (,s)
      <$> execStateT (runEffectBuilder inner) ((s :: EffectBuilder) {effectBuilderMetadata = Nothing})
  case effectBuilderMetadata builder of
    Just (EffectModifiers mods) -> do
      for_ mods $ apply . CriteriaModifier c . modifierType
    _ -> pure ()

whenAttackedEnemy :: HasGame m => EffectBuilderT m () -> EnemyMatcher -> EffectBuilderT m ()
whenAttackedEnemy body matcher = applyWhen (exists $ AttackedEnemy <> matcher) body

apply :: HasGame m => ModifierType -> EffectBuilderT m ()
apply modKind = EffectBuilderT $ do
  b <- get
  meta <- addModifier b
  put $ b {effectBuilderMetadata = meta}
 where
  addModifier b = do
    mods <- toModifiers (effectBuilderSource b) [modKind]
    pure $ case effectBuilderMetadata b of
      Just (EffectModifiers mods') ->
        Just $ EffectModifiers $ mods <> mods'
      _ -> Just $ EffectModifiers mods

damageDealt :: HasGame m => Int -> EffectBuilderT m ()
damageDealt n = apply $ DamageDealt n

combat :: HasGame m => GameCalculation -> EffectBuilderT m ()
combat = modifySkill #combat

modifySkill :: HasGame m => SkillType -> GameCalculation -> EffectBuilderT m ()
modifySkill sKind calc = apply $ CalculatedSkillModifier sKind calc

class IfLocation a where
  ifLocation :: LocationMatcher -> a

instance (IsCalculation a, IsCalculation b) => IfLocation (a -> b -> GameCalculation) where
  ifLocation matcher a b = IfLocationExistsCalculation matcher (toCalculation a) (toCalculation b)
