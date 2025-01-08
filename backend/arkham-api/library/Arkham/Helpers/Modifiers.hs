module Arkham.Helpers.Modifiers (
  module Arkham.Helpers.Modifiers,
  module X,
) where

import Arkham.Prelude

import Arkham.Ability.Types
import Arkham.Card
import Arkham.ChaosToken.Types
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Cost
import Arkham.Effect.Window
import Arkham.EffectMetadata
import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Ref
import Arkham.Id
import Arkham.Matcher.Types
import Arkham.Message
import Arkham.Modifier as X
import Arkham.Phase (Phase)
import Arkham.Placement
import Arkham.Query
import Arkham.Source
import Arkham.Target
import Arkham.Window (Window)
import Control.Lens (each, sumOf)
import Control.Monad.Trans.Class
import Control.Monad.Writer.Class
import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Monoidal.Strict (MonoidalMap (..))
import Data.Monoid (First (..))
import GHC.Records

withGrantedAction
  :: (HasGame m, Sourceable source)
  => InvestigatorId
  -> source
  -> (forall t. (MonadTrans t, HasGame (t m)) => t m a)
  -> m a
withGrantedAction iid source = withGrantedActions iid source 1

withGrantedActions
  :: (HasGame m, Sourceable source)
  => InvestigatorId
  -> source
  -> Int
  -> (forall t. (MonadTrans t, HasGame (t m)) => t m a)
  -> m a
withGrantedActions iid source n = withModifiers iid (toModifiers source [ActionCostModifier (-n)])

ignoreActionCost
  :: HasGame m
  => InvestigatorId
  -> (forall t. (MonadTrans t, HasGame (t m)) => t m a)
  -> m a
ignoreActionCost iid = withModifiers iid (toModifiers GameSource [ActionsAreFree])

ignoreCommitOneRestriction
  :: HasGame m
  => InvestigatorId
  -> (forall t. (MonadTrans t, HasGame (t m)) => t m a)
  -> m a
ignoreCommitOneRestriction iid = withModifiers iid (toModifiers GameSource [IgnoreCommitOneRestriction])

withModifiers
  :: (HasGame m, Targetable target)
  => target
  -> (m [Modifier])
  -> (forall t. (MonadTrans t, HasGame (t m)) => t m a)
  -> m a
withModifiers = withModifiers'

getCombinedModifiers :: forall m. HasGame m => [Target] -> m [ModifierType]
getCombinedModifiers targets = map modifierType . nub . concat <$> traverse getFullModifiers targets

getModifiers :: forall a m. (HasGame m, Targetable a) => a -> m [ModifierType]
getModifiers (toTarget -> target) = do
  ignoreCanModifiers <- getIgnoreCanModifiers
  let
    notCanModifier CanModify {} = False
    notCanModifier _ = True
    filterF = if ignoreCanModifiers then notCanModifier else const True
  filter filterF . map modifierType <$> getModifiers' target

getFullModifiers :: forall a m. (HasGame m, Targetable a) => a -> m [Modifier]
getFullModifiers (toTarget -> target) = do
  ignoreCanModifiers <- getIgnoreCanModifiers
  let
    notCanModifier CanModify {} = False
    notCanModifier _ = True
    filterF = if ignoreCanModifiers then notCanModifier else const True
  filter (filterF . modifierType) <$> getModifiers' target

getModifiers' :: (HasGame m, Targetable a) => a -> m [Modifier]
getModifiers' (toTarget -> BothTarget t1 t2) = do
  allMods <- getAllModifiers
  pure
    $ findWithDefault [] t1 allMods
    <> findWithDefault [] t2 allMods
    <> findWithDefault [] ThisTarget allMods
getModifiers' (toTarget -> target) = do
  allMods <- getAllModifiers
  pure $ findWithDefault [] ThisTarget allMods <> findWithDefault [] target allMods

hasModifier
  :: (HasGame m, Targetable a) => a -> ModifierType -> m Bool
hasModifier a m = (m `elem`) <$> getModifiers (toTarget a)

withoutModifier
  :: (HasGame m, Targetable a) => a -> ModifierType -> m Bool
withoutModifier a m = not <$> hasModifier a m

withoutModifiers
  :: (HasGame m, Targetable a) => a -> [ModifierType] -> m Bool
withoutModifiers a ms = all (`notElem` ms) <$> getModifiers (toTarget a)

toModifier :: (Sourceable a, HasGame m) => a -> ModifierType -> m Modifier
toModifier a mType = Modifier (toSource a) mType False <$> sourceToMaybeCard a

modifySelf
  :: (Targetable target, Sourceable target, HasGame m, MonadWriter (MonoidalMap Target [Modifier]) m)
  => target
  -> [ModifierType]
  -> m ()
modifySelf target mods = tell . MonoidalMap . singletonMap (toTarget target) =<< toModifiers target mods

modifySelf1
  :: (Targetable target, Sourceable target, HasGame m, MonadWriter (MonoidalMap Target [Modifier]) m)
  => target
  -> ModifierType
  -> m ()
modifySelf1 target mod1 = tell . MonoidalMap . singletonMap (toTarget target) =<< toModifiers target [mod1]

modifySelfMaybe
  :: (Targetable target, Sourceable target, HasGame m, MonadWriter (MonoidalMap Target [Modifier]) m)
  => target
  -> MaybeT m [ModifierType]
  -> m ()
modifySelfMaybe target body = do
  mods <- fromMaybe [] <$> runMaybeT body
  unless (null mods) $ tell . MonoidalMap . singletonMap (toTarget target) =<< toModifiers target mods

modifySelfWith
  :: (Targetable target, Sourceable target, HasGame m, MonadWriter (MonoidalMap Target [Modifier]) m)
  => target
  -> (Modifier -> Modifier)
  -> [ModifierType]
  -> m ()
modifySelfWith target f mods = tell . MonoidalMap . singletonMap (toTarget target) =<< toModifiersWith target f mods

modifySelfWhen
  :: (Targetable target, Sourceable target, HasGame m, MonadWriter (MonoidalMap Target [Modifier]) m)
  => target
  -> Bool
  -> [ModifierType]
  -> m ()
modifySelfWhen target cond mods =
  when cond do
    tell . MonoidalMap . singletonMap (toTarget target) =<< toModifiers target mods

modifySelfWhenM
  :: (Targetable target, Sourceable target, HasGame m, MonadWriter (MonoidalMap Target [Modifier]) m)
  => target
  -> m Bool
  -> [ModifierType]
  -> m ()
modifySelfWhenM target cond mods = do
  b <- cond
  modifySelfWhen target b mods

toModifiers :: (HasGame m, Sourceable a) => a -> [ModifierType] -> m [Modifier]
toModifiers = traverse . toModifier

modified :: (Sourceable a, HasGame m) => a -> [ModifierType] -> m [Modifier]
modified a = toModifiers a

modifyEach
  :: (HasGame m, Sourceable source, Targetable a, MonadWriter (MonoidalMap Target [Modifier]) m)
  => source
  -> [a]
  -> [ModifierType]
  -> m ()
modifyEach source xs mTypes = do
  mods <- toModifiers source mTypes
  tell $ MonoidalMap $ mapFromList $ map ((,mods) . toTarget) xs

modifyEachMap
  :: (HasGame m, Sourceable source, Targetable a, MonadWriter (MonoidalMap Target [Modifier]) m)
  => source
  -> [a]
  -> (a -> [ModifierType])
  -> m ()
modifyEachMap source xs f = do
  tell . MonoidalMap . mapFromList =<< for xs \x -> (toTarget x,) <$> toModifiers source (f x)

modifyEachMapM
  :: (HasGame m, Sourceable source, Targetable a, MonadWriter (MonoidalMap Target [Modifier]) m)
  => source
  -> [a]
  -> (a -> m [ModifierType])
  -> m ()
modifyEachMapM source xs f = do
  tell . MonoidalMap . mapFromList =<< for xs \x -> (toTarget x,) <$> (toModifiers source =<< f x)

modifyEachMaybe
  :: (HasGame m, Sourceable source, Targetable a, MonadWriter (MonoidalMap Target [Modifier]) m)
  => source
  -> [a]
  -> (a -> MaybeT m [ModifierType])
  -> m ()
modifyEachMaybe source xs body = do
  tell . MonoidalMap . mapFromList =<< forMaybeM xs \x -> runMaybeT do
    mods <- body x
    guard $ notNull mods
    (toTarget x,) <$> lift (toModifiers source mods)

modifyEachWith
  :: (HasGame m, Sourceable source, Targetable a, MonadWriter (MonoidalMap Target [Modifier]) m)
  => source
  -> [a]
  -> (Modifier -> Modifier)
  -> [ModifierType]
  -> m ()
modifyEachWith source xs f mTypes = do
  mods <- toModifiersWith source f mTypes
  tell $ MonoidalMap $ mapFromList $ map ((,mods) . toTarget) xs

modifySelect
  :: ( HasGame m
     , Sourceable source
     , Targetable el
     , el ~ QueryElement query
     , Query query
     , MonadWriter (MonoidalMap Target [Modifier]) m
     )
  => source
  -> query
  -> [ModifierType]
  -> m ()
modifySelect _ _ [] = pure mempty
modifySelect source q mtypes = do
  xs <- select q
  modifyEach source xs mtypes

modifySelectMap
  :: ( HasGame m
     , Sourceable source
     , Targetable el
     , el ~ QueryElement query
     , Query query
     , MonadWriter (MonoidalMap Target [Modifier]) m
     )
  => source
  -> query
  -> (el -> [ModifierType])
  -> m ()
modifySelectMap source q f = do
  xs <- select q
  modifyEachMap source xs f

modifySelectMapM
  :: ( HasGame m
     , Sourceable source
     , Targetable el
     , el ~ QueryElement query
     , Query query
     , MonadWriter (MonoidalMap Target [Modifier]) m
     )
  => source
  -> query
  -> (el -> m [ModifierType])
  -> m ()
modifySelectMapM source q f = do
  xs <- select q
  modifyEachMapM source xs f

modifySelectWith
  :: ( HasGame m
     , Sourceable source
     , Targetable el
     , el ~ QueryElement query
     , Query query
     , MonadWriter (MonoidalMap Target [Modifier]) m
     )
  => source
  -> query
  -> (Modifier -> Modifier)
  -> [ModifierType]
  -> m ()
modifySelectWith source q f mtypes = do
  xs <- select q
  modifyEachWith source xs f mtypes

modifySelectWhen
  :: ( HasGame m
     , Sourceable source
     , Targetable el
     , el ~ QueryElement query
     , Query query
     , MonadWriter (MonoidalMap Target [Modifier]) m
     )
  => source
  -> Bool
  -> query
  -> [ModifierType]
  -> m ()
modifySelectWhen source cond q mtypes = when cond $ modifySelect source q mtypes

modifySelectMaybe
  :: ( HasGame m
     , Sourceable source
     , Targetable el
     , el ~ QueryElement query
     , Query query
     , MonadWriter (MonoidalMap Target [Modifier]) m
     )
  => source
  -> query
  -> (el -> MaybeT m [ModifierType])
  -> m ()
modifySelectMaybe source q body = do
  xs <- select q
  for_ xs \x -> maybeModified_ source x (body x)

modifySelectMaybeWith
  :: ( HasGame m
     , Sourceable source
     , Targetable el
     , el ~ QueryElement query
     , Query query
     , MonadWriter (MonoidalMap Target [Modifier]) m
     )
  => source
  -> query
  -> (Modifier -> Modifier)
  -> (el -> MaybeT m [ModifierType])
  -> m ()
modifySelectMaybeWith source q f body = do
  xs <- select q
  for_ xs \x -> maybeModifiedWith_ source x f (body x)

maybeModifySelf
  :: (Targetable a, Sourceable a, HasGame m, MonadWriter (MonoidalMap Target [Modifier]) m)
  => a
  -> MaybeT m [ModifierType]
  -> m ()
maybeModifySelf a = tell . MonoidalMap . singletonMap (toTarget a) <=< modified a . fromMaybe [] <=< runMaybeT

maybeModified :: (Sourceable a, HasGame m) => a -> MaybeT m [ModifierType] -> m [Modifier]
maybeModified a = modified a . fromMaybe [] <=< runMaybeT

modified_
  :: (Sourceable a, Targetable target, HasGame m, MonadWriter (MonoidalMap Target [Modifier]) m)
  => a
  -> target
  -> [ModifierType]
  -> m ()
modified_ _ _ [] = pure mempty
modified_ a target mods = tell . MonoidalMap . singletonMap (toTarget target) =<< toModifiers a mods

modifiedWhen_
  :: (Sourceable a, Targetable target, HasGame m, MonadWriter (MonoidalMap Target [Modifier]) m)
  => a
  -> Bool
  -> target
  -> [ModifierType]
  -> m ()
modifiedWhen_ a cond target mods =
  if cond
    then tell . MonoidalMap . singletonMap (toTarget target) =<< toModifiers a mods
    else pure mempty

modifiedWith_
  :: (Sourceable a, Targetable target, HasGame m, MonadWriter (MonoidalMap Target [Modifier]) m)
  => a
  -> target
  -> (Modifier -> Modifier)
  -> [ModifierType]
  -> m ()
modifiedWith_ a target f mods = tell . MonoidalMap . singletonMap (toTarget target) =<< toModifiersWith a f mods

maybeModified_
  :: (Sourceable a, Targetable target, HasGame m, MonadWriter (MonoidalMap Target [Modifier]) m)
  => a
  -> target
  -> MaybeT m [ModifierType]
  -> m ()
maybeModified_ a target body = do
  mods <- fromMaybe [] <$> runMaybeT body
  unless (null mods) do
    tell . MonoidalMap . singletonMap (toTarget target) =<< toModifiers a mods

maybeModifiedWith_
  :: (Sourceable a, Targetable target, HasGame m, MonadWriter (MonoidalMap Target [Modifier]) m)
  => a
  -> target
  -> (Modifier -> Modifier)
  -> MaybeT m [ModifierType]
  -> m ()
maybeModifiedWith_ a target f body = do
  mods <- fromMaybe [] <$> runMaybeT body
  unless (null mods) do
    tell . MonoidalMap . singletonMap (toTarget target) =<< toModifiersWith a f mods

toModifiersWith
  :: (HasGame m, Sourceable a) => a -> (Modifier -> Modifier) -> [ModifierType] -> m [Modifier]
toModifiersWith a f xs = traverse (fmap f . toModifier a) xs

skillTestModifier
  :: (Sourceable source, Targetable target, HasGame m)
  => SkillTestId
  -> source
  -> target
  -> ModifierType
  -> m Message
skillTestModifier sid source target modifier =
  skillTestModifiers sid source target [modifier]

skillTestModifiers
  :: (Sourceable source, Targetable target, HasGame m)
  => SkillTestId
  -> source
  -> target
  -> [ModifierType]
  -> m Message
skillTestModifiers sid (toSource -> source) (toTarget -> target) mods = do
  ems <- effectModifiers source mods
  pure $ CreateWindowModifierEffect (#skillTest sid) ems source target

nextSkillTestModifier
  :: (Sourceable source, Targetable target, HasGame m)
  => source
  -> target
  -> ModifierType
  -> m Message
nextSkillTestModifier source target modifier =
  nextSkillTestModifiers source target [modifier]

nextSkillTestModifiers
  :: (Sourceable source, Targetable target, HasGame m)
  => source
  -> target
  -> [ModifierType]
  -> m Message
nextSkillTestModifiers (toSource -> source) (toTarget -> target) mods = do
  ems <- effectModifiers source mods
  pure $ CreateWindowModifierEffect EffectNextSkillTestWindow ems source target

effectModifiers
  :: (HasGame m, Sourceable a) => a -> [ModifierType] -> m (EffectMetadata Window Message)
effectModifiers source ms = EffectModifiers <$> toModifiers source ms

createWindowModifierEffect
  :: (Sourceable source, Targetable target, HasGame m)
  => EffectWindow
  -> source
  -> target
  -> [ModifierType]
  -> m Message
createWindowModifierEffect eWindow (toSource -> source) (toTarget -> target) mods = do
  ems <- effectModifiers source mods
  pure $ CreateWindowModifierEffect eWindow ems source target

createCostModifiers
  :: (HasGame m, Sourceable source, IsCard card) => source -> card -> [ModifierType] -> m Message
createCostModifiers source (toCard -> card) modifiers' =
  createWindowModifierEffect (EffectCardCostWindow $ toCardId card) source (toCardId card) modifiers'

reduceCostOf :: (HasGame m, Sourceable source, IsCard card) => source -> card -> Int -> m Message
reduceCostOf source (toCard -> card) n = createCostModifiers source card [ReduceCostOf (CardWithId $ toCardId card) n]

turnModifier
  :: (Sourceable source, Targetable target, HasGame m)
  => InvestigatorId
  -> source
  -> target
  -> ModifierType
  -> m Message
turnModifier iid source target modifier = createWindowModifierEffect (EffectTurnWindow iid) source target [modifier]

turnModifiers
  :: (Sourceable source, Targetable target, HasGame m)
  => InvestigatorId
  -> source
  -> target
  -> [ModifierType]
  -> m Message
turnModifiers iid source target modifiers = createWindowModifierEffect (EffectTurnWindow iid) source target modifiers

nextTurnModifier
  :: (Sourceable source, Targetable target, HasGame m)
  => InvestigatorId
  -> source
  -> target
  -> ModifierType
  -> m Message
nextTurnModifier iid source target modifier = createWindowModifierEffect (EffectNextTurnWindow iid) source target [modifier]

nextTurnModifiers
  :: (Sourceable source, Targetable target, HasGame m)
  => InvestigatorId
  -> source
  -> target
  -> [ModifierType]
  -> m Message
nextTurnModifiers iid source target modifiers = createWindowModifierEffect (EffectNextTurnWindow iid) source target modifiers

createRoundModifier
  :: (Sourceable source, Targetable target, HasGame m) => source -> target -> [ModifierType] -> m Message
createRoundModifier = createWindowModifierEffect EffectRoundWindow

roundModifier
  :: (Sourceable source, Targetable target, HasGame m) => source -> target -> ModifierType -> m Message
roundModifier source target modifier = createWindowModifierEffect EffectRoundWindow source target [modifier]

roundModifiers
  :: (Sourceable source, Targetable target, HasGame m) => source -> target -> [ModifierType] -> m Message
roundModifiers = createRoundModifier

gameModifier
  :: (Sourceable source, Targetable target, HasGame m) => source -> target -> ModifierType -> m Message
gameModifier source target modifier = createWindowModifierEffect EffectGameWindow source target [modifier]

resolutionModifier
  :: (Sourceable source, Targetable target, HasGame m) => source -> target -> ModifierType -> m Message
resolutionModifier source target modifier = createWindowModifierEffect EffectResolutionWindow source target [modifier]

nextPhaseModifier
  :: (Sourceable source, Targetable target, HasGame m)
  => Phase
  -> source
  -> target
  -> ModifierType
  -> m Message
nextPhaseModifier phase source target modifier = createWindowModifierEffect (EffectPhaseWindowFor phase) source target [modifier]

nextPhaseModifiers
  :: (Sourceable source, Targetable target, HasGame m)
  => Phase
  -> source
  -> target
  -> [ModifierType]
  -> m Message
nextPhaseModifiers phase source target modifiers = createWindowModifierEffect (EffectPhaseWindowFor phase) source target modifiers

endOfPhaseModifier
  :: (Sourceable source, Targetable target, HasGame m)
  => Phase
  -> source
  -> target
  -> ModifierType
  -> m Message
endOfPhaseModifier phase source target modifier = createWindowModifierEffect (EffectUntilEndOfPhaseWindowFor phase) source target [modifier]

endOfNextPhaseModifier
  :: (Sourceable source, Targetable target, HasGame m)
  => Phase
  -> source
  -> target
  -> ModifierType
  -> m Message
endOfNextPhaseModifier phase source target modifier = createWindowModifierEffect (EffectUntilEndOfNextPhaseWindowFor phase) source target [modifier]

enemyAttackModifier
  :: (Sourceable source, Targetable target, HasGame m) => source -> target -> ModifierType -> m Message
enemyAttackModifier source target modifier = createWindowModifierEffect EffectAttackWindow source target [modifier]

enemyAttackModifiers
  :: (Sourceable source, Targetable target, HasGame m) => source -> target -> [ModifierType] -> m Message
enemyAttackModifiers source target modifiers = createWindowModifierEffect EffectAttackWindow source target modifiers

costModifier
  :: (Sourceable source, Targetable target, HasGame m) => source -> target -> ModifierType -> m Message
costModifier source target modifier = createWindowModifierEffect EffectCostWindow source target [modifier]

costModifiers
  :: (Sourceable source, Targetable target, HasGame m) => source -> target -> [ModifierType] -> m Message
costModifiers source target modifiers = createWindowModifierEffect EffectCostWindow source target modifiers

eventModifier
  :: (Sourceable source, Targetable target, HasGame m) => source -> target -> ModifierType -> m Message
eventModifier source target modifier = createWindowModifierEffect EffectEventWindow source target [modifier]

gainResourcesModifier
  :: (Sourceable source, Targetable target, HasGame m)
  => InvestigatorId
  -> source
  -> target
  -> ModifierType
  -> m Message
gainResourcesModifier iid source target modifier = createWindowModifierEffect (EffectGainResourcesWindow iid) source target [modifier]

eventModifiers
  :: (Sourceable source, Targetable target, HasGame m) => source -> target -> [ModifierType] -> m Message
eventModifiers source target modifiers = createWindowModifierEffect EffectEventWindow source target modifiers

movementModifier
  :: (Sourceable source, Targetable target, HasGame m) => source -> target -> ModifierType -> m Message
movementModifier source target modifier = createWindowModifierEffect EffectMoveWindow source target [modifier]

phaseModifier
  :: (Sourceable source, Targetable target, HasGame m) => source -> target -> ModifierType -> m Message
phaseModifier source target modifier = createWindowModifierEffect EffectPhaseWindow source target [modifier]

phaseModifiers
  :: (Sourceable source, Targetable target, HasGame m) => source -> target -> [ModifierType] -> m Message
phaseModifiers source target modifiers = createWindowModifierEffect EffectPhaseWindow source target modifiers

cardDrawModifier
  :: (Sourceable source, Targetable target, HasGame m) => source -> target -> [ModifierType] -> m Message
cardDrawModifier source target modifiers = createWindowModifierEffect EffectCardDrawWindow source target modifiers

cardResolutionModifier
  :: (Sourceable source, Targetable target, IsCard card, HasGame m)
  => card
  -> source
  -> target
  -> ModifierType
  -> m Message
cardResolutionModifier card source target modifier =
  createWindowModifierEffect (EffectCardResolutionWindow $ toCardId card) source target [modifier]

cardResolutionModifiers
  :: (Sourceable source, Targetable target, IsCard card, HasGame m)
  => card
  -> source
  -> target
  -> [ModifierType]
  -> m Message
cardResolutionModifiers card source target modifiers = createWindowModifierEffect (EffectCardResolutionWindow $ toCardId card) source target modifiers

searchModifier
  :: (Sourceable source, Targetable target, HasGame m) => source -> target -> ModifierType -> m Message
searchModifier source target modifier = createWindowModifierEffect EffectSearchWindow source target [modifier]

setupModifier
  :: (Sourceable source, Targetable target, HasGame m) => source -> target -> ModifierType -> m Message
setupModifier source target modifier = createWindowModifierEffect EffectSetupWindow source target [modifier]

scenarioSetupModifier
  :: (Sourceable source, Targetable target, HasGame m)
  => ScenarioId
  -> source
  -> target
  -> ModifierType
  -> m Message
scenarioSetupModifier scenarioId source target modifier = createWindowModifierEffect (EffectScenarioSetupWindow scenarioId) source target [modifier]

abilityModifier
  :: (Sourceable source, Targetable target, HasGame m)
  => AbilityRef
  -> source
  -> target
  -> ModifierType
  -> m Message
abilityModifier abilityRef source target modifier = createWindowModifierEffect (EffectAbilityWindow abilityRef) source target [modifier]

chaosTokenEffect
  :: (HasGame m, Sourceable source) => source -> ChaosToken -> ModifierType -> m Message
chaosTokenEffect (toSource -> source) token modifier = do
  ems <- effectModifiers source [modifier]
  pure $ CreateChaosTokenEffect ems source token

onRevealChaosTokenEffect
  :: (Sourceable source, Targetable target)
  => SkillTestId
  -> ChaosTokenMatcher
  -> source
  -> target
  -> [Message]
  -> Message
onRevealChaosTokenEffect sid matchr source target msgs = CreateOnRevealChaosTokenEffect sid matchr (toSource source) (toTarget target) msgs

uiEffect
  :: (HasGame m, Sourceable source, Targetable target) => source -> target -> ModifierType -> m Message
uiEffect source target modifier = createWindowModifierEffect EffectUI source target [modifier]

getAdditionalSearchTargets :: HasGame m => InvestigatorId -> m Int
getAdditionalSearchTargets iid = sumOf (each . _AdditionalTargets) <$> getModifiers iid

getTotalSearchTargets :: HasGame m => InvestigatorId -> [a] -> Int -> m Int
getTotalSearchTargets iid targets n = do
  additionalTargets <- getAdditionalSearchTargets iid
  pure $ min (length targets) (n + additionalTargets)

getMeta :: (HasGame m, Targetable target, FromJSON a) => target -> Key -> m (Maybe a)
getMeta target k = do
  metas <- mapMaybe (preview _MetaModifier) <$> getModifiers target
  pure $ getFirst $ flip foldMap metas $ \case
    Object o -> case fromJSON <$> KeyMap.lookup k o of
      Just (Success a) -> First (Just a)
      _ -> First Nothing
    _ -> First Nothing

getMetaMaybe :: (HasGame m, Targetable target, FromJSON a) => a -> target -> Key -> m a
getMetaMaybe def target k = do
  metas <- mapMaybe (preview _MetaModifier) <$> getModifiers target
  let
    value = getFirst $ flip foldMap metas $ \case
      Object o -> case fromJSON <$> KeyMap.lookup k o of
        Just (Success a) -> First (Just a)
        _ -> First Nothing
      _ -> First Nothing
  pure $ fromMaybe def value

revelationModifiers
  :: (Sourceable source, Targetable target, HasGame m)
  => source
  -> target
  -> TreacheryId
  -> [ModifierType]
  -> m Message
revelationModifiers (toSource -> source) (toTarget -> target) tid modifiers = do
  ems <- effectModifiers source modifiers
  pure $ CreateWindowModifierEffect (EffectRevelationWindow tid) ems source target

revelationModifier
  :: (Sourceable source, Targetable target, HasGame m)
  => source
  -> target
  -> TreacheryId
  -> ModifierType
  -> m Message
revelationModifier (toSource -> source) (toTarget -> target) tid modifier = do
  revelationModifiers source target tid [modifier]

controllerGets
  :: ( HasField "controller" source (Maybe InvestigatorId)
     , Sourceable source
     , HasGame m
     , MonadWriter (MonoidalMap Target [Modifier]) m
     )
  => source
  -> [ModifierType]
  -> m ()
controllerGets a mods = case a.controller of
  Just iid -> modified_ a iid mods
  Nothing -> pure ()

controllerGetsWhen
  :: ( HasField "controller" source (Maybe InvestigatorId)
     , Sourceable source
     , HasGame m
     , MonadWriter (MonoidalMap Target [Modifier]) m
     )
  => source
  -> Bool
  -> [ModifierType]
  -> m ()
controllerGetsWhen a cond mods = when cond $ controllerGets a mods

controllerGetsWith
  :: ( HasField "controller" source (Maybe InvestigatorId)
     , Sourceable source
     , HasGame m
     , MonadWriter (MonoidalMap Target [Modifier]) m
     )
  => source
  -> (Modifier -> Modifier)
  -> [ModifierType]
  -> m ()
controllerGetsWith a f mods = case a.controller of
  Just iid -> modifiedWith_ a iid f mods
  Nothing -> pure ()

inThreatAreaGets
  :: ( HasField "placement" source Placement
     , Sourceable source
     , HasGame m
     , MonadWriter (MonoidalMap Target [Modifier]) m
     )
  => source
  -> [ModifierType]
  -> m ()
inThreatAreaGets a mods = case a.placement of
  InThreatArea iid -> modified_ a iid mods
  _ -> pure mempty

pattern CannotMoveTo :: LocationMatcher -> ModifierType
pattern CannotMoveTo lm = AdditionalCostToEnterMatching lm UnpayableCost
