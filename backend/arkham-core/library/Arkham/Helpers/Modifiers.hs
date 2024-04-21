module Arkham.Helpers.Modifiers (
  module Arkham.Helpers.Modifiers,
  module X,
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes.HasGame
import Arkham.Effect.Window
import Arkham.EffectMetadata
import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Matcher.Types
import Arkham.Message
import Arkham.Modifier as X
import Arkham.Phase (Phase)
import Arkham.Source
import Arkham.Target
import Arkham.Window (Window)
import Control.Lens (each, sumOf)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Monoid (First (..))

withModifiers
  :: (HasGame m, Targetable target) => target -> [Modifier] -> (forall n. HasGame n => n a) -> m a
withModifiers = withModifiers'

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
getModifiers' (toTarget -> target) =
  findWithDefault [] target <$> getAllModifiers

hasModifier
  :: (HasGame m, Targetable a) => a -> ModifierType -> m Bool
hasModifier a m = (m `elem`) <$> getModifiers (toTarget a)

withoutModifier
  :: (HasGame m, Targetable a) => a -> ModifierType -> m Bool
withoutModifier a m = not <$> hasModifier a m

withoutModifiers
  :: (HasGame m, Targetable a) => a -> [ModifierType] -> m Bool
withoutModifiers a ms = all (`notElem` ms) <$> getModifiers (toTarget a)

toModifier :: Sourceable a => a -> ModifierType -> Modifier
toModifier a mType = Modifier (toSource a) mType False

toModifiers :: Sourceable a => a -> [ModifierType] -> [Modifier]
toModifiers = map . toModifier

modified :: (Sourceable a, Applicative m) => a -> [ModifierType] -> m [Modifier]
modified a = pure . toModifiers a

toModifiersWith :: Sourceable a => a -> (Modifier -> Modifier) -> [ModifierType] -> [Modifier]
toModifiersWith a f xs = map (f . toModifier a) xs

skillTestModifier
  :: (Sourceable source, Targetable target)
  => source
  -> target
  -> ModifierType
  -> Message
skillTestModifier source target modifier =
  skillTestModifiers source target [modifier]

skillTestModifiers
  :: forall target source
   . (Sourceable source, Targetable target)
  => source
  -> target
  -> [ModifierType]
  -> Message
skillTestModifiers (toSource -> source) (toTarget -> target) mods =
  CreateWindowModifierEffect #skillTest (EffectModifiers $ toModifiers source mods) source target

effectModifiers :: Sourceable a => a -> [ModifierType] -> EffectMetadata Window Message
effectModifiers source = EffectModifiers . toModifiers source

createWindowModifierEffect
  :: (Sourceable source, Targetable target)
  => EffectWindow
  -> source
  -> target
  -> [ModifierType]
  -> Message
createWindowModifierEffect eWindow (toSource -> source) (toTarget -> target) mods =
  CreateWindowModifierEffect eWindow (effectModifiers source mods) source target

createCostModifiers
  :: (Sourceable source, IsCard card) => source -> card -> [ModifierType] -> Message
createCostModifiers source (toCard -> card) modifiers' =
  createWindowModifierEffect (EffectCardCostWindow $ toCardId card) source (toCardId card) modifiers'

reduceCostOf :: (Sourceable source, IsCard card) => source -> card -> Int -> Message
reduceCostOf source (toCard -> card) n = createCostModifiers source card [ReduceCostOf (CardWithId $ toCardId card) n]

turnModifier
  :: (Sourceable source, Targetable target) => source -> target -> ModifierType -> Message
turnModifier source target modifier = createWindowModifierEffect EffectTurnWindow source target [modifier]

turnModifiers
  :: (Sourceable source, Targetable target) => source -> target -> [ModifierType] -> Message
turnModifiers source target modifiers = createWindowModifierEffect EffectTurnWindow source target modifiers

nextTurnModifier
  :: (Sourceable source, Targetable target) => source -> target -> ModifierType -> Message
nextTurnModifier source target modifier = createWindowModifierEffect EffectNextTurnWindow source target [modifier]

nextTurnModifiers
  :: (Sourceable source, Targetable target) => source -> target -> [ModifierType] -> Message
nextTurnModifiers source target modifiers = createWindowModifierEffect EffectNextTurnWindow source target modifiers

createRoundModifier
  :: (Sourceable source, Targetable target) => source -> target -> [ModifierType] -> Message
createRoundModifier = createWindowModifierEffect EffectRoundWindow

roundModifier
  :: (Sourceable source, Targetable target) => source -> target -> ModifierType -> Message
roundModifier source target modifier = createWindowModifierEffect EffectRoundWindow source target [modifier]

roundModifiers
  :: (Sourceable source, Targetable target) => source -> target -> [ModifierType] -> Message
roundModifiers = createRoundModifier

gameModifier
  :: (Sourceable source, Targetable target) => source -> target -> ModifierType -> Message
gameModifier source target modifier = createWindowModifierEffect EffectGameWindow source target [modifier]

nextPhaseModifier
  :: (Sourceable source, Targetable target) => Phase -> source -> target -> ModifierType -> Message
nextPhaseModifier phase source target modifier = createWindowModifierEffect (EffectPhaseWindowFor phase) source target [modifier]

enemyAttackModifier
  :: (Sourceable source, Targetable target) => source -> target -> ModifierType -> Message
enemyAttackModifier source target modifier = createWindowModifierEffect EffectAttackWindow source target [modifier]

costModifier
  :: (Sourceable source, Targetable target) => source -> target -> ModifierType -> Message
costModifier source target modifier = createWindowModifierEffect EffectCostWindow source target [modifier]

eventModifier
  :: (Sourceable source, Targetable target) => source -> target -> ModifierType -> Message
eventModifier source target modifier = createWindowModifierEffect EffectEventWindow source target [modifier]

movementModifier
  :: (Sourceable source, Targetable target) => source -> target -> ModifierType -> Message
movementModifier source target modifier = createWindowModifierEffect EffectMoveWindow source target [modifier]

phaseModifier
  :: (Sourceable source, Targetable target) => source -> target -> ModifierType -> Message
phaseModifier source target modifier = createWindowModifierEffect EffectPhaseWindow source target [modifier]

phaseModifiers
  :: (Sourceable source, Targetable target) => source -> target -> [ModifierType] -> Message
phaseModifiers source target modifiers = createWindowModifierEffect EffectPhaseWindow source target modifiers

cardResolutionModifier
  :: (Sourceable source, Targetable target, IsCard card)
  => card
  -> source
  -> target
  -> ModifierType
  -> Message
cardResolutionModifier card source target modifier =
  createWindowModifierEffect (EffectCardResolutionWindow $ toCardId card) source target [modifier]

cardResolutionModifiers
  :: (Sourceable source, Targetable target, IsCard card)
  => card
  -> source
  -> target
  -> [ModifierType]
  -> Message
cardResolutionModifiers card source target modifiers = createWindowModifierEffect (EffectCardResolutionWindow $ toCardId card) source target modifiers

searchModifier
  :: (Sourceable source, Targetable target) => source -> target -> ModifierType -> Message
searchModifier source target modifier = createWindowModifierEffect EffectSearchWindow source target [modifier]

setupModifier
  :: (Sourceable source, Targetable target) => source -> target -> ModifierType -> Message
setupModifier source target modifier = createWindowModifierEffect EffectSetupWindow source target [modifier]

abilityModifier
  :: (Sourceable source, Targetable target) => source -> target -> ModifierType -> Message
abilityModifier source target modifier = createWindowModifierEffect EffectAbilityWindow source target [modifier]

chaosTokenEffect :: Sourceable source => source -> ChaosToken -> ModifierType -> Message
chaosTokenEffect (toSource -> source) token modifier =
  CreateChaosTokenEffect (EffectModifiers $ toModifiers source [modifier]) source token

uiEffect :: (Sourceable source, Targetable target) => source -> target -> ModifierType -> Message
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
