module Arkham.Helpers.Modifiers (
  module Arkham.Helpers.Modifiers,
  module X,
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.ChaosToken
import Arkham.Effect.Window
import Arkham.EffectMetadata
import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Matcher.Types
import Arkham.Message
import Arkham.Modifier as X
import Arkham.Source
import Arkham.Target
import Arkham.Window (Window)
import Control.Lens (each, sumOf)

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

getModifiers' :: (HasGame m, Targetable a) => a -> m [Modifier]
getModifiers' (toTarget -> target) =
  findWithDefault [] target <$> getAllModifiers

hasModifier
  :: (HasGame m, Targetable a) => a -> ModifierType -> m Bool
hasModifier a m = (m `elem`) <$> getModifiers (toTarget a)

withoutModifier
  :: (HasGame m, Targetable a) => a -> ModifierType -> m Bool
withoutModifier a m = not <$> hasModifier a m

toModifier :: Sourceable a => a -> ModifierType -> Modifier
toModifier a mType = Modifier (toSource a) mType False

toModifiers :: Sourceable a => a -> [ModifierType] -> [Modifier]
toModifiers = map . toModifier

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
skillTestModifiers source target modifiers =
  CreateWindowModifierEffect
    EffectSkillTestWindow
    (EffectModifiers $ toModifiers source modifiers)
    (toSource source)
    (toTarget target)

effectModifiers :: Sourceable a => a -> [ModifierType] -> EffectMetadata Window Message
effectModifiers source = EffectModifiers . toModifiers source

createWindowModifierEffect
  :: (Sourceable source, Targetable target)
  => EffectWindow
  -> source
  -> target
  -> [ModifierType]
  -> Message
createWindowModifierEffect eWindow source target modifiers' =
  CreateWindowModifierEffect
    eWindow
    (effectModifiers source modifiers')
    (toSource source)
    (toTarget target)

createCostModifiers
  :: (Sourceable source, IsCard card) => source -> card -> [ModifierType] -> Message
createCostModifiers source (toCard -> card) modifiers' =
  createWindowModifierEffect
    (EffectCardCostWindow $ toCardId card)
    source
    (toCardId card)
    modifiers'

reduceCostOf :: (Sourceable source, IsCard card) => source -> card -> Int -> Message
reduceCostOf source (toCard -> card) n = createCostModifiers source card [ReduceCostOf (CardWithId $ toCardId card) n]

turnModifier
  :: (Sourceable source, Targetable target) => source -> target -> ModifierType -> Message
turnModifier (toSource -> source) (toTarget -> target) modifier = createWindowModifierEffect EffectTurnWindow source target [modifier]

turnModifiers
  :: (Sourceable source, Targetable target) => source -> target -> [ModifierType] -> Message
turnModifiers (toSource -> source) (toTarget -> target) modifiers = createWindowModifierEffect EffectTurnWindow source target modifiers

createRoundModifier
  :: (Sourceable source, Targetable target) => source -> target -> [ModifierType] -> Message
createRoundModifier = createWindowModifierEffect EffectRoundWindow

roundModifier
  :: (Sourceable source, Targetable target) => source -> target -> ModifierType -> Message
roundModifier (toSource -> source) (toTarget -> target) modifier = createWindowModifierEffect EffectRoundWindow source target [modifier]

roundModifiers
  :: (Sourceable source, Targetable target) => source -> target -> [ModifierType] -> Message
roundModifiers = createRoundModifier

gameModifier
  :: (Sourceable source, Targetable target) => source -> target -> ModifierType -> Message
gameModifier (toSource -> source) (toTarget -> target) modifier = createWindowModifierEffect EffectGameWindow source target [modifier]

costModifier
  :: (Sourceable source, Targetable target) => source -> target -> ModifierType -> Message
costModifier (toSource -> source) (toTarget -> target) modifier = createWindowModifierEffect EffectCostWindow source target [modifier]

eventModifier
  :: (Sourceable source, Targetable target) => source -> target -> ModifierType -> Message
eventModifier (toSource -> source) (toTarget -> target) modifier = createWindowModifierEffect EffectEventWindow source target [modifier]

phaseModifier
  :: (Sourceable source, Targetable target) => source -> target -> ModifierType -> Message
phaseModifier (toSource -> source) (toTarget -> target) modifier = createWindowModifierEffect EffectPhaseWindow source target [modifier]

cardResolutionModifier
  :: (Sourceable source, Targetable target) => source -> target -> ModifierType -> Message
cardResolutionModifier (toSource -> source) (toTarget -> target) modifier = createWindowModifierEffect EffectCardResolutionWindow source target [modifier]

cardResolutionModifiers
  :: (Sourceable source, Targetable target) => source -> target -> [ModifierType] -> Message
cardResolutionModifiers (toSource -> source) (toTarget -> target) modifiers = createWindowModifierEffect EffectCardResolutionWindow source target modifiers

searchModifier
  :: (Sourceable source, Targetable target) => source -> target -> ModifierType -> Message
searchModifier (toSource -> source) (toTarget -> target) modifier = createWindowModifierEffect EffectSearchWindow source target [modifier]

setupModifier
  :: (Sourceable source, Targetable target) => source -> target -> ModifierType -> Message
setupModifier (toSource -> source) (toTarget -> target) modifier = createWindowModifierEffect EffectSetupWindow source target [modifier]

abilityModifier
  :: (Sourceable source, Targetable target) => source -> target -> ModifierType -> Message
abilityModifier (toSource -> source) (toTarget -> target) modifier = createWindowModifierEffect EffectAbilityWindow source target [modifier]

chaosTokenEffect :: Sourceable source => source -> ChaosToken -> ModifierType -> Message
chaosTokenEffect (toSource -> source) token modifier =
  CreateChaosTokenEffect (EffectModifiers $ toModifiers source [modifier]) source token

getAdditionalSearchTargets :: HasGame m => InvestigatorId -> m Int
getAdditionalSearchTargets iid = sumOf (each . _AdditionalTargets) <$> getModifiers iid

getTotalSearchTargets :: HasGame m => InvestigatorId -> [a] -> Int -> m Int
getTotalSearchTargets iid targets n = do
  additionalTargets <- getAdditionalSearchTargets iid
  pure $ min (length targets) (n + additionalTargets)
