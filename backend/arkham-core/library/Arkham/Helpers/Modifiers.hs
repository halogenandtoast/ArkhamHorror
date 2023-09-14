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
import Arkham.Matcher.Types
import Arkham.Message
import Arkham.Modifier as X
import Arkham.Source
import Arkham.Target
import Arkham.Window (Window)

getModifiers :: (HasGame m, Targetable a) => a -> m [ModifierType]
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
  :: (Sourceable source, Targetable target)
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

phaseModifier
  :: (Sourceable source, Targetable target) => source -> target -> ModifierType -> Message
phaseModifier (toSource -> source) (toTarget -> target) modifier = createWindowModifierEffect EffectPhaseWindow source target [modifier]

chaosTokenEffect :: Sourceable source => source -> ChaosToken -> ModifierType -> Message
chaosTokenEffect (toSource -> source) token modifier =
  CreateChaosTokenEffect (EffectModifiers $ toModifiers source [modifier]) source token
