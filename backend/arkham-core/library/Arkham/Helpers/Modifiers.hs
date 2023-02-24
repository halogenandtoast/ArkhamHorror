module Arkham.Helpers.Modifiers
  ( module Arkham.Helpers.Modifiers
  , module X
  ) where

import Arkham.Prelude

import Arkham.Classes.Entity
import Arkham.Effect.Window
import Arkham.EffectMetadata
import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Message
import Arkham.Modifier as X
import Arkham.Target

getModifiers :: HasGame m => Target -> m [ModifierType]
getModifiers target = map modifierType <$> getModifiers' target

getModifiers' :: HasGame m => Target -> m [Modifier]
getModifiers' target =
  findWithDefault [] target <$> getAllModifiers

hasModifier
  :: (HasGame m, Targetable a) => a -> ModifierType -> m Bool
hasModifier a m = (m `elem`) <$> getModifiers (toTarget a)

withoutModifier
  :: (HasGame m, Targetable a) => a -> ModifierType -> m Bool
withoutModifier a m = not <$> hasModifier a m

toModifier :: SourceEntity a => a -> ModifierType -> Modifier
toModifier a mType = Modifier (toSource a) mType False

toModifiers :: SourceEntity a => a -> [ModifierType] -> [Modifier]
toModifiers = map . toModifier

skillTestModifier
  :: (SourceEntity source, Targetable target)
  => source
  -> target
  -> ModifierType
  -> Message
skillTestModifier source target modifier =
  skillTestModifiers source target [modifier]

skillTestModifiers
  :: (SourceEntity source, Targetable target)
  => source
  -> target
  -> [ModifierType]
  -> Message
skillTestModifiers source target modifiers = CreateWindowModifierEffect
  EffectSkillTestWindow
  (EffectModifiers $ toModifiers source modifiers)
  (toSource source)
  (toTarget target)
